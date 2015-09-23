{-# LANGUAGE OverloadedStrings, TemplateHaskell, ImplicitParams #-}
module Rede.Research.ResearchWorker(
    runResearchWorker
    ,spawnHarServer
    ) where


import           Control.Lens                    (
                                                 (^.)
                                                 -- , (%~)
                                                 , (.~)
                                                 )
import qualified Control.Lens                    as L
import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.MVar
import qualified Control.Concurrent.STM.TBChan  as T
import           Control.Monad.Catch             (catch,throwM)
import           Control.Monad.Trans.Reader      (ask)
import qualified Control.Exception               as E
import           Control.Concurrent.STM

import qualified Data.ByteString                 as B
import qualified Data.ByteString.Builder         as Bu
import           Data.ByteString.Char8           (pack, unpack)
import qualified Data.ByteString.Lazy.Char8      as Lch
import qualified Data.ByteString.Lazy            as LB
import qualified Data.HashTable.IO               as H
import           Data.Conduit
import           Data.Foldable                   (foldMap)
import           Data.Monoid                     (mappend)
import qualified Data.Monoid                     as M
import qualified Data.Aeson                      as Da(decode, encode)
import           Data.Maybe                      (isJust)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader      (ReaderT (..), runReaderT)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error                 (isDoesNotExistError)
import           System.Process

import qualified Network.URI                     as U

import           System.Log.Logger
import           Text.Printf                     (printf)

import           Rede.Workers.ResponseFragments  (RequestMethod (..),
                                                  getMethodFromHeaders,
                                                  getUrlFromHeaders,
                                                  simpleResponse)

import           Rede.HarFiles.DnsMasq           (dnsMasqFileContentsToIp)
import           Rede.HarFiles.ServedEntry       (BadHarFile (..),
                                                  ResolveCenter, allSeenHosts,
                                                  rcName, resolveCenterFromLazyByteString
                                                  )

import           SecondTransfer.Http2            (http2Attendant)
import           SecondTransfer.Exception        (StreamCancelledException)
import           SecondTransfer.Sessions         (makeDefaultSessionsContext)
import           SecondTransfer                  (
                                                  tlsServeWithALPNAndFinishOnRequest,
                                                  dropIncomingData,
                                                  PrincipalStream,
                                                  InputDataStream,
                                                  CoherentWorker,
                                                  Request,
                                                  FinishRequest (..)
                                                 )
import          SecondTransfer.Types


-- import           Rede.MainLoop.CoherentWorker
import           Rede.MainLoop.ConfigHelp        (configDir, getInterfaceName,
                                                  getMimicPort)

import           Rede.Utils.ConcatConduit        (concatConduit)
import           Rede.Utils.PrintfArgByteString  ()
import           Rede.Utils.Alarm
import           Rede.Workers.HarWorker          (harCoherentWorker)

import           Rede.Research.JobId             (HashId(..), jobIdFromUrl, hashidFromJobId)
import           Rede.Research.JSONMessages      (SetNextUrl(..), DnsMasqConfig(..), WorkIndication(..))


import           Debug.Trace (trace)


type HashTable k v = H.CuckooHashTable k v


data CurrentAnalysisStage =
    |WaitingForActivateStationA_CAS
    |WaitingForBrowserReadyStationA_CAS
    |WaitingForHarvestStationToQueryNextUrl_CAS
    |WaitingForHarvestStationToDeliverHAR_CAS
    |WaitingForKillingStationA_CAS
    |WaitingForDNSMaskToAskConfig_CAS
    |WaitingForDNSMaskToConfirm_CAS
    |WaitingForActivateStationB_CAS
    |WaitingForBrowserReadyStationB_CAS
    |WaitingForTestStationToQueryNextUrl_CAS
    |WaitingForTestStationToDeliverHAR_CAS
    |WaitingForKillingStationB_CAS
    deriving (Eq, Show, Bounded, Enum, Ord)


data ReportedAnalysisStage =
    Queued_RAS
    | Processing_RAS CurrentAnalysisStage
    | Failed_RAS
    | Done_RAS
    deriving (Eq, Show)


-- TODO: Check that we really don't need the hashid at this stage...
data ReadinessPing = ReadinessPing


-- Time to wait before unleashing an alarm for a failed step
timeForAlarm :: Int
timeForAlarm = 30000000


-- Time to wait before unleashing an alarm for a failed sequence
timeForGeneralFail :: Int
timeForGeneralFail = 125000000


-- For how long the resource-capturing spree should extend, in seconds
-- and fractions
presetCaptureTime :: Float
presetCaptureTime = 10.0


maxInQueue :: Int
maxInQueue = 5


-- This state structure follows a given URL analysis.
-- You can find a dictionary of these in the structure below...
data UrlState = UrlState {
    _urlHashId                   :: HashId
    ,_jobOriginalUrl             :: B.ByteString
    ,_currentAnalysisStage       :: CurrentAnalysisStage
    }


L.makeLenses ''UrlState


data ReadyToGo =
     Ready_RTG Int
    |Processing_RTG Int


-- The id and the url
data JobDescr = JobDescr HashId B.ByteString


data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl              :: TMVar B.ByteString

    ,_nextJobDescr               :: T.TBChan JobDescr

    -- Busy: we will wait on this before doing anything
    ,_readyToGo                  :: TMVar ReadyToGo

    -- To be passed on to StationB
    ,_nextTestUrl                :: TMVar HashId

    -- To be passed from the har files receiver (from StationA)
    -- to the DNSMasq checker...Contains urls
    , _nextTestUrlToCheck        :: TMVar HashId

    -- Files to be handed out to DNSMasq
    , _nextDNSMasqFile           :: TMVar DnsMasqConfig

    , _resolveCenterChan          :: TMVar ResolveCenter

    , _finishRequestChan         :: TMVar FinishRequest

    -- Order sent to the browser resetter of anyquilating the current
    -- Chrome instance. We write to this one as soon as we want to kill
    -- the browser, and then read from it when we are ready to go to the
    -- next step.
    , _startTesterBrowserChan    :: TMVar HashId

    -- The "Research dir" is the "hars" subdirectory of the mimic
    -- scratch directory.
    , _researchDir               :: FilePath

    -- The IP address that StationB needs to connect
    , _useAddressForStationB     :: B.ByteString

    -- The state of the currently processing URL
    , _urlState                  :: TMVar UrlState

    }


L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO

type ServiceHandler =
    (
      ?input_headers::Headers,
      ?maybe_source::Maybe InputDataStream,
      ?url_state::UrlState
    ) => ServiceStateMonad TupledPrincipalStream


runResearchWorker ::
    TMVar ResolveCenter
    -> TMVar FinishRequest
    -> FilePath                -- Research dir
    -> B.ByteString
    -> IO CoherentWorker
runResearchWorker resolve_center_chan finish_request_chan research_dir use_address_for_station_b = do

    liftIO $ debugM "ResearchWorker" "(on research worker)"

    -- Only queue
    next_harvest_url_chan         <- atomically $ newEmptyTMVar

    -- All the other jobs simply go on
    next_test_url_chan            <- atomically $ newEmptyTMVar -- <-- Goes from the dnsmasq response handler
    next_test_url_to_check_chan   <- atomically $ newEmptyTMVar -- <-- Goes to the dnsmasq response handler
    next_dns_masq_file            <- atomically $ newEmptyTMVar

    start_harvester_browser       <- atomically $ newEmptyTMVar
    kill_harvester_browser        <- atomically $ newEmptyTMVar

    start_tester_browser          <- atomically $ newEmptyTMVar
    kill_tester_browser           <- atomically $ newEmptyTMVar

    harvester_ready               <- atomically $ newEmptyTMVar
    tester_ready                  <- atomically $ newEmptyTMVar
    ready_to_go                   <- atomically $ newTMVar (Ready_RTG 0)
    next_job_descr                <- atomically $ T.newTBChan maxInQueue

    new_url_state  <- H.new

    let
        state = ServiceState {
             _nextHarvestUrl              = next_harvest_url_chan
            ,_nextJobDescr                = next_job_descr
            ,_readyToGo                   = ready_to_go
            ,_nextTestUrl                 = next_test_url_chan
            ,_nextTestUrlToCheck          = next_test_url_to_check_chan
            ,_nextDNSMasqFile             = next_dns_masq_file
            ,_resolveCenterChan           = resolve_center_chan
            ,_finishRequestChan           = finish_request_chan
            ,_researchDir                 = research_dir
            ,_useAddressForStationB       = use_address_for_station_b
            ,_urlState                    = new_url_state

            ,_startHarvesterBrowserChan   = start_harvester_browser
            ,_killHarvesterBrowserChan    = kill_harvester_browser
            ,_startTesterBrowserChan      = start_tester_browser
            ,_killTesterBrowserChan       = kill_tester_browser

            ,_testerReadyChan             = tester_ready
            ,_harvesterReadyChan          = harvester_ready
            }

    -- Let's create the job-pusher thread
    forkIO $ runReaderT startNextJobThread state

    -- And return the worker
    return $ \request -> runReaderT (researchWorkerComp request) state


researchWorkerComp :: (Headers, Maybe InputDataStream) -> ServiceStateMonad TupledPrincipalStream
researchWorkerComp (input_headers, maybe_source) = do
    url_state_tmvar               <- L.view urlState

    -- The entire thing, a few functions need it.
    comp_state                    <- ask

    url_state_maybe               <- liftIO . atomically $ tryReadTMVar url_state_tmvar

    case url_state_maybe of

        Just url_state ->
            handleWithUrlStateCases input_headers maybe_source

        Nothing ->
            _handleWithoutUrlStateCases input_headers


handleWithoutUrlStateCases :: (Headers, Maybe InputDataStream) -> ServiceStateMonad TupledPrincipalStream
handleWithoutUrlStateCases (headers, maybe_source) =  do
    next_harvest_url              <- L.view nextHarvestUrl
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    next_test_url_chan            <- L.view nextTestUrl
    next_test_url_to_check_chan   <- L.view nextTestUrlToCheck
    resolve_center_chan           <- L.view resolveCenterChan
    finish_chan                   <- L.view finishRequestChan
    base_research_dir             <- L.view researchDir
    use_address_for_station_b     <- L.view useAddressForStationB
    -- url_state_hashtable           <- L.view urlState

    start_harvester_browser       <- L.view startHarvesterBrowserChan
    kill_harvester_browser        <- L.view killHarvesterBrowserChan
    start_tester_browser          <- L.view startTesterBrowserChan
    kill_tester_browser           <- L.view killTesterBrowserChan
    tester_ready                  <- L.view testerReadyChan
    harvester_ready               <- L.view harvesterReadyChan
    ready_to_go                   <- L.view readyToGo
    next_job_descr                <- L.view nextJobDescr

    -- The entire thing, a few functions need it.
    comp_state                    <- ask


    let
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers
        reject_request :: ServiceStateMonad TupledPrincipalStream
        reject_request = return $ simpleResponse 500 "bad-stage"

    case method of

        Post_RM
              | req_url == "/setnexturl/", Just source <- maybe_source -> do
                -- Receives a url to investigate from the user front-end, or from curl,
                -- and drops it in a queue

                url_or_json_to_analyze <- liftIO $ source $$ concatConduit
                let
                    url_to_analyze = case Da.decode . LB.fromStrict $ url_or_json_to_analyze of
                        Just (SetNextUrl x)  -> x
                        Nothing              -> url_or_json_to_analyze

                -- Ojivas are launched at this point, so let it be IO.
                -- The only side effect is that the system clock is read,
                -- so this operation is harmless in the sense that it doesn't
                -- create any kind of filesystem entries or something like that.
                url_job_id <- liftIO $ jobIdFromUrl url_to_analyze
                let hashid = hashidFromJobId url_job_id

                -- Are we allowed to queue this task? That depends on having enough space
                -- in the queue....
                could_write <- liftIO . atomically $
                                    T.tryWriteTBChan next_job_descr $ JobDescr hashid url_to_analyze

                if could_write
                  then do
                    liftIO $ infoM "ResearchWorker" "After /setnexturl/ queueing"
                    liftIO $ markAnalysisStage hashid AnalysisRequested_CAS base_research_dir
                    return $ simpleResponse 200 (unHashId hashid)
                  else
                    return $ simpleResponse 505 "QueueFull"


handleWithUrlStateCases :: (Headers, Maybe InputDataStream) -> UrlState -> ServiceStateMonad TupledPrincipalStream
handleWithUrlStateCases  (input_headers, maybe_source) url_state = do
    next_harvest_url              <- L.view nextHarvestUrl
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    next_test_url_chan            <- L.view nextTestUrl
    next_test_url_to_check_chan   <- L.view nextTestUrlToCheck
    resolve_center_chan           <- L.view resolveCenterChan
    finish_chan                   <- L.view finishRequestChan
    base_research_dir             <- L.view researchDir
    use_address_for_station_b     <- L.view useAddressForStationB
    -- url_state_hashtable           <- L.view urlState

    start_harvester_browser       <- L.view startHarvesterBrowserChan
    kill_harvester_browser        <- L.view killHarvesterBrowserChan
    start_tester_browser          <- L.view startTesterBrowserChan
    kill_tester_browser           <- L.view killTesterBrowserChan
    tester_ready                  <- L.view testerReadyChan
    harvester_ready               <- L.view harvesterReadyChan
    ready_to_go                   <- L.view readyToGo
    next_job_descr                <- L.view nextJobDescr

    -- The entire thing, a few functions need it.
    comp_state                    <- ask


    let
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers
        reject_request :: ServiceStateMonad TupledPrincipalStream
        reject_request = return $ simpleResponse 500 "bad-stage"

    let
        ?analysis_stage = analysis_stage
        ?input_headers = input_headers
        ?maybe_source = maybe_source
        ?url_state = url_state
      in case method of

        -- Most requests are served by POST to emphasize that a request changes
        -- the state of this program...
        Post_RM
            | req_url == "/startbrowser/StationA" && (correctStage WaitingForActivateStationA_CAS)  ->
                unQueueStartBrowser "/startbrowser/StationA" start_harvester_browser

            | req_url == "/browserready/StationA" && (correctStage WaitingForBrowserReadyStationA_CAS) -> do
                handle_browserready_Station_H harvesterReadyChan

            | req_url == "/nexturl/"  && (correctStage WaitingForHarvestStationToQueryNextUrl_CAS)  ->
                handle_nexturl_H

            | req_url == "/har/" && (correctStage WaitingForHarvestStationToDeliverHAR_CAS),  Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and
                -- makes all the arrangements for it to be tested using HTTP 2
                handle_harvesterhar_H source

            | req_url == "/killbrowser/StationA" && (correctStage WaitingForKillingSationA_CAS) -> do
                unQueueKillBrowser "/killbrowser/StationA" kill_harvester_browser

            | req_url == "/dnsmasq/" && (correctStage WaitingForDNSMaskToAskConfig_CAS) -> do
                handle_dnsmasq_H maybe_source

            | req_url == "/dnsmasqupdated/" && (correctStage WaitingForDNSMaskToConfirm_CAS) , Just source <- maybe_source -> do
                handle_dnsmasqupdated_H source

            | req_url == "/startbrowser/StationB" && (correctStage WaitingForActivateStationB_CAS) -> do
                unQueueStartBrowser "/startbrowser/StationB" start_tester_browser

            | req_url == "/browserready/StationB" && (correctStage WaitingForBrowserReadyStationB_CAS) -> do
                handle_browserready_Station_H testerReadyChan

            | req_url == "/testurl/" && (correctState WaitingForTestStationToQueryNextUrl_CAS) -> do
                -- Sends the next test url to the Chrome extension, this starts processing by
                -- StationB... the request is started by StationB and we send the url in the response...
                handle_testurl_H

            | req_url == "/http2har/" && (correctStage WaitingForTestStationToDeliverHAR_CAS), Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and
                -- makes all the arrangements for it to be tested using HTTP 2
                handle_testerhar_H source

            | req_url == "/killbrowser/StationB" && (correctStage WaitingForKillingStationB_CAS) -> do
                unQueueKillBrowser "/killbrowser/StationB" kill_tester_browser


            | otherwise     ->
                reject_request

        _ ->
            reject_request


  where


    on_url_wait_interrupted :: StreamCancelledException -> ServiceStateMonad a
    on_url_wait_interrupted e = do
        liftIO $ warningM "ResearchWorker" "Post wait interrupted"
        throwM e



handle_nexturl_H :: ServiceHandler
handle_nexturl_H = do
    harvester_ready               <- L.view harvesterReadyChan
    next_harvest_url              <- L.view nextHarvestUrl
    base_research_dir             <- L.view researchDir

    let
        hashid = ?url_state ^. urlHashId

    -- Starts harvesting of a resource... this request is made by StationA.
    --
    maybe_url <- liftIO . atomically $ do
        harvester_ready_maybe <- tryTakeTMVar harvester_ready
        nexturl_maybe <- tryTakeTMVar next_harvest_url
        case (harvester_ready_maybe, nexturl_maybe) of
          (Just _, Just nexturl_maybe) -> return $ Just nexturl_maybe

    case maybe_url of
        Nothing ->
            return $ simpleResponse 500 "bad-stage"

        Just url -> do
            markAnalysisStageAndAdvance
            let msg = WorkIndication hashid url presetCaptureTime
            return $ simpleResponse 200 $ LB.toStrict $ Da.encode msg


handle_browserready_Station_H :: _ -> ServiceHandler
handle_browserready_Station_H tmvar_getter = do
    harvester_ready <- L.view tmvar_getter
    liftIO . atomically $ putTMVar harvester_ready ReadinessPing
    markAnalysisStageAndAdvance
    return $ simpleResponse 200 "Ok"


handle_testurl_H :: ServiceHandler
handle_testurl_H = do
    liftIO $ infoM "ResearchWorker" ".. /testurl/ asked"
    let
        hashid       = ?url_state ^. urlHashId
        anyschema_url = ?url_state ^. jobOriginalUrl

    let https_url = urlToHTTPSScheme anyschema_url

    liftIO . infoM "ResearchWorker" . builderToString $ "..  /testurl/ answered with " `mappend` (Bu.byteString https_url)

    let msg = WorkIndication hashid https_url presetCaptureTime
    return $ simpleResponse 200 $ LB.toStrict $ Da.encode msg


outputComputation :: InputDataStream -> IO (ResolveCenter, LB.ByteString)
outputComputation source =  do
    full_builder <- source $$ consumer ""
    let
        lb = Bu.toLazyByteString full_builder
        rc = resolveCenterF
  where
     consumer  b = do
         maybe_bytes <- await
         case maybe_bytes of

             Just bytes -> do
                 -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                 consumer $ b `M.mappend` (Bu.byteString bytes)

             Nothing -> do
                 -- liftIO $ putStrLn "Finishing"
                 return b


handle_harvesterhar_H :: _ -> ServiceHandler
handle_harvesterhar_H source = do
    base_research_dir             <- L.view researchDir
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    kill_harvester_browser        <- L.view killHarvesterBrowserChan
    resolve_center_chan           <- L.view resolveCenterChan

    let
        hashid  = ?url_state ^. urlHashId

    liftIO $ infoM "ResearchWorker" "..  /har/"
    catch
        (do
            (resolve_center, har_file_contents) <- liftIO $ outputComputation source
            liftIO . infoM "ResearchWorker" $ "Seen the following hosts: " ++ (show . L.view  allSeenHosts $ resolve_center)
            let
                -- test_url = resolve_center ^. rcOriginalUrl
                use_text = "Response processed"

            -- Create the contents to go in the dnsmasq file, and queue them
            let
                all_seen_hosts = resolve_center ^. allSeenHosts
                dnsmasq_contents = dnsMasqFileContentsToIp use_address_for_station_b all_seen_hosts

            liftIO $ do
                infoM "ResearchWorker" "BeforeHARBlock"
                atomically $ do
                    putTMVar next_dnsmasq_chan $! DnsMasqConfig hashid dnsmasq_contents
                    -- And the resolve center to be used by the serving side
                    putTMVar resolve_center_chan resolve_center
                -- We can also use this opportunity to create a folder for this research
                -- project
                let
                    scratch_folder = (base_research_dir) </> (unpack . unHashId $ hashid)
                    har_filename = scratch_folder </> "harvested.har"
                createDirectoryIfMissing True scratch_folder
                LB.writeFile har_filename har_file_contents

            markAnalysisStageAndAdvance

            return $ simpleResponse 200 use_text
        )
        onBadHarFile


handle_dnsmasq_H :: _ -> ServiceHandler
handle_dnsmasq_H maybe_source =
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    catch
        (do
            dnsmasq_contents <- liftIO $ do
                dropIncomingData maybe_source
                -- Sends a DNSMASQ file to the test station ("StationB")
                infoM "ResearchWorker" ".. /dnsmasq/ asked"
                -- Serve the DNS masq file corresponding to the last .har file
                -- received.
                dnsmasq_contents <- atomically $ takeTMVar next_dnsmasq_chan

                infoM "ResearchWorker" ".. /dnsmasq/ answers"
                -- infoM "ResearchWorker" $ " /dnsmasq/ sending " ++ (show dnsmasq_contents)

                return dnsmasq_contents

            markAnalysisStageAndAdvance

            return $ simpleResponse 200 $ LB.toStrict $ Da.encode dnsmasq_contents
        )
        onGeneralError


handle_dnsmasqupdated_H :: _ -> ServiceHandler
handle_dnsmasqupdated_H source  =
    -- Received  advice that everything is ready to proceed
    liftIO $ do
        received_id <- waitRequestBody source
        -- Ensure any in-system changes get enough time to propagate
        threadDelay 2000000
        markAnalysisStageAndAdvance
        infoM "ResearchWorker" $ "When receiving dnsmasqupdated, proceed = " ++ (show proceed)
    return $ simpleResponse 200 $ "ok"


handle_testerhar_H :: _ -> ServiceHandler
handle_testerhar_H source = do
    base_research_dir             <- L.view researchDir

    let
        hashid  = ?url_state ^. urlHashId

    liftIO $ infoM "ResearchWorker" ".. /http2har/"
    catch
        (do
            (resolve_center,  har_contents_lb) <- liftIO $ outputComputation source
            let
                use_text = "Response processed"
                hashid = resolve_center ^. rcName
            let
                scratch_folder = base_research_dir </> (unpack . unHashId $ hashid)
                har_filename = scratch_folder </> "test_http2.har"
            liftIO $ LB.writeFile har_filename har_contents_lb

            -- And finish the business
            liftIO . atomically $ do
                -- Ends the mimic serving of resources
                putTMVar finish_chan FinishRequest
                putTMVar kill_tester_browser hashid
            -- liftIO $ threadDelay 10000000 -- <-- Remove this!!

            -- Mark the state
            liftIO $ markReportedAnalysisStage hashid Done_RAS base_research_dir
            liftIO $ infoM "ResearchWorker" "Just after setting status to \"done\""

            return $ simpleResponse 200 use_text
        )
        onBadHarFile


onBadHarFile :: BadHarFile -> ServiceStateMonad TupledPrincipalStream
onBadHarFile  (BadHarFile _) =
    return $ simpleResponse 500 "BadHarFile"


onGeneralError   :: E.SomeException -> ServiceStateMonad TupledPrincipalStream
onGeneralError e = do
    liftIO $ errorM "ResearchWorker" (show e)
    throwM e


sayIfFull :: MonadIO m => String -> TMVar b -> m ()
sayIfFull msg tmvar = do
    r <- liftIO . atomically . tryTakeTMVar $ tmvar
    liftIO $
        if isJust r
            then putStrLn $ msg ++ (" is full")
            else putStrLn $ msg ++ (" is empty")


-- External thread in charge of pushing jobs
-- (Also register the new hashid to url mapping...). It reads from
-- the nextJobDescr field of the ServiceStateMonad
startNextJobThread :: ServiceStateMonad ()
startNextJobThread = do
    next_harvest_url              <- L.view nextHarvestUrl
    -- next_dnsmasq_chan             <- L.view nextDNSMasqFile
    -- next_test_url_chan            <- L.view nextTestUrl
    next_test_url_to_check_chan   <- L.view nextTestUrlToCheck
    -- resolve_center_chan           <- L.view resolveCenterChan
    -- finish_chan                   <- L.view finishRequestChan
    -- base_research_dir             <- L.view researchDir
    -- use_address_for_station_b     <- L.view useAddressForStationB
    url_state_hashtable           <- L.view urlState

    start_harvester_browser       <- L.view startHarvesterBrowserChan
    -- kill_harvester_browser        <- L.view killHarvesterBrowserChan
    -- start_tester_browser          <- L.view startTesterBrowserChan
    -- kill_tester_browser           <- L.view killTesterBrowserChan
    -- tester_ready                  <- L.view testerReadyChan
    -- harvester_ready               <- L.view harvesterReadyChan
    ready_to_go                   <- L.view readyToGo
    next_job_descr                <- L.view nextJobDescr

    -- The entire thing, a few functions need it.
    comp_state <- ask

    -- This op will be retried until we can get a ready on the
    -- rtg_state
    (jobseq_id, hashid, url_to_analyze) <- liftIO $ do
        infoM "ResearchWorker" "Before startNextJobThread block"
        data_out <- atomically $ do

            -- Debug code ....
            ready_to_go_is <- isEmptyTMVar ready_to_go
            start_harvester_browser_is <- isEmptyTMVar start_harvester_browser
            next_test_url_to_check_chan_is <- isEmptyTMVar next_test_url_to_check_chan
            next_harvest_url_is <- isEmptyTMVar next_harvest_url
            next_job_descr_is <-T.isEmptyTBChan next_job_descr
            let
                c = trace (
                    printf " *** %d %d %d %d %d"
                        (fromEnum ready_to_go_is)
                        (fromEnum start_harvester_browser_is)
                        (fromEnum next_test_url_to_check_chan_is)
                        (fromEnum next_harvest_url_is)
                        (fromEnum next_job_descr_is)
                    )
                d = trace "rtg ready "
                e = trace "rtg busy"
            -- End debug code

            rtg_state <- c $ takeTMVar ready_to_go
            JobDescr hashid url <- T.readTBChan next_job_descr
            case rtg_state of
                Ready_RTG n -> d $ do
                    putTMVar ready_to_go $ Processing_RTG n
                    putTMVar start_harvester_browser hashid
                    putTMVar next_test_url_to_check_chan  hashid
                    putTMVar next_harvest_url hashid
                    return (n,hashid,url)
                _ -> e $ retry
        infoM "ResearchWorker" "After startNextJobThread block"
        return data_out

    -- Just complain
    alarm <- liftIO $ newAlarm 65000000 $ do
        errorM "ResearchWorker" . builderToString $
            "Job for url " `mappend` (Bu.byteString url_to_analyze) `mappend` " still waiting... "

    let
        new_url_state = UrlState hashid AnalysisRequested_CAS url_to_analyze alarm

    liftIO $ do
        H.insert
            url_state_hashtable hashid new_url_state
        infoM "ResearchWorker" . builderToString $
            ".. <<processing new job>> asked " `mappend` (Bu.byteString url_to_analyze)

    -- The system may fail to complete the whole task, and then

    -- we will be stuck and blocked... what to do in this situation?
    -- Well, in that case have all the pipes emptied and start again...
    liftIO $ newAlarm timeForGeneralFail $ do
        failed <- atomically $ do
            rtg_state <- readTMVar ready_to_go
            case rtg_state of
                Processing_RTG nn | nn == jobseq_id -> return True
                                  | otherwise       -> return False
                _                                   -> return False
        if failed
          then do
            errorM "ResearchWorker" " .. General fail triggered "
            waitAndResetState comp_state
          else do
            infoM "ResearchWorker" " .. (harmless alarm went off).. "
            return ()

    startNextJobThread




unQueueStartBrowser :: B.ByteString -> TMVar HashId -> ServiceHandler
unQueueStartBrowser log_url chan_to_read = do
    let
      hashid = ?url_state ^. urlHashId

    bs_hashid <- liftIO $ do
        infoM "ResearchWorker" (unpack $ B.append "start browser asked .. " log_url)
        hashid <- atomically $ takeTMVar chan_to_read
        let bs_hashid = unHashId hashid
        infoM "ResearchWorker" (unpack $ B.concat ["start browser delivered .. ", log_url, " ", bs_hashid ])
        return bs_hashid

    markAnalysisStageAndAdvance

    return $ simpleResponse 200 $ B.append "hashid=" bs_hashid


unQueueKillBrowser :: B.ByteString -> TMVar HashId -> ServiceHandler
unQueueKillBrowser log_url chan_to_read = do
    let
      hashid = ?url_state ^. urlHashId
    liftIO $ do
        -- Let's wait a second before killing the process....
        infoM "ResearchWorker" $ unpack $ B.append log_url " cleared for killing"
        return hashid

    markAnalysisStageAndAdvance

    return $ simpleResponse 200 $ B.append "hashid=" (unHashId hashid)


-- And here for when we need to fork a server to load the .har file from
spawnHarServer :: FilePath -> TMVar ResolveCenter -> TMVar  FinishRequest -> IO ()
spawnHarServer mimic_dir resolve_center_chan finish_request_chan = do
    let
        mimic_config_dir = configDir mimic_dir
    port  <-  getMimicPort
    infoM "ResearchWorker.SpawnHarServer"  $ ".. Mimic port: " ++ (show port)
    iface <-  getInterfaceName mimic_config_dir
    infoM "ResearchWorker.SpawnHarServer" $ ".. Mimic using interface: " ++ (show iface)
    sessions_context <- makeDefaultSessionsContext

    let
        serveWork = do
            finish_request_mvar <- newEmptyMVar

            let
                watcher = do
                    r <- atomically $ takeTMVar finish_request_chan
                    infoM "ResearchWorker.SpawnHarServer" $ " .. Finishing mimic service  "
                    putMVar finish_request_mvar r

            forkIO watcher

            infoM "ResearchWorker.SpawnHarServer" "Waiting for next assignment"
            resolve_center <- atomically $ takeTMVar resolve_center_chan

            infoM "ResearchWorker.SpawnHarServer" $ printf ".. START for resolve center %s" (show $ resolve_center ^. rcName)

            let
                har_filename = unpack . unHashId $ resolve_center ^. rcName
                priv_key_filename = privKeyFilename mimic_dir har_filename
                cert_filename  = certificateFilename mimic_dir har_filename
                host_list = resolve_center ^. allSeenHosts

            infoM "ResearchWorker.SpawnHarServer" $ ".. .. about to create comprhensive certificate for " ++ har_filename
            getComprehensiveCertificate mimic_dir har_filename host_list

            infoM "ResearchWorker.SpawnHarServer" $ ".. .. .. Chosen cert. at file: " ++ (show cert_filename)
            infoM "ResearchWorker.SpawnHarServer" $ ".. .. .. ..  with private key: " ++ (show priv_key_filename)
            infoM "ResearchWorker.SpawnHarServer" $ ".. Starting mimic server"

            let
                http2worker = coherentToAwareWorker $ harCoherentWorker resolve_center
            tlsServeWithALPNAndFinishOnRequest  cert_filename priv_key_filename iface [
                 ("h2-14", http2Attendant sessions_context http2worker)
                ] port finish_request_mvar
            --
            infoM "ResearchWorker.SpawnHarServer" $ printf ".. FINISH for resolve center %s" (show $ resolve_center ^. rcName)
            serveWork

    serveWork


certificateFilename :: FilePath -> FilePath -> FilePath
certificateFilename mimic_dir harfilename = let
    basename = harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in
    namesprefix </> "server.pem"


privKeyFilename :: FilePath -> FilePath -> FilePath
privKeyFilename mimic_dir harfilename = let
    basename = harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in
    namesprefix </> "privkey.pem"


getComprehensiveCertificate :: FilePath -> FilePath -> [B.ByteString] -> IO ()
getComprehensiveCertificate mimic_dir harfilename all_seen_hosts = do

    let
        basename = harfilename
        mimic_config_dir = mimic_dir </> "config"
        namesprefix = mimic_dir </> "fakecerts" </> basename

    createDirectoryIfMissing True namesprefix
    let
        ca_location       = mimic_config_dir </> "ca"
        cnf_filename      = ca_location      </> "openssl-noprompt.cnf"
        db_filename       = ca_location      </> "certindex.txt"
        ca_cert           = ca_location      </> "cacert.pem"
        csr_filename      = namesprefix      </> "cert.csr"
        template_cnf      = namesprefix      </> "openssl.conf"
        priv_key_filename = privKeyFilename  mimic_dir harfilename
        cert_filename     = namesprefix      </> "server.pem"

    -- Create a food file
    templatefile <- B.readFile cnf_filename

    let
        builder0 = Bu.byteString templatefile
        builderM1 = Bu.byteString $ "dir =" `mappend` (pack ca_location) `mappend` "\n\n\n"
        segment_builder = foldMap ( \ (hostname,i) ->
                    Bu.byteString $ B.concat ["DNS.", (pack $ show i), "=", hostname, "\n"]
                ) all_seen_hosts_and_indices
        all_seen_hosts_and_indices = zip all_seen_hosts [2 .. ] :: [(B.ByteString,Int)]
        complete_builder = builderM1 `mappend` builder0 `mappend` segment_builder

    -- Save it
    withFile template_cnf WriteMode $ \ handle ->
        Bu.hPutBuilder handle complete_builder

    -- Invoke openssl to create a signing request
    (_, _, _, h ) <- createProcess $ proc "openssl"
        ["req",
            "-outform" , "PEM",
            "-batch"   ,
            "-new"     ,
            "-newkey"  , "rsa:2048",
            "-nodes"   ,
            "-keyout"  ,  priv_key_filename,
            "-out"     , csr_filename,
            "-config"  , template_cnf
        ]
    waitForProcess h

    -- Restart the database....
    B.writeFile db_filename ""

    -- Invoke to sign the certificate...
    (_, _, _, h2 ) <- createProcess $ proc "openssl"
        [
              "ca"
            , "-config", template_cnf
            , "-in"    , csr_filename
            , "-out"   , cert_filename
            , "-batch"
            , "-cert"  , ca_cert
        ]
    waitForProcess h2

    return ()


urlToHTTPSScheme :: B.ByteString -> B.ByteString
urlToHTTPSScheme original_url = let
    Just (U.URI {
        U.uriScheme    = _,
        U.uriPath      = uri_path,
        U.uriQuery     = uri_query,
        U.uriAuthority = uri_authority,
        U.uriFragment  = uri_fragment
        }) = U.parseURI $ unpack original_url
  in
    pack $ show $ U.URI {
        U.uriScheme   = "https:",
        U.uriPath     = uri_path,
        U.uriQuery    = uri_query,
        U.uriFragment = uri_fragment,
        U.uriAuthority = uri_authority
        }


builderToString :: Bu.Builder -> String
builderToString =  Lch.unpack . Bu.toLazyByteString


markReportedAnalysisStage :: HashId -> ReportedAnalysisStage -> FilePath -> IO ()
markReportedAnalysisStage hashid reported_analysis_stage research_dir = do
    research_dir <- L.view researchDir
    let
        hash_bs = unHashId hashid
        scratch_dir = research_dir </> (unpack hash_bs)
    createDirectoryIfMissing True scratch_dir
    eraseStatusFiles url_hash research_dir
    let
        percent:: Int
        (status_fname, percent) = case reported_analysis_stage of
            Queued_RAS                 -> ("status.queued",0)

            Processing_RAS current_analysis_stage  ->
                let
                    percent_real :: Double
                    percent_real = fromRational $ (fromEnum  current_analysis_stage) / 7.0
                in ("status.processing", round precent_real)

            Done_RAS                   -> ("status.done", 100)
            Failed_RAS                 -> ("status.failed", 100)

            -- Here down: just as a way to not crash the program.
            -- TODO: include all the states
            _                          -> ("status.processing",0)
        file_location = scratch_dir  </> status_fname
    (withFile file_location WriteMode $ \ handle -> hPutStr handle (show percent))


markAnalysisStageAndAdvance :: (?url_state::UrlState) => ServiceStateMonad ()
markAnalysisStageAndAdvance  = do
    let
        analysis_stage = ?url_state ^. currentAnalysisStage
        hashid = ?url_state ^. urlHashId
        new_analysis_stage = shiftAnalysisStage analysis_stage

    research_dir  <- L.view researchDir
    url_state_tmvar <- L.view urlState

    liftIO $ do
        atomically $ do
            url_state <- takeTMVar url_state_tmvar
            let
                new_url_state = L.set currentAnalysisStage url_state new_analysis_stage
            putTMVar url_state_tmvar new_url_state

        markReportedAnalysisStage hashid (Processing_RAS new_analysis_stage)  research_dir

        infoM "ResearchWorker" $ "Went from " ++ (show current_analysis_stage) ++ " to " ++ (show new_analysis_stage)


eraseStatusFiles :: HashId -> FilePath -> IO ()
eraseStatusFiles url_hash research_dir = do
    let
        hash_bs = unHashId url_hash
    mapM_ (
        \ end_name -> do
            let
                complete_name = research_dir </> (unpack hash_bs) </> end_name
            removeIfExists complete_name
        )
        fileNames


fileNames :: [ FilePath ]
fileNames = [
    "status.processing",
    "status.processing",
    "status.processing",
    "status.failed"
  ]


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = E.throwIO e


-- Cleans the state of any unfinished tokens
resetState :: ServiceState -> IO ()
resetState state =
  do
    atomically $ do
        tryTakeTMVar $ state ^. nextTestUrl
        tryTakeTMVar $ state ^. nextTestUrlToCheck
        tryTakeTMVar $ state ^. nextHarvestUrl
        tryTakeTMVar $ state ^. nextDNSMasqFile
        tryTakeTMVar $ state ^. resolveCenterChan
        tryTakeTMVar $ state ^. finishRequestChan
        tryTakeTMVar $ state ^. startHarvesterBrowserChan
        tryTakeTMVar $ state ^. killHarvesterBrowserChan
        tryTakeTMVar $ state ^. startTesterBrowserChan
        tryTakeTMVar $ state ^. killTesterBrowserChan
        tryTakeTMVar $ state ^. testerReadyChan
        tryTakeTMVar $ state ^. harvesterReadyChan
        tryTakeTMVar $ (state ^. readyToGo)
        tryPutTMVar (state ^. readyToGo) (Ready_RTG 0)
    return ()



waitAndResetState :: ServiceState -> IO ()
waitAndResetState state = do
    -- Trigger stopping the mirror service, if needs come
    atomically $ tryPutTMVar (state ^. finishRequestChan) FinishRequest
    -- Not need to trigger kill-browsers, the resetter kills them
    -- automatically after a time.

    -- If the mimic server was not running before, the state of the
    -- finishRequestChan will be reset anyway....

    -- Wait a little bit, so that a few resources can be freed
    threadDelay 1000000
    -- and then reset the state
    resetState state


-- | Consumes the request body and returns it.... this can be 
--   happily done in the threadlets of Haskell without any further
--   brain-burning.....
waitRequestBody :: InputDataStream -> IO B.ByteString
waitRequestBody source =
  let
    consumer  b = do
        maybe_bytes <- await
        case maybe_bytes of

            Just bytes -> do
                -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                consumer $ b `M.mappend` (Bu.byteString bytes)

            Nothing -> do
                -- liftIO $ putStrLn "Finishing"
                return b
  in do
    full_builder <- (source $$ consumer "") :: IO Bu.Builder
    return $ (LB.toStrict . Bu.toLazyByteString) full_builder


shiftAnalysisStage :: CurrentAnalysisStage -> CurrentAnalysisStage
shiftAnalysisStage = toEnum . ( + 1) . fromEnum


correctStage :: (?analysis_stage :: CurrentAnalysisStage ) => CurrentAnalysisStage ->  Bool
correctStage expected_stage = expected_stage == ?analysis_stage


incorrectStage :: (?analysis_stage :: CurrentAnalysisStage ) => CurrentAnalysisStage -> Bool
incorrectStage expected_stage = expected_stage /= ?analysis_stage

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
-- import           Control.Exception              (catch)
import           Control.Concurrent              (forkIO, myThreadId, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Catch             (catch,throwM)
import qualified Control.Exception               as E

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
-- import qualified Data.ByteString.Lazy           as LB

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
-- import           Rede.HarFiles.JSONDataStructure (originUrl)
import           Rede.HarFiles.ServedEntry       (BadHarFile (..),
                                                  ResolveCenter, allSeenHosts,
                                                  rcName, resolveCenterFromLazyByteString,
                                                  rcOriginalUrl
                                                  )
import           Rede.Http2.MakeAttendant        (http2Attendant)
import           Rede.MainLoop.CoherentWorker
import           Rede.MainLoop.ConfigHelp        (configDir, getInterfaceName,
                                                  getMimicPort)
import           Rede.MainLoop.OpenSSL_TLS       (FinishRequest (..), tlsServeWithALPNAndFinishOnRequest)
import           Rede.Utils.ConcatConduit        (concatConduit)
import           Rede.Utils.PrintfArgByteString  ()
import           Rede.Utils.Alarm                
import           Rede.Utils.JustOneMakesSense
import           Rede.Workers.HarWorker          (harCoherentWorker)

import           Rede.Research.JobId             (HashId(..), UrlJobId, jobIdFromUrl, 
                                                  lnOriginalUrl, hashidFromJobId)
import           Rede.Research.JSONMessages      (SetNextUrl(..), DnsMasqConfig(..), WorkIndication(..))


type HashTable k v = H.CuckooHashTable k v

data CurrentAnalysisStage = 
    AnalysisRequested_CAS
    |SentToHarvester_CAS
    |ReceivedFromHarvester_CAS
    |SentToTest_CAS
    |SystemCancelled_CAS
    |Done_CAS


-- TODO: Check that we really don't need the hashid at this stage...
data ReadinessPing = ReadinessPing


-- Time to wait before unleashing an alarm... 
timeForAlarm :: Int 
timeForAlarm = 40000000


-- This state structure follows a given URL analysis. 
-- You can find a dictionary of these in the structure below...
data UrlState = UrlState {
    _urlJobIdAtState :: UrlJobId
    ,_analysisStage   :: CurrentAnalysisStage
    ,_currentAlarm   :: Alarm ()
    }


L.makeLenses ''UrlState


data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl              :: Chan HashId

    -- To be passed on to StationB
    ,_nextTestUrl                :: Chan HashId

    -- To be passed from the har files receiver (from StationA)
    -- to the DNSMasq checker...Contains urls
    , _nextTestUrlToCheck        :: Chan HashId

    -- Files to be handed out to DNSMasq
    , _nextDNSMasqFile           :: Chan DnsMasqConfig

    , _resolveCenterChan         :: Chan ResolveCenter

    , _finishRequestChan         :: Chan FinishRequest

    -- Goes between the browser resetter and the next url call. 
    -- We pass the url to load through here just to be sure....
    -- This is to avoid uncertainty with Chrome.
    -- The logic for "starts" is that we write to it when we want
    -- the browser started, then we read from it to start it, and 
    -- then put the url in the next step.
    , _startHarvesterBrowserChan :: Chan HashId

    -- Order sent to the browser resetter of anyquilating the current
    -- Chrome instance. We write to this one as soon as we want to kill
    -- the browser, and then read from it when we are ready to go to the 
    -- next step.
    , _killHarvesterBrowserChan  :: Chan HashId

    , _startTesterBrowserChan    :: Chan HashId
    
    , _killTesterBrowserChan     :: Chan HashId

    -- TODO: Is this the best idiom to use?
    , _testerReadyChan           :: Chan ReadinessPing

    , _harvesterReadyChan        :: Chan ReadinessPing

    -- The "Research dir" is the "hars" subdirectory of the mimic
    -- scratch directory. 
    , _researchDir               :: FilePath 

    -- The IP address that StationB needs to connect
    , _useAddressForStationB     :: B.ByteString

    -- The state of each URL 
    , _urlState                  :: HashTable HashId UrlState

    , _nextUrlAsked              :: JustOneMakesSense

    , _testUrlAsked              :: JustOneMakesSense 
    }


L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO


runResearchWorker :: 
    Chan ResolveCenter 
    -> Chan FinishRequest 
    -> FilePath                -- Research dir
    -> B.ByteString
    -> IO CoherentWorker
runResearchWorker resolve_center_chan finish_request_chan research_dir use_address_for_station_b = do 

    liftIO $ debugM "ResearchWorker" "(on research worker)"

    next_harvest_url_chan         <- newChan
    next_test_url_chan            <- newChan -- <-- Goes from the dnsmasq response handler 
    next_test_url_to_check_chan   <- newChan -- <-- Goes to the dnsmasq response handler
    next_dns_masq_file            <- newChan

    start_harvester_browser       <- newChan
    kill_harvester_browser        <- newChan

    start_tester_browser          <- newChan
    kill_tester_browser           <- newChan

    harvester_ready               <- newChan 
    tester_ready                  <- newChan 

    new_url_state  <- H.new
    next_url_asked <- newJOMSAtZero
    test_url_asked <- newJOMSAtZero


    let     
        state = ServiceState {
             _nextHarvestUrl              = next_harvest_url_chan
            ,_nextTestUrl                 = next_test_url_chan
            ,_nextTestUrlToCheck          = next_test_url_to_check_chan
            ,_nextDNSMasqFile             = next_dns_masq_file
            ,_resolveCenterChan           = resolve_center_chan
            ,_finishRequestChan           = finish_request_chan
            ,_researchDir                 = research_dir
            ,_useAddressForStationB       = use_address_for_station_b
            ,_urlState                    = new_url_state
            ,_nextUrlAsked                = next_url_asked
            ,_testUrlAsked                = test_url_asked

            ,_startHarvesterBrowserChan   = start_harvester_browser
            ,_killHarvesterBrowserChan    = kill_harvester_browser
            ,_startTesterBrowserChan      = start_tester_browser
            ,_killTesterBrowserChan       = kill_tester_browser

            ,_testerReadyChan             = tester_ready
            ,_harvesterReadyChan          = harvester_ready
            }
    return $ \request -> runReaderT (researchWorkerComp request) state


researchWorkerComp :: Request -> ServiceStateMonad PrincipalStream
researchWorkerComp (input_headers, maybe_source) = do 
    next_harvest_url              <- L.view nextHarvestUrl
    next_dnsmasq_chan             <- L.view nextDNSMasqFile
    next_test_url_chan            <- L.view nextTestUrl
    next_test_url_to_check_chan   <- L.view nextTestUrlToCheck
    resolve_center_chan           <- L.view resolveCenterChan 
    finish_chan                   <- L.view finishRequestChan 
    base_research_dir             <- L.view researchDir
    use_address_for_station_b     <- L.view useAddressForStationB
    url_state_hashtable           <- L.view urlState
    test_url_asked                <- L.view testUrlAsked
    next_url_asked                <- L.view nextUrlAsked

    start_harvester_browser       <- L.view startHarvesterBrowserChan   
    kill_harvester_browser        <- L.view killHarvesterBrowserChan    
    start_tester_browser          <- L.view startTesterBrowserChan      
    kill_tester_browser           <- L.view killTesterBrowserChan     
    tester_ready                  <- L.view testerReadyChan 
    harvester_ready               <- L.view harvesterReadyChan  

    let 
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers

    case method of 

        -- Most requests are served by POST to emphasize that a request changes 
        -- the state of this program... 
        Post_RM 
            | req_url == "/nexturl/" -> doJOMS next_url_asked 
                (
                    do 
                        -- Starts harvesting of a resource... this request is made by StationA
                        liftIO $ do 
                            infoM "ResearchWorker" ".. /nexturl/ asked "
                            my_thread <- myThreadId
                            infoM "ResearchWorker" $ "waiting on thread " ++ (show my_thread)

                        hashid <- 
                            catch
                                (liftIO $ do 
                                    -- First we wait for a notification about the browser being ready
                                    readChan harvester_ready
                                    -- And then we give out the next url to scan...
                                    readChan next_harvest_url
                                )
                                on_url_wait_interrupted
                        url <- urlFromHashId hashid
                        liftIO $ infoM "ResearchWorker" .  builderToString $ "..  /nexturl/" `mappend` " answer " `mappend` (Bu.byteString url) `mappend` " "
                        liftIO $ markAnalysisStage hashid SentToHarvester_CAS base_research_dir

                        -- Mark it as sent...
                        modifyUrlState hashid $ \ old_state -> do 
                            -- Set an alarm here also...this alarm should be disabled when a .har file comes....
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            let 
                                a1 = analysisStage .~ SentToHarvester_CAS $ old_state
                            alarm <- liftIO $ newAlarm timeForAlarm $ do 
                                markAnalysisStage hashid SystemCancelled_CAS base_research_dir
                                errorM "ResearchWorker" . builderToString $ " failed harvesting " `mappend` (Bu.byteString req_url) `mappend` " (timeout) "
                                writeChan kill_harvester_browser hashid
                                errorM "ResearchWorker" " ... harvester browser asked killed "
                            let
                                a2 = currentAlarm .~ alarm $ a1
                            return a2 

                        -- And finally return.
                        let msg = WorkIndication hashid url
                        return $ simpleResponse 200 $ LB.toStrict $ Da.encode msg
                ) (
                    do 
                        -- Somebody asked too many times, say it
                        liftIO $ errorM "ResearchWorker" ".. /nexturl/ asked too many times, returning 501"
                        return $ simpleResponse 501 "Asked too many times"
                )
                       
            | req_url == "/browserready/StationA" -> do 
                -- Check when we can proceed
                liftIO $ do 
                    infoM "ResearchWorker" ".. /browserready/StationA/"
                    writeChan harvester_ready ReadinessPing
                return $ simpleResponse 200 "Ok"

            | req_url == "/browserready/StationB" -> do 
                -- Check when we can proceed
                liftIO $ do 
                    infoM "ResearchWorker" ".. /browserready/StationB/"
                    writeChan tester_ready ReadinessPing
                return $ simpleResponse 200 "Ok"

            | req_url == "/testurl/" -> doJOMS test_url_asked (
                    -- Normal flow
                    do 
                        -- Sends the next test url to the Chrome extension, this starts processing by 
                        -- StationB... the request is started by StationB and we send the url in the response...
                        hashid <- liftIO $ do
                            -- Wait for readiness indication in the browser
                            readChan tester_ready
                            -- And then snatch the url.... 
                            readChan next_test_url_chan
                        anyschema_url <-  urlFromHashId hashid
                        let https_url = urlToHTTPSScheme anyschema_url
                        liftIO $ do
                            infoM "ResearchWorker" . builderToString $ "..  /testurl/ answered with " `mappend` (Bu.byteString https_url)
                            -- Mark the new stage 
                            markAnalysisStage hashid SentToTest_CAS base_research_dir

                        modifyUrlState hashid $ \ old_state -> do 
                            -- This alarm should be disabled by the http2har file
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            let 
                                a1 = analysisStage .~ SentToTest_CAS $ old_state
                            alarm <- liftIO $ newAlarm 15000000 $ do 
                                errorM "ResearchWorker" . builderToString $ " failed testing " `mappend` (Bu.byteString https_url)
                                --writeChan kill_tester_browser ""
                            return $ currentAlarm .~ alarm $ a1
                        let msg = WorkIndication hashid https_url
                        return $ simpleResponse 200 $ LB.toStrict $ Da.encode msg
                ) (
                    -- Problems spotted
                    do
                        liftIO $ errorM "ResearchWorker" ".. /testurl/ was asked more than once, returning 501"
                        return $ simpleResponse 501 "Asked too many times"
                ) 

            | req_url == "/har/", Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and 
                -- makes all the arrangements for it to be tested using HTTP 2
                liftIO $ infoM "ResearchWorker" "..  /har/"
                catch 
                    (do
                        (resolve_center, har_file_contents) <- liftIO $ output_computation source
                        let 
                            test_url = resolve_center ^. rcOriginalUrl
                            use_text = "Response processed"
                            hashid = resolve_center ^. rcName

                        -- Okey, cancel the alarm first 
                        modifyUrlState hashid $ \ old_state -> do
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            alarm <- liftIO $ newAlarm 15000000 $ do 
                                errorM "ResearchWorker" . builderToString $ " failed forwarding " `mappend` (Bu.byteString test_url)
                            return $ currentAlarm .~ alarm $ old_state

                        -- Mark the state
                        liftIO $ markAnalysisStage hashid ReceivedFromHarvester_CAS base_research_dir
                        

                        -- Create the contents to go in the dnsmasq file, and queue them 
                        let 
                            all_seen_hosts = resolve_center ^. allSeenHosts
                            dnsmasq_contents = dnsMasqFileContentsToIp use_address_for_station_b all_seen_hosts 

                        liftIO $ do
                            writeChan next_dnsmasq_chan $! DnsMasqConfig hashid dnsmasq_contents

                            -- We also need to queue the url somewhere to be checked by the dnsmasqupdated entrypoint
                            writeChan kill_harvester_browser hashid

                            -- And the resolve center to be used by the serving side
                            writeChan resolve_center_chan resolve_center

                            -- We can also use this opportunity to create a folder for this research 
                            -- project 
                            let 
                                scratch_folder = (base_research_dir) </> (unpack . unHashId $ hashid)
                                har_filename = scratch_folder </> "harvested.har"
                            createDirectoryIfMissing True scratch_folder
                            LB.writeFile har_filename har_file_contents

                        return $ simpleResponse 200 use_text
                    )
                    on_bad_har_file

            | req_url == "/startbrowser/StationA" -> 
                unQueueStartBrowser "/startbrowser/StationA" start_harvester_browser

            | req_url == "/killbrowser/StationA" -> do 
                unQueueKillBrowser "/killbrowser/StationA" kill_harvester_browser

            | req_url == "/startbrowser/StationB" -> do 
                unQueueStartBrowser "/startbrowser/StationB" start_tester_browser

            | req_url == "/killbrowser/StationB" -> do 
                unQueueKillBrowser "/killbrowser/StationB" kill_tester_browser

                -- let endToken = "EAJ"
                -- liftIO $ do 
                --     -- Let's wait a second before killing the process....
                --     infoM "ResearchWorker" " .. /killbrowser/StationB"
                --     threadDelay 1000000
                --     -- There is an url being returned from here, but we don't need it... 
                --     readChan kill_tester_browser

                -- return $ simpleResponse 200 endToken


            | req_url == "/http2har/", Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and 
                -- makes all the arrangements for it to be tested using HTTP 2
                liftIO $ infoM "ResearchWorker" "..  /http2har/"
                catch 
                    (do
                        (resolve_center,  har_contents_lb) <- liftIO $ output_computation source
                        let 
                            use_text = "Response processed"
                            hashid = resolve_center ^. rcName
                        let 
                            scratch_folder = base_research_dir </> (unpack . unHashId $ hashid)
                            har_filename = scratch_folder </> "test_http2.har"
                        liftIO $ LB.writeFile har_filename har_contents_lb

                        -- Okey, cancel the alarm  
                        modifyUrlState  hashid $ \ old_state -> do
                            liftIO $ cancelAlarm $ old_state ^. currentAlarm
                            return old_state

                        -- And finish the business
                        liftIO $ do
                            writeChan finish_chan FinishRequest
                            writeChan kill_tester_browser hashid

                        -- Mark the state 
                        liftIO $ markAnalysisStage hashid Done_CAS base_research_dir

                        return $ simpleResponse 200 use_text
                    )
                    on_bad_har_file

            | req_url == "/dnsmasq/" , Just source <- maybe_source  -> do 
                catch 
                    (liftIO $ do
                        received_version <- waitRequestBody source
                        infoM "ResearchWorker" $ " .. updatednsmasq VERSION= " ++ (show received_version)
                        -- Sends a DNSMASQ file to the test station ("StationB")
                        infoM "ResearchWorker" ".. /dnsmasq/ asked"
                        -- Serve the DNS masq file corresponding to the last .har file 
                        -- received.
                        dnsmasq_contents <- readChan next_dnsmasq_chan
                        infoM "ResearchWorker" ".. /dnsmasq/ answers"
                        infoM "ResearchWorker" $ "Sending " ++ (show dnsmasq_contents)
                        return $ simpleResponse 200 $ LB.toStrict $ Da.encode dnsmasq_contents
                    )
                    on_general_error

            | req_url == "/dnsmasqupdated/",  Just source <- maybe_source -> do 
                -- Received  advice that everything is ready to proceed
                liftIO $ do 
                    received_id <- waitRequestBody source
                    -- Ensure any in-system changes get enough time to propagate
                    threadDelay 2000000
                    testurl_hashid <- readChan next_test_url_to_check_chan  
                    infoM "ResearchWorker" $ "Dnsmasqupdated received " ++ (show received_id)
                    infoM "ResearchWorker" $ "and test_url has hash "   ++ (show testurl_hashid )
                    -- Write the url to the queue so that /testurl/ can return when seeing this value 
                    writeChan next_test_url_chan testurl_hashid
                    writeChan start_tester_browser testurl_hashid

                return $ simpleResponse 200 $ "ok"

            | req_url == "/setnexturl/", Just source <- maybe_source -> do 
                -- Receives a url to investigate from the user front-end, or from curl
                url_or_json_to_analyze <- liftIO $ source $$ concatConduit
                let 
                    url_to_analyze = case Da.decode . LB.fromStrict $ url_or_json_to_analyze of 
                        Just (SetNextUrl x)  -> x 
                        Nothing              -> url_or_json_to_analyze

                -- Ojivas are launched at this point, so let it be IO
                url_job_id <- liftIO $ jobIdFromUrl url_to_analyze
                let hashid = hashidFromJobId url_job_id

                alarm <- liftIO $ newAlarm 25000000 $ do 
                    errorM "ResearchWorker" . builderToString $ 
                        "Job for url " `mappend` (Bu.byteString url_to_analyze) `mappend` " still waiting... "
                    markAnalysisStage hashid SystemCancelled_CAS base_research_dir
                let 
                    new_url_state = UrlState url_job_id AnalysisRequested_CAS alarm

                liftIO $ do
                    H.insert 
                        url_state_hashtable hashid new_url_state 
                    infoM "ResearchWorker" . builderToString $ 
                        ".. /setnexturl/ asked " `mappend` (Bu.byteString url_to_analyze)


                    writeChan next_harvest_url hashid

                    writeChan start_harvester_browser hashid

                    writeChan next_test_url_to_check_chan  hashid

                    markAnalysisStage hashid AnalysisRequested_CAS base_research_dir

                return $ simpleResponse 200 (unHashId hashid)

            | otherwise     -> do 
                return $ simpleResponse 500 "Can't handle url and method"

        _ -> do 
                return $ simpleResponse 500 "Can't handle url and method"


  where 
    on_bad_har_file :: BadHarFile -> ServiceStateMonad PrincipalStream
    on_bad_har_file  (BadHarFile _) = 
        return $ simpleResponse 500 "BadHarFile"

    on_general_error :: E.SomeException -> ServiceStateMonad PrincipalStream
    on_general_error e = do
        liftIO $ errorM "ResearchWorker" (show e)
        throwM e

    on_url_wait_interrupted :: StreamCancelledException -> ServiceStateMonad a
    on_url_wait_interrupted e = do 
        liftIO $ warningM "ResearchWorker" "Post wait interrupted"
        throwM e 

    output_computation :: InputDataStream -> IO (ResolveCenter,  LB.ByteString)
    output_computation source = do 
        full_builder <- source $$ consumer ""
        let
            lb  = Bu.toLazyByteString full_builder
            rc = resolveCenterFromLazyByteString lb
        return (rc, lb)
    consumer  b = do 
        maybe_bytes <- await 
        case maybe_bytes of 

            Just bytes -> do
                -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                consumer $ b `M.mappend` (Bu.byteString bytes)

            Nothing -> do
                -- liftIO $ putStrLn "Finishing"
                return b


unQueueStartBrowser :: B.ByteString -> Chan HashId -> ServiceStateMonad PrincipalStream
unQueueStartBrowser log_url chan_to_read = 
  do 
    -- Ensure start
    bs_hashid <- liftIO $ do
        infoM "ResearchWorker" (unpack $ B.append "start browser asked .. " log_url)
        hashid <- readChan chan_to_read
        let bs_hashid = unHashId hashid
        infoM "ResearchWorker" (unpack $ B.concat ["start browser delivered .. ", log_url, " ", bs_hashid ])
        return bs_hashid

    return $ simpleResponse 200 $ B.append "hashid=" bs_hashid


unQueueKillBrowser :: B.ByteString -> Chan HashId -> ServiceStateMonad PrincipalStream
unQueueKillBrowser log_url chan_to_read = do 
    -- StationA refers to the harvester....
    hashid <- liftIO $ do 
        -- Let's wait a second before killing the process....
        infoM "ResearchWorker" $ unpack $ B.append " .. " log_url
        hashid <- readChan chan_to_read
        infoM "ResearchWorker" $ unpack $ B.append log_url " cleared for killing"
        return hashid

    return $ simpleResponse 200 $ B.append "hashid=" (unHashId hashid)


urlFromHashId :: HashId -> ServiceStateMonad B.ByteString
urlFromHashId hashid = do 
    url_state_hashtable <- L.view urlState
    -- Pattern matching fails here and we are in deep trouble...
    Just state <- liftIO $ H.lookup url_state_hashtable hashid
    return $ state ^. ( urlJobIdAtState . lnOriginalUrl )


-- And here for when we need to fork a server to load the .har file from 
spawnHarServer :: FilePath -> Chan ResolveCenter -> Chan  FinishRequest -> IO ()
spawnHarServer mimic_dir resolve_center_chan finish_request_chan = do 
    let 
        mimic_config_dir = configDir mimic_dir    
    port  <-  getMimicPort
    infoM "ResearchWorker.SpawnHarServer"  $ ".. Mimic port: " ++ (show port)
    iface <-  getInterfaceName mimic_config_dir
    infoM "ResearchWorker.SpawnHarServer" $ ".. Mimic using interface: " ++ (show iface)

    finish_request_mvar <- newEmptyMVar 

    let 
        watcher = do 
            r <- readChan finish_request_chan
            infoM "ResearchWorker.SpawnHarServer" $ " .. Finishing mimic service  " 
            putMVar finish_request_mvar r
            watcher 

    forkIO watcher

    let 
        serveWork = do

            resolve_center <- readChan resolve_center_chan

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
                http2worker = harCoherentWorker resolve_center
            tlsServeWithALPNAndFinishOnRequest  cert_filename priv_key_filename iface [ 
                 -- ("h2-14", wrapSession veryBasic)
                 ("h2-14", http2Attendant http2worker)

                 -- TODO: Let the user select HTTP/1.1 from time to time...
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


modifyUrlState :: HashId -> (UrlState -> ServiceStateMonad UrlState) -> ServiceStateMonad ()
modifyUrlState key mutator = do 
    h  <- L.view urlState
    vv <- liftIO $ H.lookup h key
    case vv  of 
        Just v -> do 
            new_value <- mutator v 
            liftIO $ H.insert h key new_value

        Nothing -> do
            -- Can this even happen by accident?
            liftIO 
                $ warningM "ResearchWorker" 
                $ builderToString 
                $ "Trying to modify state of non-seen url job " `mappend` 
                  (Bu.byteString . unHashId $ key)


markAnalysisStage :: HashId -> CurrentAnalysisStage -> FilePath -> IO ()
markAnalysisStage url_hash current_analysis_stage research_dir = do 
    let 
        hash_bs = unHashId url_hash
        scratch_dir = research_dir </> (unpack hash_bs)
    createDirectoryIfMissing True scratch_dir
    eraseStatusFiles url_hash research_dir
    let 
        percent:: Int
        (status_fname, percent) = case current_analysis_stage of 
            AnalysisRequested_CAS      -> ("status.processing", 5)
            SentToHarvester_CAS        -> ("status.processing", 20)
            ReceivedFromHarvester_CAS  -> ("status.processing", 45)
            SentToTest_CAS             -> ("status.processing", 65)
            SystemCancelled_CAS        -> ("status.failed", 0)
            Done_CAS                   -> ("status.done", 100)
        file_location = scratch_dir  </> status_fname 
    (withFile file_location WriteMode $ \ handle -> hPutStr handle (show percent))


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

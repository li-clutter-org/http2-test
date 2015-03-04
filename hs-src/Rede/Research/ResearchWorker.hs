{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.Research.ResearchWorker(
    runResearchWorker
    ,spawnHarServer
    ) where 


import           Control.Lens                   ((^.))
import qualified Control.Lens                   as L
-- import           Control.Exception              (catch)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent             (forkIO)
import           Control.Monad.Catch            (catch)

import qualified Data.ByteString                as B
import           Data.ByteString.Char8          (unpack, pack)
import           Data.Conduit
import qualified Data.Monoid                    as M
import           Data.Monoid                    (
                                                --mempty, 
                                                mappend)
import           Data.Foldable                  (foldMap)
-- import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Builder        as Bu

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader     (ReaderT (..), runReaderT)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import           Rede.Workers.ResponseFragments (RequestMethod (..),
                                                 getMethodFromHeaders,
                                                 getUrlFromHeaders,
                                                 simpleResponse)

import           Rede.HarFiles.DnsMasq          (dnsMasqFileContents)
import           Rede.HarFiles.ServedEntry      (BadHarFile (..), ResolveCenter,
                                                 allSeenHosts, resolveCenterAndOriginUrlFromLazyByteString,
                                                 hashFromResolveCenter
                                                 )
import           Rede.MainLoop.CoherentWorker
import           Rede.Workers.HarWorker         (harCoherentWorker)
import           Rede.Http2.MakeAttendant       (http2Attendant)
import           Rede.MainLoop.ConfigHelp       (configDir, getInterfaceName,
                                                 getMimicPort, mimicDataDir)
import           Rede.MainLoop.OpenSSL_TLS      (FinishRequest(..), tlsServeWithALPNAndFinishOnRequest)
import           Rede.Utils.ConcatConduit       (concatConduit)



data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl :: Chan B.ByteString

    -- To be passed on to StationB
    ,_nextTestUrl :: Chan B.ByteString

    -- Files to be handed out to DNSMasq
    , _nextDNSMasqFile :: Chan B.ByteString

    , _resolveCenterChan :: Chan ResolveCenter

    , _finishRequestChan :: Chan FinishRequest

    -- This one we use in the opposite sense: we write here...
    -- ,_servingHar    :: Chan (ResolveCenter, OriginUrl )
    }

L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO


runResearchWorker :: 
    Chan B.ByteString 
    -> Chan ResolveCenter 
    -> Chan FinishRequest 
    -> IO CoherentWorker
runResearchWorker url_chan resolve_center_chan finish_request_chan = do 
    liftIO $ putStrLn "Init block called"
    next_test_url_chan <- newChan 
    next_dns_masq_file <- newChan
    liftIO $ putStrLn "End of init block"
    let    
        state = ServiceState {
             _nextHarvestUrl = url_chan
            ,_nextTestUrl    = next_test_url_chan
            ,_nextDNSMasqFile = next_dns_masq_file
            ,_resolveCenterChan = resolve_center_chan
            ,_finishRequestChan = finish_request_chan
            }
    return $ \request -> runReaderT (researchWorkerComp request) state


researchWorkerComp :: Request -> ServiceStateMonad PrincipalStream
researchWorkerComp (input_headers, maybe_source) = do 
    next_harvest_url    <- L.view nextHarvestUrl
    next_dnsmasq_chan   <- L.view nextDNSMasqFile
    next_test_url_chan  <- L.view nextTestUrl
    resolve_center_chan <- L.view resolveCenterChan 
    finish_chan         <- L.view finishRequestChan 
    let 
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers

    case method of 

        -- Most requests are served by POST to emphasize that a request changes 
        -- the state of this program... 
        Post_RM 
            | req_url == "/nexturl/" -> do 
                -- Starts harvesting of a resource
                liftIO $ putStrLn "..  /nexturl/"
                url <- liftIO $ readChan next_harvest_url
                return $ simpleResponse 200 url 

            | req_url == "/testurl/" -> do 
                -- Sends the next test url to the Chrome extension
                liftIO $ putStrLn "..  /testurl/"
                url <- liftIO $ readChan next_test_url_chan
                return $ simpleResponse 200 url

            | req_url == "/har/", Just source <- maybe_source -> do
                -- Receives a .har object from the harvest station ("StationA"), and 
                -- makes all the arrangements for it to be tested using HTTP 2
                liftIO $ putStrLn "..  /har/"
                catch 
                    (do
                        (resolve_center, test_url) <- liftIO $ output_computation source
                        let 
                            use_text = "Response processed"
                        -- serving_har_chan <- L.view servingHar

                        -- Create the contents to go in the dnsmasq file, and queue them 
                        let 
                            all_seen_hosts = resolve_center ^. allSeenHosts
                            dnsmasq_contents = dnsMasqFileContents all_seen_hosts 

                        liftIO $ writeChan next_dnsmasq_chan dnsmasq_contents

                        -- We also need to queue the url somewhere to be used by StationB
                        liftIO $ writeChan next_test_url_chan test_url

                        -- And the resolve center to be used by the serving side
                        liftIO $ writeChan resolve_center_chan resolve_center

                        return $ simpleResponse 200 use_text
                    )
                    error_handler

            | req_url == "/dnsmasq/" -> do 
                -- Sends a DNSMASQ file to the test station ("StationB")
                liftIO $ putStrLn ".. /dnsmasq/ asked"
                -- Serve the DNS masq file corresponding to the last .har file 
                -- received.
                dnsmasq_contents <- liftIO $ readChan next_dnsmasq_chan
                liftIO $ putStrLn ".. /dnsmasq/ about to answer"
                
                return $ simpleResponse 200 dnsmasq_contents

            | req_url == "/setnexturl/", Just source <- maybe_source -> do 
                -- Receives a url to investigate from the user front-end
                liftIO $ putStrLn ".. /setnexturl/"
                post_contents <- liftIO $ source $$ concatConduit
                -- Wishful thinking: post_contents is just the url to analyze...
                liftIO $ writeChan next_harvest_url post_contents
                liftIO $ putStrLn $ "... " ++ (unpack post_contents)

                return $ simpleResponse 200 "url queued"

            | otherwise     -> do 
                return $ simpleResponse 500 "Can't handle url and method"

        _ -> do 
                return $ simpleResponse 500 "Can't handle url and method"


  where 
    error_handler :: BadHarFile -> ServiceStateMonad PrincipalStream
    error_handler  (BadHarFile _) = 
        return $ simpleResponse 500 "BadHarFile"

    output_computation :: InputDataStream -> IO (ResolveCenter, B.ByteString)
    output_computation source = do 
        full_builder <- source $$ consumer ""
        let
            lb  = Bu.toLazyByteString full_builder
            resolve_center_and_url = resolveCenterAndOriginUrlFromLazyByteString lb
        return resolve_center_and_url
    consumer  b = do 
        maybe_bytes <- await 
        case maybe_bytes of 

            Just bytes -> do
                -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                consumer $ b `M.mappend` (Bu.byteString bytes)

            Nothing -> do
                liftIO $ putStrLn "Finishing"
                return b


-- And here for when we need to fork a server to load the .har file from 
spawnHarServer :: FilePath -> Chan ResolveCenter -> Chan  FinishRequest -> IO ()
spawnHarServer mimic_dir resolve_center_chan finish_request_chan = do 
    let 
        mimic_config_dir = configDir mimic_dir    
    port  <-  getMimicPort
    putStrLn $  "Mimic port: " ++ (show port)
    iface <-  getInterfaceName mimic_config_dir
    putStrLn $ "Using interface: " ++ (show iface)


    finish_request_mvar <- newEmptyMVar 

    let 
        watcher = do 
            r <- readChan finish_request_chan
            putMVar finish_request_mvar r
            watcher 

    forkIO watcher

    let 
        serveWork = do

            resolve_center <- readChan resolve_center_chan

            let 
                har_filename = unpack $ hashFromResolveCenter resolve_center
                priv_key_filename = privKeyFilename mimic_dir har_filename
                cert_filename  = certificateFilename mimic_dir har_filename
                host_list = resolve_center ^. allSeenHosts

            putStrLn $ "About to create comprhensive certificate for " ++ har_filename
            getComprehensiveCertificate mimic_dir har_filename host_list

            putStrLn $ "Chosen cert. at file: " ++ (show cert_filename)
            putStrLn $ "... with private key: " ++ (show priv_key_filename)
            putStrLn $ "Starting server"

            let 
                http2worker = harCoherentWorker resolve_center
            tlsServeWithALPNAndFinishOnRequest  cert_filename priv_key_filename iface [ 
                 -- ("h2-14", wrapSession veryBasic)
                 ("h2-14", http2Attendant http2worker)

                 -- TODO: Let the user select HTTP/1.1 from time to time...
                ] port finish_request_mvar
            -- 
            putStrLn ".. Finished a har serve session"
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
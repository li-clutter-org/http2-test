{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.Research.ResearchWorker(
    runResearchWorker
    ) where 


import qualified Control.Lens                   as L
import           Control.Lens                   ( (^.) )
-- import           Control.Exception              (catch)
import           Control.Monad.Catch            (catch)
import           Control.Concurrent.Chan
-- import           Control.Concurrent.MVar

import           Data.Conduit
import qualified Data.ByteString                as B
import           Data.ByteString.Char8          (pack, unpack)
import qualified Data.Monoid                    as M
-- import qualified Data.ByteString.Lazy           as LB 
import qualified Data.ByteString.Builder        as Bu

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader     (ReaderT(..), runReaderT)

import           System.IO                      (
                                                 -- IOMode (..), 
                                                 -- hClose,
                                                 -- openBinaryFile
                                                 )

import           Rede.Workers.ResponseFragments (
                                                 getMethodFromHeaders,
                                                 getUrlFromHeaders,
                                                 simpleResponse,
                                                 RequestMethod(..)
                                                 )

import           Rede.MainLoop.CoherentWorker
import           Rede.HarFiles.ServedEntry      (ResolveCenter, 
                                                 BadHarFile(..),
                                                 resolveCenterAndOriginUrlFromLazyByteString,
                                                 allSeenHosts
                                                 )
import           Rede.HarFiles.DnsMasq          (dnsMasqFileContents)
import           Rede.Utils.ConcatConduit       (concatConduit)


data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl :: Chan B.ByteString

    -- To be passed on to StationB
    ,_nextTestUrl :: Chan B.ByteString

    -- Files to be handed out to DNSMasq
    , _nextDNSMasqFile :: Chan B.ByteString

    -- This one we use in the opposite sense: we write here...
    -- ,_servingHar    :: Chan (ResolveCenter, OriginUrl )
    }

L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO


runResearchWorker :: 
    Chan B.ByteString 
    -> IO CoherentWorker
runResearchWorker url_chan  = do 
    liftIO $ putStrLn "Init block called"
    next_test_url_chan <- newChan 
    next_dns_masq_file <- newChan
    liftIO $ putStrLn "End of init block"
    let    
        state = ServiceState {
             _nextHarvestUrl = url_chan
            ,_nextTestUrl    = next_test_url_chan
            ,_nextDNSMasqFile = next_dns_masq_file
            }
    return $ \request -> runReaderT (researchWorkerComp request) state


researchWorkerComp :: Request -> ServiceStateMonad PrincipalStream
researchWorkerComp (input_headers, maybe_source) = do 
    next_harvest_url   <- L.view nextHarvestUrl
    next_dnsmasq_chan  <- L.view nextDNSMasqFile
    next_test_url_chan <- L.view nextTestUrl
    let 
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers

    case method of 

        -- Most requests are served by POST to emphasize that a request changes 
        -- the state of this program... 
        Post_RM 
            | req_url == "/nexturl/" -> do 
                -- This is it... Let the browser use this data
                -- This can block here... hope it don't be the end of the world...
                liftIO $ putStrLn "..  /nexturl/"
                url <- liftIO $ readChan next_harvest_url
                return $ simpleResponse 200 url 

            | req_url == "/testurl/" -> do 
                liftIO $ putStrLn "..  /testurl/"
                url <- liftIO $ readChan next_test_url_chan
                return $ simpleResponse 200 url

            | req_url == "/har/", Just source <- maybe_source -> do
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

                        return $ simpleResponse 200 use_text
                    )
                    error_handler

            | req_url == "/dnsmasq/" -> do 
                liftIO $ putStrLn ".. /dnsmasq/ asked"
                -- Serve the DNS masq file corresponding to the last .har file 
                -- received.
                dnsmasq_contents <- liftIO $ readChan next_dnsmasq_chan
                liftIO $ putStrLn ".. /dnsmasq/ about to answer"
                
                return $ simpleResponse 200 dnsmasq_contents

            | req_url == "/setnexturl/", Just source <- maybe_source -> do 
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
    error_handler  (BadHarFile contents) = 
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


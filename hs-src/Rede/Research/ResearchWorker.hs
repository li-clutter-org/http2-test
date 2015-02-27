{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.Research.ResearchWorker(
    startResearchWorker
    ) where 


import qualified Control.Lens                   as L
-- import           Control.Exception              (bracket)
import           Control.Concurrent.Chan
-- import           Control.Concurrent.MVar

import           Data.Conduit
import qualified Data.ByteString                as B
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
                                                 resolveCenterAndOriginUrlFromLazyByteString)


type OriginUrl = B.ByteString


data ServiceState = ServiceState {
    -- Put one here, let it run through the pipeline...
    _nextHarvestUrl :: Chan B.ByteString

    -- This one we use in the opposite sense: we write here...
    ,_servingHar    :: Chan (ResolveCenter, OriginUrl )
    }

L.makeLenses ''ServiceState


type ServiceStateMonad = ReaderT ServiceState IO


startResearchWorker :: 
    Chan B.ByteString 
    -> Chan (ResolveCenter, B.ByteString)
    -> CoherentWorker
startResearchWorker url_chan serving_har         request = let 
    state = ServiceState {
        _nextHarvestUrl = url_chan
        ,_servingHar    = serving_har
        }

    in runReaderT (researchWorkerComp request) state


researchWorkerComp :: Request -> ServiceStateMonad PrincipalStream
researchWorkerComp (input_headers, maybe_source) = do 
    url_chan <- L.view nextHarvestUrl
    let 
        method = getMethodFromHeaders input_headers
        req_url  = getUrlFromHeaders input_headers

    case method of 
        Post_RM 
            | req_url == "/nexturl/" -> do 
                -- This is it... Let the browser use this data
                -- This can block here... hope it don't be the end of the world...
                url <- liftIO $ readChan url_chan
                return $ simpleResponse 200 url 

            | req_url == "/har/", Just source <- maybe_source -> do
                resolve_center_and_url <- liftIO $ output_computation source
                let 
                    use_text = "Response processed"
                serving_har_chan <- L.view servingHar
                liftIO $ writeChan serving_har_chan resolve_center_and_url
                return $ simpleResponse 200 use_text

            | otherwise     -> do 
                return $ simpleResponse 500 "Can't handle url and method"


        _ -> do 
                return $ simpleResponse 500 "Can't handle url and method"


  where 
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


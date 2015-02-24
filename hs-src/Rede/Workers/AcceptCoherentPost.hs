{-# LANGUAGE OverloadedStrings #-}
module Rede.Workers.AcceptCoherentPost(
    acceptCoherentPost
    ,informUrlToDownload
    ) where 


import           Control.Exception              (bracket)

import           Data.Conduit

import           Control.Monad.IO.Class
import qualified Data.ByteString                as B
import           System.IO                      (IOMode (..), hClose,
                                                 openBinaryFile)

import           Rede.Workers.ResponseFragments (ajaxResponseWithLength,
                                                 getMethodFromHeaders,
                                                 simpleResponse,
                                                 RequestMethod(..))

import           Rede.MainLoop.CoherentWorker



informUrlToDownload :: B.ByteString -> CoherentWorker 
informUrlToDownload url (input_headers, maybe_source) = do 
    let method = getMethodFromHeaders input_headers

    case method of 
        Get_RM -> do 
            -- This is it... Let the browser use this data
            return $ simpleResponse 200 url 

        _      -> do 
            -- TODO: Handle this properly, if needs come 
            error "Got a request by a different method at this stage..."




acceptCoherentPost :: FilePath -> CoherentWorker 
acceptCoherentPost save_at (input_headers, maybe_source) = do 

    -- Create write handle so that stuff goes to destination
    -- file, and let it run....

    case maybe_source of 

        Just source -> do

            -- We could fork this in its own thread....
            -- forkIO $ output_computation
            -- But let's keep it here
            output_computation source

            let 
                use_text = "File is being accepted"

            return $ ajaxResponseWithLength 200 (B.length use_text) "text/plain" use_text

        Nothing  ->
            return $ simpleResponse 500 "Was awaiting POST data"



  where 
    output_computation :: InputDataStream -> IO ()
    output_computation source = bracket
                (openBinaryFile save_at WriteMode)
                hClose
                (\ write_handle -> source $$ consumer write_handle)
    consumer write_handle = do 
        maybe_bytes <- await 
        case maybe_bytes of 

            Just bytes -> do
                liftIO $ B.hPut write_handle bytes 
                consumer write_handle

            Nothing -> 
                return ()
    
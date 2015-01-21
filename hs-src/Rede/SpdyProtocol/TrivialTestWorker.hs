{-# LANGUAGE OverloadedStrings 
            , GeneralizedNewtypeDeriving
            , TypeSynonymInstances
            , FlexibleInstances 
            , MultiParamTypeClasses
            #-}

module Rede.SpdyProtocol.TrivialTestWorker(
    trivialWorker
    ,fsWorker
    ,FsWorkerServicePocket
    ,FsWorkerSessionPocket
    ) where


import           Control.Monad.IO.Class
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Conduit
import           Data.List                  (find, isInfixOf, isSuffixOf)
import qualified Network.URI                as U
import           System.Directory           (doesFileExist)
import           System.FilePath



import           Rede.MainLoop.ConfigHelp
import           Rede.MainLoop.Tokens       (StreamInputToken       (..)
                                             ,StreamOutputAction    (..)
                                             ,StreamWorker
                                             ,UnpackedNameValueList (..)
                                             ,StreamWorkerClass     (..)
                                             
                                             ,getHeader)
import           Rede.SimpleHTTP1Response   (shortResponse)


--
-- type StreamWorker = Conduit StreamInputToken IO StreamOutputAction



trivialWorker :: StreamWorker
trivialWorker = do 
    input_token_maybe <- await 
    case input_token_maybe of 
        Nothing     ->  do 
            return ()

        Just (Headers_STk _) -> do
            yield $ SendHeaders_SOA $ UnpackedNameValueList  [
                 (":status", "200")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length shortResponse))
                ,("content-type", "text/html")
                ,("server", "ReHv0.0")
                ]
            yield $ SendData_SOA shortResponse
            yield $ Finish_SOA



data FsWorkerSessionPocket  = FsWorkerSessionPocket {}


data FsWorkerServicePocket  = FsWorkerServicePocket {}


instance StreamWorkerClass FsWorkerServicePocket FsWorkerSessionPocket where

    --prepareStreamWorkerFactory :: base (servicePocket, sessionPocket)
    initService = return FsWorkerServicePocket

    initSession _ = return FsWorkerSessionPocket

    initStream _ _ = return fsWorker


fsWorker :: StreamWorker 
fsWorker = do 
    input_token_maybe <- await 
    www_dir <- liftIO $ wwwDir
    case input_token_maybe of 
        Nothing     ->  do 
            return ()

        Just (Headers_STk headers) -> do

            case (maybe_real_path, method) of    

                (Just uu, "GET") -> do
                    liftIO $ putStrLn $ "Got relUri= " ++ the_path
                    if  (".." `isInfixOf` the_path ) then 
                        send404 
                    else do
                        full_path <- return $ www_dir </> relativized_path
                        maybe_contents <- liftIO $ fetchFile full_path
                        case maybe_contents of 

                            Just contents -> do
                                sendResponse the_path contents

                            -- TODO: Refactor and remove duplications
                            Nothing ->  do 
                                check <- liftIO $ pathGoesToIndex full_path relativized_path
                                case check of 
                                    Just index_fname -> do
                                        Just contents2 <- liftIO $ fetchFile index_fname
                                        sendResponse index_fname contents2 

                                    Nothing -> send404
                  where 
                    the_path = U.uriPath uu
                    relativized_path = tail the_path


                _ -> send404

          where 
            (Just path)     = getHeader headers ":path"
            str_path        = unpack path
            maybe_real_path = U.parseRelativeReference str_path
            (Just method)   = getHeader headers ":method"


sendResponse :: String -> B.ByteString -> StreamWorker
sendResponse the_path contents = do 
    mimetype <- return $ getRelPathMime the_path
    yield $ SendHeaders_SOA $ UnpackedNameValueList  [
         (":status", "200")
        ,(":version", "HTTP/1.1")
        ,("content-length", (pack.show $ B.length contents))
        ,("content-type",  mimetype)

        -- TODO here: set the no-cache headers .... 

        ,("server", "ReHv0.0")
        ]
    yield $ SendData_SOA contents
    yield $ Finish_SOA



pathGoesToIndex :: String -> String -> IO (Maybe String)
pathGoesToIndex abs_path relpath = do 
    if perhaps then do
        b <- doesFileExist index_html
        if b then 
            return $ Just index_html 
        else
            return Nothing
    else
        return Nothing
  where
    perhaps = "/" `isSuffixOf` abs_path || relpath == ""
    index_html = if relpath == "" then
        abs_path ++ "/index.html"
    else 
        abs_path ++ "index.html"


fetchFile :: String ->  IO (Maybe B.ByteString)
fetchFile full_path  = do
    exists <- doesFileExist full_path
    if exists then
      do 
        contents <- B.readFile full_path
        return $ Just contents
    else 
      do 
        return $ Nothing
 

bad404ResponseData :: B.ByteString 
bad404ResponseData = "404: ReH: Didn't find that"


bad404ResponseHeaders ::  UnpackedNameValueList 
bad404ResponseHeaders =  UnpackedNameValueList [
                 (":status", "404")
                ,(":version", "HTTP/1.1")
                ,("content-length", (pack.show $ B.length bad404ResponseData))
                ,("content-type", "text/plain")
                ,("server", "ReHv0.0")
                ]


suffixToMimeTypes :: [ (B.ByteString, B.ByteString) ]
suffixToMimeTypes = [
    (".js", "application/x-javascript")
    ,(".html", "text/html")
    ,(".css", "text/css")
    ,(".svg", "image/svg+xml")
    ,(".json", "application/json")
    ,(".txt", "text/plain")
    ]


getRelPathMime :: String -> B.ByteString
getRelPathMime  rel_path = case maybe_mime of 
    Just (_, mime_type) -> mime_type
    Nothing             -> "application/octet-stream" 
  where 
    maybe_mime  = find (\ (ext, _) -> ext `B.isSuffixOf` rel_path_bs ) suffixToMimeTypes
    rel_path_bs = pack rel_path


send404 :: ConduitM StreamInputToken StreamOutputAction IO ()
send404 = do 
    yield $ SendHeaders_SOA bad404ResponseHeaders
    yield $ SendData_SOA bad404ResponseData
    yield $ Finish_SOA


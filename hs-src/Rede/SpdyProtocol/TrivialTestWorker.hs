{-# LANGUAGE OverloadedStrings #-}

module Rede.SpdyProtocol.TrivialTestWorker(
    trivialWorker
    ,fsWorker
    ) where


import qualified Data.ByteString          as B
import           Data.Conduit
import           Control.Monad.IO.Class
import           Data.ByteString.Char8    (pack, unpack)
import           Data.List(isInfixOf, find)
import qualified Network.URI as U
import           System.Directory (doesFileExist)
import           System.FilePath



import           Rede.MainLoop.Tokens     (StreamInputToken (..),
                                           StreamOutputAction (..),
                                           StreamWorker,
                                           UnpackedNameValueList (..),
                                           getHeader)
import           Rede.MainLoop.ConfigHelp
import           Rede.SimpleHTTP1Response (shortResponse)


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
                    maybe_contents <- liftIO $ fetchFile www_dir relativized_path
                    case maybe_contents of 
                        Just contents -> do
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


fetchFile :: String -> String ->  IO (Maybe B.ByteString)
fetchFile root_dir rel_path  = do
    putStrLn full_path
    -- Very primitive sanitation of rel_path
    if ".." `isInfixOf` rel_path then 
        return Nothing 
    else 
        do 
            exists <- doesFileExist full_path
            if exists then
              do 
                contents <- B.readFile full_path
                putStrLn "EXISTS"
                return $ Just contents
            else 
              do 
                putStrLn "Notexists"
                return $ Nothing
  where 
    full_path = root_dir </> rel_path


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


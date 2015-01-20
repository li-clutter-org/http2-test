{-# LANGUAGE OverloadedStrings #-}

module Rede.SpdyProtocol.TrivialTestWorker(
    trivialWorker
    ) where


import Data.Conduit
import qualified Data.ByteString as B
-- import           Control.Monad.IO.Class
import Data.ByteString.Char8(pack)

import Rede.MainLoop.Tokens     (StreamWorker
                                 ,StreamInputToken      (..)
                                 ,StreamOutputAction    (..)
                                 ,UnpackedNameValueList (..)
                                )

import Rede.SimpleHTTP1Response (shortResponse)


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
            -- liftIO $ putStrLn $ "Shit sent"



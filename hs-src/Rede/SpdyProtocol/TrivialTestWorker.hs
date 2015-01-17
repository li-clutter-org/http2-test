{-# LANGUAGE OverloadedStrings #-}

module Rede.SpdyProtocol.TrivialTestWorker(
    trivialWorker
    ) where


import Data.Conduit
import qualified Data.ByteString as B
import Data.ByteString.Char8(pack)

import Rede.MainLoop.Tokens     (StreamWorker
                                 ,StreamInputToken      (..)
                                 ,StreamOutputAction    (..)
                                 ,UnpackedNameValueList (..)
                                )

import Rede.SimpleHTTP1Response (exampleHTTP11Response)


trivialWorker :: StreamWorker
trivialWorker = do 
    input_token_maybe <- await 
    case input_token_maybe of 
        Nothing     ->  do 
            return ()

        Just (Headers_STk _) -> do
            yield $ SendHeaders_SOA $ UnpackedNameValueList  [
                ("content-length", (pack.show $ B.length exampleHTTP11Response))
                ,("content-type", "text/html")
                ,("server", "ReH v 0.0")
                ]
            yield $ SendData_SOA exampleHTTP11Response



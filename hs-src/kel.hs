
{-# LANGUAGE OverloadedStrings #-}

import Rede.MainLoop.Tls(
    tlsServe
    )

import qualified Data.ByteString.Lazy         as BL

import           Rede.SimpleHTTP1Response (exampleHTTP11Response)
import           Rede.MainLoop.PushPullType


main :: IO ()
main = tlsServe httpAttendant "127.0.0.1" 1060


httpAttendant :: PushAction -> PullAction -> IO ()
httpAttendant push _ = 
    push $ BL.fromChunks [exampleHTTP11Response]


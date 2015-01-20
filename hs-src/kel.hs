
{-# LANGUAGE OverloadedStrings #-}

import Rede.MainLoop.Tls(
    tlsServeProtocols
    )

import qualified Data.ByteString.Lazy         as BL

import           Rede.SimpleHTTP1Response (exampleHTTP11Response)
import           Rede.MainLoop.PushPullType
import           Rede.MainLoop.Conduit
import           Rede.SpdyProtocol.Session(basicSession)
-- import           Rede.Test(dotests)


main :: IO ()
main = do
	tlsServeProtocols [ 
		("spdy/3.1",spdyAttendant)
		,("http/1.1",httpAttendant) 
		] "127.0.0.1" 1060


-- The "PushAction" is a callback that can pull bytes from 
-- some medium (a network socket, for example), while the 
-- PullAction is the opposite. Here I'm saying this: give me two 
-- callbacks for getting and sending data, and I will take care of 
-- the rest.
httpAttendant :: PushAction -> PullAction -> IO ()
httpAttendant push _ = 
    push $ BL.fromChunks [exampleHTTP11Response]


spdyAttendant :: PushAction -> PullAction -> IO () 
spdyAttendant = activateSessionManager 
	id
	basicSession
 
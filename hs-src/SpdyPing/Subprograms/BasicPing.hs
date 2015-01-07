{-# LANGUAGE OverloadedStrings #-}

module SpdyPing.Subprograms.BasicPing(
	basicPingProgram
	) where


import Pipes
import qualified Pipes.Core as PC
import SpdyPing.Framing.AnyFrame
import qualified System.Clock as SC
import Text.Printf(printf)
import SpdyPing.Framing.Ping
import qualified Data.ByteString as B
import SpdyPing.MainLoop
import SpdyPing.Utils


-- The ping operation I have been building
frameTranslator1 :: PC.Pipe AnyFrame AnyFrame IO ()
frameTranslator1 = 
  do 
    -- Get the time 
    ts0 <- lift $ SC.getTime SC.Monotonic
    -- Get the first frame and print it
    f1 <- await
    -- Time it 
    t1 <- lift $ getTimeDiff ts0
    lift $ printf "t1: %0.4f -- \n" t1
    lift $ putStrLn $ show f1 
    -- Send a ping request
    t2 <- lift $ getTimeDiff ts0
    lift $ printf "t2: %0.4f -- \n" t2
    yield $ AnyControl_AF $ PingFrame_ACF $ pingFrame 1
    -- Get the second frame and print it 
    f2 <- await
    lift $ putStrLn $ show f2
    t3 <- lift $ getTimeDiff ts0
    lift $ printf "t3: %0.4f -- \n" t3
    f3 <- await
    lift $ putStrLn $ show f3
    t4 <- lift $ getTimeDiff ts0
    lift $ printf "t4: %0.4f -- \n" t4


frameTranslator1_Pipe :: IO B.ByteString  -> (B.ByteString -> IO () ) -> PC.Effect IO ()
frameTranslator1_Pipe gen pushToWire =  
    (chunkProducer gen "") >-> inputToFrames >-> frameTranslator1 >-> framesToOutput >-> (outputConsumer pushToWire)
 

basicPingProgram :: IO B.ByteString  -> (B.ByteString -> IO () ) -> IO ()
basicPingProgram a b = runEffect (frameTranslator1_Pipe a b)

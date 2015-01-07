{-# LANGUAGE OverloadedStrings #-}
module SpdyPing.MainLoop (showFrames, framesTranslator1_Program) where 

-- import           Control.Monad          (unless)
-- import           Data.Binary.Get        (runGet)
import           Data.Binary.Put        (runPut)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Pipes
import qualified Pipes.Core             as PC
-- import qualified Pipes.Prelude          as P
import qualified System.Clock as        SC
import Text.Printf(printf)

import           SpdyPing.Framing.AnyFrame (AnyFrame(..)
                                           ,AnyControlFrame(..)
                                           -- , PerfunctoryClassif
                                           , lengthFromPerfunct
                                           , perfunctoryClassify
                                           , readFrame
                                           , writeFrame)
import          SpdyPing.Framing.Ping
-- import           SpdyPing.Framing.Frame


chunkProducer :: IO B.ByteString    -- Generator
      -> LB.ByteString              -- Left-overs
      -> PC.Producer LB.ByteString IO ()
chunkProducer gen leftovers = do 
    (bytes_of_frame, new_leftovers) <- lift $ chunkProducerHelper leftovers gen Nothing
    yield bytes_of_frame
    chunkProducer gen new_leftovers


chunkProducerHelper :: LB.ByteString 
                       -> IO B.ByteString 
                       -> Maybe Int -- Length to read
                       -> IO (LB.ByteString, LB.ByteString)  -- To yield, left-overs...
chunkProducerHelper pieces gen Nothing = 
    let 
        lazy = pieces 
        length_lazy = LB.length lazy
        perfunctory_classif = perfunctoryClassify lazy
        total_length = lengthFromPerfunct perfunctory_classif
    in if length_lazy >= 8 then
          chunkProducerHelper pieces gen (Just total_length)
       else do 
          new_piece <- gen 
          new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
          chunkProducerHelper new_lazy gen Nothing 
chunkProducerHelper lazy gen j@(Just length_to_read) =
    let 
        length_lazy = LB.length lazy
        l64 = fromIntegral length_to_read
    in if length_lazy >= l64 then
            return $ LB.splitAt l64 lazy
        else do 
            new_piece <- gen 
            new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
            chunkProducerHelper new_lazy gen j


-- Now let's define a pipe that converts ByteString representations of frames 
-- to AnyFrame
inputToFrames :: PC.Pipe LB.ByteString AnyFrame IO ()
inputToFrames = 
  do
    the_bytes <- await
    perfunct_classif <- return $ perfunctoryClassify the_bytes
    the_frame  <- return $ readFrame the_bytes perfunct_classif 
    yield the_frame 
    inputToFrames


framesToOutput :: PC.Pipe AnyFrame LB.ByteString IO ()
framesToOutput =
  do 
    the_frame <- await
    the_bytes <- return $ runPut $ writeFrame the_frame
    yield the_bytes
    framesToOutput


outputConsumer :: (B.ByteString -> IO () ) -> PC.Consumer LB.ByteString IO ()
outputConsumer pushToWire = 
  do
    lazy_bytes <- await 
    lift $ pushToWire $ LB.toStrict lazy_bytes
    outputConsumer pushToWire


-- For debugging
showFrames :: IO B.ByteString -> IO ()
showFrames gen = runEffect $ framesPrint gen


framesPrint :: IO B.ByteString -> PC.Effect IO ()
framesPrint gen = 
    for framesProducer $ \ frame -> do 
        lift $ putStrLn $ show frame
  where 
    framesProducer = (chunkProducer gen "") >-> inputToFrames


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
 

framesTranslator1_Program :: IO B.ByteString  -> (B.ByteString -> IO () ) -> IO ()
framesTranslator1_Program a b = runEffect (frameTranslator1_Pipe a b)


getTimeDiff :: SC.TimeSpec -> IO Double
getTimeDiff ts0 = do 
    ts1 <- SC.getTime SC.Monotonic
    return $ (timeAsDouble ts1) - (timeAsDouble ts0)


timeAsDouble :: SC.TimeSpec -> Double 
timeAsDouble t = 
    (fromIntegral (SC.sec t)) + (fromIntegral (SC.nsec t))/1.0e9
{-# LANGUAGE OverloadedStrings #-}
module SpdyPing.MainLoop.Pipes (
    showFrames
    ,inputToFrames
    ,framesToOutput
    ,outputConsumer
    ,chunkProducer) where 

-- import           Control.Monad          (unless)
-- import           Data.Binary.Get        (runGet)
import           Data.Binary.Put        (runPut)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           Pipes
import qualified Pipes.Core             as PC
-- import qualified Pipes.Prelude          as P

import           SpdyPing.Framing.AnyFrame (AnyFrame(..)
                                           , lengthFromPerfunct
                                           , perfunctoryClassify
                                           , readFrame
                                           , writeFrame)


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


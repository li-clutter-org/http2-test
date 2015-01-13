module SpdyPing.MainLoop.Conduit where 


import SpdyPing.Framing.AnyFrame(AnyFrame(..))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Binary.Put        (runPut)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB
import           SpdyPing.MainLoop.Common (chunkProducerHelper)


data ErrorCondition = 
	Generic_EC


data InputEvent = 
	Frame_IE AnyFrame
	| Error_IE ErrorCondition


data OutputDecision =
	Frame_OD AnyFrame
	| Error_IE ErrorCondition


type FrameConduit = C.Conduit InputEvent IO OutputDecision


chunkProducer :: IO B.ByteString    -- Generator
      -> LB.ByteString              -- Left-overs
      -> C.Source IO LB.ByteString
chunkProducer gen leftovers = do 
    (bytes_of_frame, new_leftovers) <- lift $ chunkProducerHelper leftovers gen Nothing
    yield bytes_of_frame
    chunkProducer gen new_leftovers

-- Now let's define a pipe that converts ByteString representations of frames 
-- to AnyFrame
inputToFrames :: Monad m => C.Conduit LB.ByteString m AnyFrame
inputToFrames = CL.map $ \ the_bytes -> let
	perfunct_classif = perfunctoryClassify the_bytes
  in
    readFrame the_bytes perfunct_classif 


framesToOutput :: Monad m => C.Conduit AnyFrame m LB.ByteString
framesToOutput = CL.map $ \ the_frame ->  
    runPut $ writeFrame the_frame
  

outputConsumer :: (B.ByteString -> IO () ) -> C.Sink LB.ByteString IO ()
outputConsumer pushToWire = CL.mapM_ (pushToWire . LB.toStrict)


runSimpleServer :: String                -- Certificate path 
                   ->  String            -- Network interface
                   ->  Int               -- Port 
                   ->  FrameConduit      -- Frames logic callback 
                   ->  IO ()             -- Doesn't get simpler than this
runSimpleServer  certificate_path network_interface port frame_conduit = do 
	
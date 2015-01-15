module Rede.MainLoop.Conduit (
  FrameConduit
  ,ErrorCondition(..)
  ,InputEvent(..)
  ,OutputDecision(..)
  ,runSimpleServer
  ) where 


import Rede.SpdyProtocol.Framing.AnyFrame (AnyFrame(..))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import           Data.Binary.Put          (runPut)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB
import           Rede.MainLoop.Common     (chunkProducerHelper)
import           Control.Concurrent
import qualified Control.Exception            as E
import qualified Data.ByteString.Lazy         as BL
import qualified Network.TLS                  as T
import           Rede.SimpleHTTP1Response (exampleHTTP11Response)
import           System.Exit
import           System.Posix.Signals

import           Rede.MainLoop.PushPullType


data ErrorCondition = 
	Generic_EC


data InputEvent = 
	Frame_IE AnyFrame
	| Error_IE ErrorCondition


data OutputDecision =
	Frame_OD AnyFrame
	| Error_IE ErrorCondition


type FrameConduit m = C.Conduit InputEvent m OutputDecision



chunkProducer :: Monad m => m B.ByteString    -- Generator
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
  

outputConsumer :: Monad m => (B.ByteString -> m () ) -> C.Sink LB.ByteString m ()
outputConsumer pushToWire = CL.mapM_ (pushToWire . LB.toStrict)


produceSessionManager :: MonadIO m => (m a -> IO a) ->  PushAction -> PullAction -> IO () 
produceSessionManager session_producer push pull = 
    session_producer (
      (chunkProducer (liftIO pull)) $= inputToFrames =$= _middlepiece =$= framesToOutput =$ (outputConsumer (liftIO push))
    )






          


	
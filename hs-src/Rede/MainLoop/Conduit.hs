{-# LANGUAGE OverloadedStrings, ExistentialQuantification, Rank2Types #-}


module Rede.MainLoop.Conduit ( 
  Session
  ,activateSessionManager
  ) where 


import           Control.Monad.Trans.Class(lift)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Concurrent
import qualified Data.Conduit                 as C
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.Binary.Put          (runPut)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB
import           Rede.MainLoop.Common     (chunkProducerHelper)
-- import           Control.Concurrent
-- import qualified Control.Exception            as E
-- import qualified Data.ByteString.Lazy         as BL
-- import qualified Network.TLS                  as T
-- import           System.Exit
-- import           System.Posix.Signals

import           Rede.MainLoop.PushPullType
import           Rede.SpdyProtocol.Framing.AnyFrame


--  Generalized Session type....
--  frames to engine      ---v  frames from engine---v
type SessionM m = m ( (C.Sink AnyFrame m () ), (C.Source m AnyFrame ) )

--  Concrete, good-for-now session type ....
type Session = SessionM IO


chunkProducer :: Monad m => m B.ByteString    -- Generator
      -> LB.ByteString              -- Left-overs
      -> C.Source m LB.ByteString
chunkProducer gen leftovers = do 
    (bytes_of_frame, new_leftovers) <- lift $ chunkProducerHelper leftovers gen Nothing
    C.yield bytes_of_frame
    chunkProducer gen new_leftovers


-- Now let's define a pipe that converts ByteString representations of frames 
-- to AnyFrame
inputToFrames :: Monad m => C.Conduit LB.ByteString m AnyFrame
inputToFrames = CL.map $ \ the_bytes -> let
	perfunct_classif = perfunctoryClassify the_bytes
  in
    readFrame the_bytes perfunct_classif 


framesToOutput :: MonadIO m => C.Conduit AnyFrame m LB.ByteString
framesToOutput =  do  
    the_frame_maybe <- await
    case the_frame_maybe of 
      (Just the_frame)    -> do
        serialized <- return $ runPut $ writeFrame the_frame
        C.yield serialized
        framesToOutput
      Nothing             -> return ()
  

outputConsumer :: Monad m => (LB.ByteString -> m () ) -> C.Sink LB.ByteString m ()
outputConsumer pushToWire = CL.mapM_ pushToWire


-- | Creates the session source and the session sink. The session source is where 
--   output frames appear, and the session sink is where input frames dissapear (this, 
--   is is the entrance to the session manager). The session sink runs in its own 
--   thread, while the session source runs in this thread. This manager exits when 
--   the session source exits.
activateSessionManager :: MonadIO m =>  (forall a . m a -> IO a) -> SessionM m ->  PushAction -> PullAction -> IO () 
activateSessionManager session_start session push pull = let
    input_to_session  = (chunkProducer (liftIO pull) "") $= inputToFrames
    output_to_session = framesToOutput =$ (outputConsumer (liftIO . push))

  in session_start $ do
    (session_sink, session_source) <- session
    liftIO $ forkIO $ session_start $ (input_to_session $$ session_sink)
    -- This thread itself will take care of the outputs...
    liftIO $ putStrLn "Taking care of the outputs"
    liftIO $ session_start $ (session_source $$ output_to_session) 
    return ()







          


	
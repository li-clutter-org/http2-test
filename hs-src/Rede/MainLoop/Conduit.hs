{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}


module Rede.MainLoop.Conduit (
  Session
  ,activateSessionManager
  ) where 


import           Control.Monad.Trans.Class(lift)
import           Control.Monad.IO.Class(MonadIO, liftIO)
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

type Session m = C.Conduit AnyFrame m AnyFrame 


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


framesToOutput :: Monad m => C.Conduit AnyFrame m LB.ByteString
framesToOutput = CL.map $ \ the_frame ->  
    runPut $ writeFrame the_frame
  

outputConsumer :: Monad m => (LB.ByteString -> m () ) -> C.Sink LB.ByteString m ()
outputConsumer pushToWire = CL.mapM_ pushToWire


activateSessionManager :: MonadIO m => (  m () -> IO () ) -> Session m ->  PushAction -> PullAction -> IO () 
activateSessionManager session_start session push pull = 
    session_start
      (
        (chunkProducer (liftIO pull) "") $= inputToFrames =$= session =$= framesToOutput $$ (outputConsumer (liftIO . push))
      )






          


	
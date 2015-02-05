{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Rede.SpdyProtocol.Conduit() where 


-- import Rede.


import qualified Data.Conduit                 as C
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import Rede.MainLoop.Conduit(FramesInterface(..))
import           Rede.SpdyProtocol.Framing.AnyFrame
import           Data.Binary.Put              (runPut)



import Rede.SpdyProtocol.Framing.AnyFrame( AnyFrame )


-- instance FramesInterface IO AnyFrame where 
-- 	-- Now let's define a pipe that converts ByteString representations of frames 
-- 	-- to AnyFrame
-- 	-- inputToFrames :: C.Conduit LB.ByteString IO AnyFrame
-- 	inputToFrames = CL.map $ \ the_bytes -> let
-- 		perfunct_classif = perfunctoryClassify the_bytes
-- 	  in
-- 	    readFrame the_bytes perfunct_classif 


-- 	-- framesToOutput :: MonadIO m => C.Conduit AnyFrame IO LB.ByteString
-- 	framesToOutput =  do  
-- 	    the_frame_maybe <- await
-- 	    case the_frame_maybe of 
-- 	      (Just the_frame)    -> do
-- 	        serialized <- return $ runPut $ writeFrame the_frame
-- 	        C.yield serialized
-- 	        framesToOutput
-- 	      Nothing             -> return ()


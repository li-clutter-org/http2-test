module SpdyPing.Framing.Ping(PingFrame, pingFrame, getPingFrame) where 

import SpdyPing.Framing.Frame
import Data.BitSet.Generic(empty)
import           Data.Binary         (Binary,  get, put, Get)
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord32be)
import           Data.Binary.Get     (getWord32be)


data PingFrameValidFlags = None_F
	deriving (Show, Enum)

data PingFrame = 
	PingFrame {
		prologue:: ControlFrame PingFrameValidFlags 
		, frameId:: Int 
	}
	deriving Show

instance Binary PingFrame where 
	put PingFrame{prologue=pr, frameId=fi} = 
	  do 
		put pr
		putWord32be $ fromIntegral fi

	get = 
	  do
	  	pr <- get 
	  	w32 <- getWord32be
	  	return $ PingFrame pr $ fromIntegral w32


getPingFrame :: Get PingFrame
getPingFrame = get


instance Measurable PingFrame where
	measure x = measure $ prologue x


pingFrame :: Int -> PingFrame
pingFrame  frame_id = 
  PingFrame 
    (ControlFrame Ping_CFT empty 4)
    frame_id
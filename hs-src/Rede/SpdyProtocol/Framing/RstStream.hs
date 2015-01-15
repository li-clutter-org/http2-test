
module Rede.SpdyProtocol.Framing.RstStream (
	RstStreamFrame
	,RstStreamValidFlags
	,getRstStreamFrame
	,rstStreamFrame) where 


import Rede.SpdyProtocol.Framing.Frame
import Data.BitSet.Generic(empty)
import           Data.Binary         (Binary,  get, put, Get)
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord32be)
import           Data.Binary.Get     (getWord32be)


data RstStreamValidFlags = None_F
	deriving (Show, Enum)

data RstStreamFrame = 
	RstStreamFrame {
		prologue:: ControlFrame RstStreamValidFlags 
		, streamId:: Int 
		, statusCode:: Int
	}
	deriving Show

instance Binary RstStreamFrame where 
	put RstStreamFrame{prologue=pr, streamId=fi, statusCode=s} = 
	  do 
		put pr
		putWord32be $ fromIntegral fi
		putWord32be $ fromIntegral s

	get = 
	  do
	  	pr <- get 
	  	w32 <- getWord32be
	  	s <- getWord32be
	  	return $ RstStreamFrame pr (fromIntegral w32) (fromIntegral s)


getRstStreamFrame :: Get RstStreamFrame
getRstStreamFrame = get


instance Measurable RstStreamFrame where
	measure x = measure $ prologue x


rstStreamFrame :: Int -> Int -> RstStreamFrame
rstStreamFrame  stream_id status_code = 
  RstStreamFrame 
    (ControlFrame RstStream_CFT empty 8)
    stream_id status_code

module SpdyPing.Framing.AnyFrame(
	AnyControlFrame(..)
	,readFrame
	) where 

import           SpdyPing.Framing.Frame
import           SpdyPing.Framing.Ping
import           SpdyPing.Framing.RstStream
import           SpdyPing.Framing.Settings

import           Data.Binary            (Binary,  get)
import           Data.Binary.Get        (runGet)
-- import           Data.Binary.Put        (runPut)
-- import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB



data AnyControlFrame =
	PingFrame_ACF PingFrame
	|RstStreamFrame_ACF RstStreamFrame
	|SettingsFrame_ACF SettingsFrame
	deriving Show


readFrame :: LB.ByteString -> AnyControlFrame
readFrame bs = 
    case frame_type of 
    	Ping_CFT      ->  PingFrame_ACF $ extract bs 
    	RstStream_CFT ->  RstStreamFrame_ACF $ extract bs
    	Settings_CFT  ->  SettingsFrame_ACF $ extract bs
  where 
  	control_frame = runGet getControlFrame bs 
  	frame_type = cfType control_frame


extract :: Binary a => LB.ByteString -> a 
extract bs = runGet get bs 

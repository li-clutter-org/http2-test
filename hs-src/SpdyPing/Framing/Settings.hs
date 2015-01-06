
module SpdyPing.Framing.Settings (
	SettingsFrame
	,SettingsValidFlags
	,getSettingsFrame
	-- ,settingsFrame
	) where 


import SpdyPing.Framing.Frame
-- import Data.BitSet.Generic(empty)
import           Data.Binary         (Binary,  get, put, Get)
-- import           Data.Binary.Builder (Builder)
-- import           Data.Binary.Put     (putWord32be)
-- import           Data.Binary.Get     (getWord32be)



data SettingsValidFlags = Clear_F
	deriving (Show, Enum)

data SettingsFrame = 
	SettingsFrame {
		prologue:: ControlFrame SettingsValidFlags 
		-- , streamId:: Int 
		-- , statusCode:: Int
	}
	deriving Show

instance Binary SettingsFrame where 
	put SettingsFrame{prologue=pr} = 
	  do 
		put pr
		-- putWord32be $ fromIntegral fi
		-- putWord32be $ fromIntegral s

	get = 
	  do
	  	pr <- get 
	  	-- w32 <- getWord32be
	  	-- s <- getWord32be
	  	return $ SettingsFrame pr -- (fromIntegral w32) (fromIntegral s)


getSettingsFrame :: Get SettingsFrame
getSettingsFrame = get


instance Measurable SettingsFrame where
	measure x = measure $ prologue x


-- settingsFrame :: Int -> Int -> SettingsFrame
-- settingsFrame  stream_id status_code = 
--   SettingsFrame 
--     (ControlFrame Settings_CFT empty 8)
--     stream_id status_code
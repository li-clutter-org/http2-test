{-# LANGUAGE StandaloneDeriving #-}
module Rede.SpdyProtocol.Framing.Settings (
    SettingsFrame
    ,SettingsValidFlags
    ,getSettingsFrame
    -- ,settingsFrame
    ) where 


import Rede.SpdyProtocol.Framing.Frame
import Data.Word(Word32)
-- import Data.BitSet.Generic(empty)
import Control.Monad
import           Data.Binary         (Binary,  get, put, Get, putWord8, getWord8)
import Rede.Utils
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord32be)
import           Data.Binary.Get     (getWord32be)



data SettingsValidFlags =
     Null_SVF 
    |Clear_SVF
    deriving (Show, Enum)


data SettingsFrame =
    SettingsFrame {
        prologue:: ControlFrame SettingsValidFlags
        ,persistSettings:: [SettingItem]
    }
    deriving Show


data SettingsSettingsId =
    Null_S 
    |UploadBandwith_S
    |DownloadBandwith_S
    |RoundTripTime_S
    |MaxConcurrentStreams_S
    |CurrentCwnd_S
    |DownloadRetransRate_S
    |InitialWindowSize_S
    |ClientCertificateVectorSize_S
    deriving (Enum,Show)


type SettingItem = (SettingsSettingsId, Word32, PersistSettings) 


data PersistSettings = 
    None_PS
    |PersistValue_PS
    |PersistedValue_PS
    deriving (Enum,Show)


instance Binary SettingsFrame where 
    put SettingsFrame{prologue=pr, persistSettings=psh} = 
      do 
        let 
          entry_count = length psh 
          -- Create a different prologue with a correct size, 
          -- since we are not sure this one is...
          frame_size = 8*entry_count + 4
          new_pr = resetControlFrameSize pr frame_size
        put new_pr
        putWord32be $ fromIntegral entry_count
        forM_  psh $ \ (key,value, persist) -> 
          do 
            let 
              persist_setting = fromEnum persist
            putWord8 $ fromIntegral persist_setting
            putWord24be $ fromEnum key 
            putWord32be $ fromIntegral value


    get = 
      do
        pr <- get 
        entry_count <- getWord32be
        settings_themselves <- replicateM (fromIntegral entry_count) $ do 
          persist_setting_w8 <- getWord8
          key_24 <- getWord24be 
          value  <- getWord32be
          return (
            (toEnum key_24),
            value,
            (toEnum $ fromIntegral persist_setting_w8) 
            )
        return $ SettingsFrame pr settings_themselves


getSettingsFrame :: Get SettingsFrame
getSettingsFrame = get



-- settingsFrame :: Int -> Int -> SettingsFrame
-- settingsFrame  stream_id status_code = 
--   SettingsFrame 
--     (ControlFrame Settings_CFT empty 8)
--     stream_id status_code
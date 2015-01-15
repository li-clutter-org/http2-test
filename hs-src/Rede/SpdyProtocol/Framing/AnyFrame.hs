{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Rede.SpdyProtocol.Framing.AnyFrame(
  AnyControlFrame(..)
  ,readControlFrame
  ,perfunctoryClassify
  ,PerfunctoryClassif
  ,readFrame
  ,lengthFromPerfunct
  ,AnyFrame(..)
  ,writeFrame
  ) where 

import           Rede.SpdyProtocol.Framing.Frame
import           Rede.SpdyProtocol.Framing.Ping
import           Rede.SpdyProtocol.Framing.RstStream
import           Rede.SpdyProtocol.Framing.Settings
import           Rede.SpdyProtocol.Framing.DataFrame
import           Rede.SpdyProtocol.Framing.WindowUpdate
import           Rede.SpdyProtocol.Framing.SynReply
import           Rede.SpdyProtocol.Framing.SynStream
import           Rede.SpdyProtocol.Framing.Headers
import           Rede.SpdyProtocol.Framing.GoAway
import           Rede.Utils( getWord24be )
-- import qualified GHC.Generics as E

import           Data.Binary            (Binary,  get, put, Put)
import           Data.Binary.Get        (runGet)
import qualified Data.Binary.Get as G
-- import           Data.Binary.Put        (Put)
-- import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as LB


data AnyFrame = 
  AnyControl_AF AnyControlFrame 
  |DataFrame_AF DataFrame 
  deriving Show


data AnyControlFrame =
  PingFrame_ACF PingFrame
  |SynStream_ACF SynStreamFrame
  |SynReplyFrame_ACF SynReplyFrame
  |RstStreamFrame_ACF RstStreamFrame
  |SettingsFrame_ACF SettingsFrame
  |WindowUpdateFrame_ACF WindowUpdateFrame
  |GoAwayFrame_ACF GoAwayFrame
  |HeadersFrame_ACF HeadersFrame
  |Ignored_ACF LB.ByteString
  deriving (Show)


readControlFrame :: LB.ByteString -> AnyControlFrame
readControlFrame bs = 
    case frame_type of 
      Ping_CFT          ->  PingFrame_ACF $ extract bs 
      RstStream_CFT     ->  RstStreamFrame_ACF $ extract bs
      Settings_CFT      ->  SettingsFrame_ACF $ extract bs
      SynStream_CFT     ->  SynStream_ACF $ extract bs
      SynReply_CFT      ->  SynReplyFrame_ACF $ extract bs
      GoAway_CFT        -> GoAwayFrame_ACF $ extract bs
      Headers_CFT       -> HeadersFrame_ACF $ extract bs
      WindowUpdate_CFT  ->  WindowUpdateFrame_ACF $ extract bs
      _                 ->  Ignored_ACF bs
  where 
    control_frame = runGet getControlFrame bs 
    frame_type    = cfType control_frame


data PerfunctoryClassif = 
   ControlFrame_PC Int
  |DataFrame_PC Int


lengthFromPerfunct :: PerfunctoryClassif -> Int 
lengthFromPerfunct (ControlFrame_PC x) = x 
lengthFromPerfunct (DataFrame_PC x)    = x


perfunctoryClassify :: LB.ByteString -> PerfunctoryClassif
perfunctoryClassify bs = 
    runGet scanning bs   
  where
    scanning = do 
      first_byte <- G.getWord8
      if first_byte >= 128 then 
        -- Control frame
        do 
          getWord24be
          G.getWord8
          payload_length <- getWord24be 
          return $ ControlFrame_PC (payload_length + 8)
      else 
        do 
          getWord24be
          G.getWord8
          payload_length <- getWord24be 
          return $ DataFrame_PC (payload_length + 8)


readFrame :: LB.ByteString -> PerfunctoryClassif -> AnyFrame
readFrame bs (ControlFrame_PC _) = 
  AnyControl_AF (readControlFrame bs) 
readFrame bs (DataFrame_PC _) = 
  DataFrame_AF  (runGet get bs)


writeFrame :: AnyFrame -> Put  
writeFrame (AnyControl_AF acf) = writeControlFrame acf 
writeFrame (DataFrame_AF  acf) = writeDataFrame acf 


writeControlFrame :: AnyControlFrame -> Put 
writeControlFrame (PingFrame_ACF a)         = put a
writeControlFrame (RstStreamFrame_ACF a)    = put a
writeControlFrame (SettingsFrame_ACF a)     = put a
writeControlFrame (WindowUpdateFrame_ACF a) = put a
writeControlFrame (SynStream_ACF a)         = put a
writeControlFrame (SynReplyFrame_ACF a)     = put a



writeDataFrame :: DataFrame -> Put 
writeDataFrame df = put df


extract :: Binary a => LB.ByteString -> a 
extract bs = runGet get bs 

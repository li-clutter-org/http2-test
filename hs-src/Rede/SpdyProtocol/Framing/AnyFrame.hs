{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Rede.SpdyProtocol.Framing.AnyFrame(
   readControlFrame
  ,perfunctoryClassify
  ,readFrame
  ,writeFrame
  ,lengthFromPerfunct
  ,streamIdFromAnyFrame
  
  ,PerfunctoryClassif
  ,AnyControlFrame       (..)
  ,AnyFrame              (..)
  ,WrapControlFrame      (..)

  -- These can lead to ambiguity...
  ,ControlFrame         
  ,PingFrame          
  ,RstStreamFrame     
  ,SettingsFrame      
  ,DataFrame          
  ,WindowUpdateFrame  
  ,SynReplyFrame      
  ,SynStreamFrame     
  ,HeadersFrame       
  ,GoAwayFrame        
  ) where 

import           Data.Binary             (Binary,  get, put, Put)
import           Data.Binary.Get         (runGet)
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as LB

import           Rede.SpdyProtocol.Framing.Frame
import           Rede.SpdyProtocol.Framing.Ping         (PingFrame(..))
import           Rede.SpdyProtocol.Framing.RstStream    (RstStreamFrame(..))
import           Rede.SpdyProtocol.Framing.Settings     (SettingsFrame(..))
import           Rede.SpdyProtocol.Framing.DataFrame    (DataFrame(..))
import           Rede.SpdyProtocol.Framing.WindowUpdate (WindowUpdateFrame(..))
import           Rede.SpdyProtocol.Framing.SynReply     (SynReplyFrame(..))
import           Rede.SpdyProtocol.Framing.SynStream    (SynStreamFrame(..))
import           Rede.SpdyProtocol.Framing.Headers      (HeadersFrame(..))
import           Rede.SpdyProtocol.Framing.GoAway       (GoAwayFrame(..))

import           Rede.Utils              ( getWord24be )


data AnyFrame = 
  AnyControl_AF             AnyControlFrame 
  |DataFrame_AF             DataFrame 
  deriving Show


data AnyControlFrame =
  PingFrame_ACF             PingFrame
  |SynStream_ACF            SynStreamFrame
  |SynReplyFrame_ACF        SynReplyFrame
  |RstStreamFrame_ACF       RstStreamFrame
  |SettingsFrame_ACF        SettingsFrame
  |WindowUpdateFrame_ACF    WindowUpdateFrame
  |GoAwayFrame_ACF          GoAwayFrame
  |HeadersFrame_ACF         HeadersFrame
  |Ignored_ACF              LB.ByteString
  deriving (Show)


class WrapControlFrame a where 
    wrapCF :: a -> AnyFrame


instance WrapControlFrame   PingFrame where 
    wrapCF frame = AnyControl_AF $ PingFrame_ACF frame

instance WrapControlFrame  SynStreamFrame where 
    wrapCF frame = AnyControl_AF $ SynStream_ACF frame

instance WrapControlFrame  SynReplyFrame where 
    wrapCF frame = AnyControl_AF $ SynReplyFrame_ACF frame

instance WrapControlFrame  RstStreamFrame where 
    wrapCF frame = AnyControl_AF $ RstStreamFrame_ACF frame

instance WrapControlFrame  SettingsFrame where 
    wrapCF frame = AnyControl_AF $ SettingsFrame_ACF frame

instance WrapControlFrame  WindowUpdateFrame where 
    wrapCF frame = AnyControl_AF $ WindowUpdateFrame_ACF frame

instance WrapControlFrame  GoAwayFrame where 
    wrapCF frame = AnyControl_AF $ GoAwayFrame_ACF frame

instance WrapControlFrame  HeadersFrame where 
    wrapCF frame = AnyControl_AF $ HeadersFrame_ACF frame



readControlFrame :: LB.ByteString -> AnyControlFrame
readControlFrame bs = 
    case frame_type of 
      Ping_CFT          ->  PingFrame_ACF $ extract bs 
      RstStream_CFT     ->  RstStreamFrame_ACF $ extract bs
      Settings_CFT      ->  SettingsFrame_ACF $ extract bs
      SynStream_CFT     ->  SynStream_ACF $ extract bs
      SynReply_CFT      ->  SynReplyFrame_ACF $ extract bs
      GoAway_CFT        ->  GoAwayFrame_ACF $ extract bs
      Headers_CFT       ->  HeadersFrame_ACF $ extract bs
      WindowUpdate_CFT  ->  WindowUpdateFrame_ACF $ extract bs
      _                 ->  Ignored_ACF bs
  where 
    control_frame = runGet getControlFrame bs 
    frame_type    = cfType control_frame


data PerfunctoryClassif = 
    ControlFrame_PC Int
    |DataFrame_PC Int


instance FrameIsControlOrData PerfunctoryClassif where 
    isControlOrData (ControlFrame_PC _)  = FrameIsControl 
    isControlOrData (DataFrame_PC _)     = FrameIsData


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
writeControlFrame (GoAwayFrame_ACF a)       = put a
writeControlFrame (HeadersFrame_ACF a)      = put a


writeDataFrame :: DataFrame -> Put 
writeDataFrame df = put df


extract :: Binary a => LB.ByteString -> a 
extract bs = runGet get bs 


-- Returns 0 if no valid id is present... 
streamIdFromAnyFrame :: AnyFrame -> Int  
streamIdFromAnyFrame anyframe = case anyframe of 
    AnyControl_AF anycontrol    -> case anycontrol of 
        (WindowUpdateFrame_ACF frame)       ->   streamIdFromFrame frame 
        (SynStream_ACF frame)               ->   streamIdFromFrame frame 
        (SynReplyFrame_ACF frame)           ->   streamIdFromFrame frame 
        (HeadersFrame_ACF frame)            ->   streamIdFromFrame frame 
    DataFrame_AF dataframe        -> streamIdFromFrame dataframe

streamIdFromAnyFrame _ = 0
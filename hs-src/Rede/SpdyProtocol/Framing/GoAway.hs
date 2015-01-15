
module Rede.SpdyProtocol.Framing.GoAway (
	GoAwayValidFlags
	,GoAwayFrame
	) where 



import           Data.Word
import           Data.Binary                    (Binary, Get, get, put)
import           Rede.SpdyProtocol.Framing.Frame
import           Data.Binary.Get                (getWord32be)
import           Data.Binary.Put                (putWord32be)


data GoAwayValidFlags = None_GAVF    
    deriving (Show, Enum)


data GoAwayReason = OK_GAR
                   |ProtocolError_GAR
                   |InternalError_GAR
    deriving (Show, Enum)


data GoAwayFrame = GoAwayFrame {
    prologue:: ControlFrame GoAwayValidFlags
    , lastGoodStream:: Int
    , statusCode:: GoAwayReason
  } deriving Show


instance Binary GoAwayFrame where

    put (GoAwayFrame pr lgs sc) = do
        put newprologue 
        putWord32be (fromIntegral lgs::Word32 )
        putWord32be $ (fromIntegral . fromEnum ) sc
      where 
        newprologue = resetControlFrameSize pr 8

    get = do 
        pr  <- get :: Get (ControlFrame GoAwayValidFlags)
        lgi <- getWord32be 
        sc  <- getWord32be  
        return $ GoAwayFrame {
            prologue = pr
            ,lastGoodStream = fromIntegral lgi
            ,statusCode = (toEnum . fromIntegral) sc 
        }


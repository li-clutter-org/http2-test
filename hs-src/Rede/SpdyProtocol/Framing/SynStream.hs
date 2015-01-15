module Rede.SpdyProtocol.Framing.SynStream(
    SynStreamValidFlags
    ,SynStreamFrame
    ) where 


import           Data.Word
import           Data.Binary                    (Binary, Get, get, put)
import           Rede.SpdyProtocol.Framing.Frame
import           Data.Binary.Get                (getWord32be, getByteString, getWord16be)
import           Data.Binary.Put                (putWord32be, putWord16be, putByteString)
import           Rede.SpdyProtocol.Framing.KeyValueBlock (CompressedKeyValueBlock(..))
import qualified Data.ByteString as BS


data SynStreamValidFlags = None_SSVF 
                           |Fin_SSVF
                           |Unidirectional_SSVF
    deriving (Show, Enum)



data SynStreamFrame = 
  SynStreamFrame {
    prologue:: ControlFrame SynStreamValidFlags
    , streamId:: Int
    , associatedToStream:: Int 
    -- To make sense of this, we need the state of the stream...
    , compressedKeyValueBlock:: CompressedKeyValueBlock
  } deriving Show


instance Binary SynStreamFrame where

    put (SynStreamFrame pr strid assocstrid (CompressedKeyValueBlock cmpkvb)) = do
        put newprologue 
        putWord32be (fromIntegral strid::Word32 )
        putWord32be (fromIntegral assocstrid::Word32)
        putWord16be 0
        putByteString cmpkvb
      where 
        data_length = BS.length cmpkvb 
        total_data_length = 10 + data_length
        newprologue = resetControlFrameSize pr total_data_length

    get = do 
        pr <- get :: Get (ControlFrame SynStreamValidFlags)
        w32strid <- getWord32be 
        w32assocstrid <- getWord32be 
        getWord16be
        data_length <- return $ (cfLength pr) - 10  
        cmkvb <- getByteString data_length 
        return $ SynStreamFrame {
            prologue = pr
            ,streamId = fromIntegral w32strid
            ,associatedToStream = fromIntegral w32assocstrid
            ,compressedKeyValueBlock = CompressedKeyValueBlock cmkvb
        }


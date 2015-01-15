module Rede.SpdyProtocol.Framing.Headers(
	HeadersValidFlags
	,HeadersFrame
	) where 



import           Data.Word
import           Data.Binary                    (Binary, Get, get, put)
import           Rede.SpdyProtocol.Framing.Frame
import           Data.Binary.Get                (getWord32be, getByteString)
import           Data.Binary.Put                (putWord32be, putByteString)
import           Rede.SpdyProtocol.Framing.KeyValueBlock (CompressedKeyValueBlock(..))
import qualified Data.ByteString as BS


data HeadersValidFlags = None_HVF 
                         |Fin_HVF
    deriving (Show, Enum)



data HeadersFrame = 
  HeadersFrame {
    prologue:: ControlFrame HeadersValidFlags
    , streamId:: Int
    -- To make sense of this, we need the state of the stream...
    , compressedKeyValueBlock:: CompressedKeyValueBlock
  } deriving Show


instance Binary HeadersFrame where

    put (HeadersFrame pr strid (CompressedKeyValueBlock cmpkvb)) = do
        put newprologue 
        putWord32be (fromIntegral strid::Word32 )
        putByteString cmpkvb
      where 
        data_length = BS.length cmpkvb 
        total_data_length = 4 + data_length
        newprologue = resetControlFrameSize pr total_data_length

    get = do 
        pr <- get :: Get (ControlFrame HeadersValidFlags)
        w32strid <- getWord32be 
        data_length <- return $ (cfLength pr) - 4 
        cmkvb <- getByteString data_length 
        return $ HeadersFrame {
            prologue = pr
            ,streamId = fromIntegral w32strid
            ,compressedKeyValueBlock = CompressedKeyValueBlock cmkvb
        }


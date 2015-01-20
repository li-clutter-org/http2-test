module Rede.SpdyProtocol.Framing.DataFrame(
	DataFrame(..)
	,DataFrameValidFlags(..)
	) where 

import           Data.Bits
-- import Data.Enum
import           Data.Binary            (Binary, get, getWord8, put, putWord8)
import           Data.Binary.Get        (getWord32be, getByteString)
import           Data.Binary.Put        (putWord32be, putByteString)
import           Rede.SpdyProtocol.Framing.Frame (FlagsBitSet, bitsetToWord8, word8ToBitset)
import           Rede.Utils             (getWord24be, putWord24be)
import qualified Data.ByteString  as B 


data DataFrameValidFlags = Fin_F   -- Signals stream termination
	deriving (Show, Enum)


data DataFrame = 
	DataFrame {
		streamId:: Int
		, dataFrameFlags:: FlagsBitSet DataFrameValidFlags
		, payload:: B.ByteString
	}
	deriving Show


instance Binary DataFrame where 
	put (DataFrame stream_id flags bs_payload) = 
	  do 
		putWord32be $ fromIntegral $ (stream_id .&. 2147483647)
		putWord8 $ bitsetToWord8 flags
		putWord24be $ B.length bs_payload 
		putByteString bs_payload

	get = 
	  do
	  	stream_id <- getWord32be
	  	flags_word <- getWord8
	  	data_length <- getWord24be
	  	bs_payload <- getByteString data_length
	  	return $ DataFrame
	  		(fromIntegral stream_id) 
	  		(word8ToBitset flags_word)
	  		bs_payload

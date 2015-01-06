module SpdyPing.Framing.DataFrame where 

import           Data.Bits
-- import Data.Enum
import           Data.Binary            (Binary, get, getWord8, put, putWord8)
import           Data.Binary.Get        (getWord32be)
import           Data.Binary.Put        (putWord32be)
import           SpdyPing.Framing.Frame (FlagsBitSet, bitsetToWord8, word8ToBitset)
import           SpdyPing.Utils         (getWord24be, putWord24be)
import qualified Data.ByteString  as B 


data DataFrameValidFlags = None_F
	| Fin_F   -- Signals stream termination
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
		put bs_payload

	get = 
	  do
	  	stream_id <- getWord32be
	  	flags_word <- getWord8
	  	_ <- getWord24be
	  	bs_payload <- get
	  	return $ DataFrame
	  		(fromIntegral stream_id) 
	  		(word8ToBitset flags_word)
	  		bs_payload

module SpdyPing.Utils (strToInt) where 


import           Data.Binary            (Binary,  put, get, putWord8)
-- import           Data.Binary.Get        (runGet)
import           Data.Binary.Put     (putWord16be)
import           Data.Binary.Get     (getWord16be, getWord8)
import           Data.Bits
-- import           Data.Binary.Put        (runPut)
-- import qualified Data.ByteString        as B
-- import qualified Data.ByteString.Lazy   as LB


strToInt::String -> Int 
strToInt = fromIntegral . toInteger . read


newtype Word24 = Word24 Int
    deriving (Show)


instance Binary Word24 where

    put (Word24 w24) = 
        do 
          let 
            high_stuff   = w24 `shiftR` 24 
            low_stuff    = w24 `mod`  (1 `shiftL` 24) 
          putWord8 $ fromIntegral high_stuff
          putWord16be $ fromIntegral low_stuff 

    get = do
      high_stuff <- getWord8 
      low_stuff  <- getWord16be
      let 
        value = (fromIntegral low_stuff) + ( (fromIntegral high_stuff) `shiftL` 24 ) 
      return $ Word24 value
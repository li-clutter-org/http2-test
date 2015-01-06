module SpdyPing.Utils (
    strToInt,
    Word24,
    word24ToInt,
    putWord24be,
    getWord24be
    ) where 


import           Data.Binary            (Binary,  put, get, putWord8)
-- import           Data.Binary.Get        (runGet)
import           Data.Binary.Put     (putWord16be, Put)
import           Data.Binary.Get     (getWord16be, getWord8, Get)
import           Data.Bits
-- import           Data.Binary.Put        (runPut)
-- import qualified Data.ByteString        as B
-- import qualified Data.ByteString.Lazy   as LB


strToInt::String -> Int 
strToInt = fromIntegral . toInteger . read


newtype Word24 = Word24 Int
    deriving (Show)


word24ToInt :: Word24 -> Int 
word24ToInt (Word24 w24) = w24


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


getWord24be :: Get Int
getWord24be = do 
    w24 <- get
    return $ word24ToInt w24


putWord24be :: Int -> Put 
putWord24be x = put (Word24 x)
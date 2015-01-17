{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Rede.SpdyProtocol.Framing.KeyValueBlock(
    UnpackedNameValueList(..)
    ,CompressedKeyValueBlock(..)
    ) where 


import           Control.Monad                  (forM_, replicateM)
import           Data.Binary                    (Binary,  get, put)
import qualified Data.ByteString                as BS
import           Data.Word
import           Data.Binary.Get                (getByteString, getWord32be)
import           Data.Binary.Put                (putWord32be)
import           Data.List                      (sortBy)


-- Not to use raw....
newtype UnpackedNameValueList = UnpackedNameValueList [(BS.ByteString, BS.ByteString)]
    deriving Show


-- To use raw
newtype CompressedKeyValueBlock = CompressedKeyValueBlock BS.ByteString
    deriving Show

instance Binary UnpackedNameValueList where 
    put unvl = 
        do 
            putWord32be length32
            forM_ packed $ \ (h,v) -> do 
                putWord32be $ fromIntegral (BS.length h)
                put h 
                putWord32be $ fromIntegral (BS.length v)
                put v
      where 
        length32 = (fromIntegral $ length packed)::Word32 
        packed = packHeaderTuples unvl

    get = 
        do
            entry_count    <- getWord32be
            packed_entries <- replicateM  (fromIntegral entry_count) $ do { 
                name_length    <- getWord32be
                ; name         <- getByteString (fromIntegral name_length)
                ; value_length <- getWord32be 
                ; value        <- getByteString (fromIntegral value_length)
                ; return (name, value) }
            return $ unpackHeaderTuples packed_entries        
        


-- Just puts them together, as per the spec
packHeaderTuples ::  UnpackedNameValueList -> [(BS.ByteString, BS.ByteString)]
packHeaderTuples (UnpackedNameValueList uvl) = let
    sortFun (h1, _) (h2, _) = compare h1 h2
    sorted_uvl                = sortBy sortFun uvl 
    sameName []               = []
    sameName ((h, v):rest)      = let 
        (cousins,nocousins) = span (\ (hh, _) -> hh == h ) rest
        cousings_value = BS.intercalate "\0" $ v:(map snd cousins)
      in 
        (h, cousings_value):(sameName nocousins)
  in 
    sameName sorted_uvl


-- And unputs them together 
unpackHeaderTuples :: [(BS.ByteString, BS.ByteString)] -> UnpackedNameValueList
unpackHeaderTuples [] = UnpackedNameValueList []
unpackHeaderTuples vl  =    UnpackedNameValueList $ step vl
  where  
    valueSplit v = BS.split 0 v
    step [] = []
    step ((h,v):rest) = [ (h,vv) | vv <- valueSplit v ] ++ (step rest) 

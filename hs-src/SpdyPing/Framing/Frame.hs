{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module SpdyPing.Framing.Frame(
    FrameControlOrData(..),
    Frame,
    ControlFrame(..),
    ControlFrameType(..),
    FlagsBitSet,
    Measurable(..),
    cfType,
    cfLength,
    cfFlags,
    getControlFrame
    ) where 

import           Data.Binary         (Binary,  putWord8, get, put, Get)
-- import           Data.Binary.Builder (Builder)
import           Data.Binary.Put     (putWord16be)
import           Data.Binary.Get     (getWord16be, getWord8)
import           Data.Bits
import qualified Data.BitSet.Generic as GenericBitset
import qualified Data.ByteString     as B
-- import           Data.Monoid
import           Data.Word


type FlagsBitSet = GenericBitset.BitSet Word8


data FrameControlOrData = 
    FrameIsControl
    |FrameIsData
    deriving (Show,Enum)


class Frame a where 
    classify:: a -> FrameControlOrData
    classify x = 
        if first_byte .&. 128 > 0 then FrameIsControl
                                  else FrameIsData
      where first_byte = B.head $ toBytes x

    toBytes:: a -> B.ByteString


-- Computes a number of bytes
class Measurable a where 
    measure :: a -> Int 


data ControlFrameType = 
    Zero_CFT -- Not a valid control frame
    |SynStream_CFT 
    |SynReply_CFT
    |RstStream_CFT
    |Settings_CFT
    |Deprecated1_CFT
    |Ping_CFT
    |GoAway_CFT
    |Headers_CFT
    |WindowUpdate_CFT
    deriving (Show, Enum)


data ControlFrameFlags = 
    Fin_CFF
    deriving (Show, Enum)


data ControlFrame valid_flags where 
     ControlFrame ::  
        Enum valid_flags => 
        ControlFrameType 
        -> FlagsBitSet valid_flags 
        -> Int -- Length
        -> ControlFrame valid_flags 


deriving instance Show valid_flags => Show (ControlFrame valid_flags)

instance Measurable (ControlFrame a) where 
    measure cf = 8 + (cfLength cf)


cfType :: ControlFrame vf -> ControlFrameType 
cfType (ControlFrame cftype _ _) = cftype 

cfFlags :: ControlFrame vf -> FlagsBitSet vf 
cfFlags (ControlFrame _ flags _) = flags

cfLength :: ControlFrame vf -> Int 
cfLength (ControlFrame _ _ l) = l


instance Enum valid_flags => Binary (ControlFrame valid_flags) where 
    put cf = 
        do 
            let 
                frame_length = cfLength cf 
                high_stuff   = frame_length `shiftR` 24 
                low_stuff    = frame_length `mod`  (1 `shiftL` 24)    
            putWord16be $ controlSet 3 -- Set the version in the first two bytes
            putWord16be $ fromIntegral $ fromEnum $ cfType cf
            putWord8 $ foldl (.|.) 0 $ map (fromIntegral . (shiftL (1::Word8)) . fromEnum) $ GenericBitset.toList $ cfFlags cf
            putWord8 $ fromIntegral high_stuff
            putWord16be $ fromIntegral low_stuff

    get  = do 
        getWord16be 
        type_int <- getWord16be
        flags_word <- getWord8
        high_stuff <- getWord8 
        low_stuff  <- getWord16be
        return $ let
                    numbers = [0..7]
                    frame_length = (fromIntegral low_stuff) + ( (fromIntegral high_stuff) `shiftL` 24 ) 
                    sieves  = map (\ x -> 1 `shiftL` x) numbers 
                    -- actual_flags  :: [valid_flags]
                    actual_flags   = map (toEnum . fst) $ filter (\ (_, sieve) -> 
                        (sieve .&. flags_word) /= 0 )  $ zip numbers sieves
                    in ControlFrame 
                        (toEnum $ fromIntegral type_int) 
                        (GenericBitset.fromList actual_flags)
                        frame_length


getControlFrame :: Get (ControlFrame ControlFrameFlags)
getControlFrame = get

 
controlSet :: Word16 -> Word16
controlSet = (32768 .|. ) 


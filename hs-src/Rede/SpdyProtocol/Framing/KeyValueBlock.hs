{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Rede.SpdyProtocol.Framing.KeyValueBlock(
    UnpackedNameValueList(..)
    ,CompressedKeyValueBlock(..)
    ,CompressedHeadersOnFrame(..)
    ) where 


import qualified Data.ByteString                as BS

import Rede.MainLoop.Tokens                     (
                                                UnpackedNameValueList(..) )
                                                -- ,packHeaderTuples
                                                -- ,unpackHeaderTuples )

-- To use raw
newtype CompressedKeyValueBlock = CompressedKeyValueBlock BS.ByteString
    deriving Show


class CompressedHeadersOnFrame a where 
    getCompressedHeaders :: a ->  CompressedKeyValueBlock

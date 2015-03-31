{-# LANGUAGE OverloadedStrings #-}

-- The purpose of this module is to have several types of messages that 
-- can be exchanged with the outside world...

module Rede.Research.JSONMessages(
    SetNextUrl(..)

    ) where 


import           Control.Applicative
import qualified Data.ByteString       as B
-- import           Data.ByteString.Char8 (pack)


import           Data.Aeson            ()
import           Data.Aeson.Types
import           Data.Text.Encoding    (encodeUtf8)



newtype SetNextUrl = SetNextUrl B.ByteString

instance FromJSON SetNextUrl where 
    parseJSON (Object v) = SetNextUrl <$>
        (encodeUtf8 <$> v .: "url" )

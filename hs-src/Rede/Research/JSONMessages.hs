{-# LANGUAGE OverloadedStrings #-}

-- The purpose of this module is to have several types of messages that 
-- can be exchanged with the outside world...

module Rede.Research.JSONMessages(
    SetNextUrl(..),
    DnsMasqConfig(..)

    ) where 


import           Control.Applicative
import qualified Data.ByteString     as B
-- import           Data.ByteString.Char8 (pack)


import           Data.Aeson          ()
import           Data.Aeson.Types
import           Data.Text.Encoding  (encodeUtf8, decodeUtf8)
-- import           Data.Text.Decoding  (decodeUtf8)


import           Rede.Utils          (SafeUrl, unSafeUrl)



-- This is a more sophisticated message that we can 
-- receive for the web-server front-end to queue a particular
-- url. 
newtype SetNextUrl = SetNextUrl B.ByteString

instance FromJSON SetNextUrl where 
    parseJSON (Object v) = SetNextUrl <$>
        (encodeUtf8 <$> v .: "url" )


-- This is a controlled message to be sure that dnsmasq set 
-- the correct DNS file for the current item in the pipeline.
data DnsMasqConfig = DnsMasqConfig !SafeUrl !B.ByteString
    deriving (Show, Eq)

instance ToJSON DnsMasqConfig where 
	toJSON (DnsMasqConfig safe_url file_contents) = object [
		"analysis_id" .= (decodeUtf8 . unSafeUrl $ safe_url),
		"config_file" .= decodeUtf8 file_contents
		]
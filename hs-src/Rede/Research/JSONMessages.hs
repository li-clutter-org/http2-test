{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FunctionalDependencies,
             OverlappingInstances,
             FlexibleInstances,
             UndecidableInstances #-}

-- The purpose of this module is to have several types of messages that 
-- can be exchanged with the outside world...

module Rede.Research.JSONMessages(
    hashOfJob 
    ,workIndicationDatum
    ,captureTime

    ,SetNextUrl(..)
    ,DnsMasqConfig(..)
    ,WorkIndication(..)
    ) where 


import           Control.Lens.TH        (makeLenses)
import           Control.Applicative
import qualified Data.ByteString     as B
-- import           Data.ByteString.Char8 (pack)


import           Data.Aeson          ()
import           Data.Aeson.Types
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8, decodeUtf8)
-- import           Data.Text.Decoding  (decodeUtf8)

import           Rede.Research.JobId (HashId, unHashId)



-- This is a more sophisticated message that we can 
-- receive for the web-server front-end to queue a particular
-- url. 
newtype SetNextUrl = SetNextUrl B.ByteString

instance FromJSON SetNextUrl where 
    parseJSON (Object v) = SetNextUrl <$>
        (encodeUtf8 <$> v .: "url" )

-- This is a controlled message to be sure that dnsmasq set 
-- the correct DNS file for the current item in the pipeline.
data DnsMasqConfig = DnsMasqConfig !HashId !B.ByteString
    deriving (Show, Eq)

instance ToJSON DnsMasqConfig where 
	toJSON (DnsMasqConfig hashid file_contents) = object [
		"analysis_id" .= (decodeUtf8 . unHashId  $ hashid),
		"config_file" .= decodeUtf8 file_contents
		]

data WorkIndication a = WorkIndication {
    _hashOfJob               :: HashId 
    ,_workIndicationDatum    :: a
    -- Wait time in seconds and fractions of a second.
    ,_captureTime            :: Float 
    }

makeLenses ''WorkIndication

class (ToJSON b) => ToJSONInput a b | a -> b where 
    toJSONInput :: a -> b

instance ToJSONInput B.ByteString Text where 
    toJSONInput a = decodeUtf8 a


instance (ToJSONInput a b) => ToJSON (WorkIndication a) where 
    toJSON (WorkIndication h datum ct) = object [
        "analysis_id" .= (decodeUtf8 . unHashId $ h),
        "datum"       .= (toJSONInput datum),
        "capture_time".= ct
        ]


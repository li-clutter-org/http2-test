{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.HarFiles.DataStructure(
     entries           
    ,pages            
    ,browser          
    ,version          
    ,creator
    ,headerName 
    ,headerValue 
    ,status 
    ,content 
    ,respHeaders 
    ,method 
    ,reqHeaders
    ,queryString 
    ,reqUrl 
    ,onContentLoad 
    ,comment
    ,onLoad 
    ,vpName 
    ,vpVersion
    ) where 


import           Control.Applicative
import           Control.Monad
import qualified Control.Lens        as L
import           Control.Lens.TH     (makeLenses)

import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.ByteString.Char8 (pack)



-- Which kind of string?
type HereString = ByteString


data Har_Log = Har_Log {
    _entries           :: [Har_Entry]
    ,_pages            :: [Har_Page]
    ,_browser          :: Har_VersionPair
    ,_version          :: HereString 
    ,_creator          :: Har_VersionPair
   }


data Har_Header = Har_Header {
    _headerName :: HereString
    ,_headerValue :: HereString 
    }


data Har_Response = Har_Response {
    _status       :: Int 
    ,_content     :: HereString
    ,_respHeaders :: Har_Header
    }


data Har_Request = Har_Request {
    _method       :: HereString
    ,_reqHeaders  :: Har_Header
    ,_queryString :: HereString
    ,_reqUrl      :: HereString
    ,_reqBody     :: HereString
    }


data Har_Outer = Har_Outer {
    _log:: Har_Log
    }


data Har_Page = Har_Page {
    _startedDateTime   :: HereString 
    ,_pageTimings      :: Har_PageTimings
    ,_pageId           :: HereString
    ,_title            :: HereString
   }


data Har_PageTimings = Har_PageTimings {
    -- Milliseconds
    _onContentLoad     :: Int 
    ,_comment          :: HereString 
    -- Milliseconds
    ,_onLoad           :: Int 
   }


data Har_VersionPair = Har_VersionPair {
   _vpVersion          :: Int 
   ,_vpName            :: HereString
   }


-- I'm only taking some of the fields on this object....
data Har_Entry = Har_Entry {
    _request           :: Har_Request
    ,_response         :: Har_Response
   }


makeLenses ''Har_Page
makeLenses ''Har_Outer
makeLenses ''Har_Response
makeLenses ''Har_Entry 
makeLenses ''Har_Header
makeLenses ''Har_VersionPair
makeLenses ''Har_Log
makeLenses ''Har_Request
makeLenses ''Har_PageTimings


instance FromJSON Har_VersionPair where 

    parseJSON (Object v)  = Har_VersionPair <$>
        v   .: "version"               <*>
        ( pack <$>  v .: "name" )

    parseJSON _   =  mzero 


instance FromJSON Har_PageTimings where 

    parseJSON (Object v)  = Har_PageTimings <$> 
         v .: "onContentLoad"          <*>
         (pack <$> v .: "comment")     <*>
         v .: "onLoad"


instance FromJSON Har_Header where 

    parseJSON (Object v) = Har_Header <$> 
        (pack <$> v .: "name"  )       <*>
        (pack <$> v .: "value" ) 


instance FromJSON Har_Response where 

    parseJSON (Object v) = Har_Response <$>
        v .: "status"                  <*>
        (pack <$> v .: "content")      <*>
        v .: "headers"


instance FromJSON Har_Request where 

    parseJSON (Object v) = Har_Request <$> 
        (pack <$> v .: "method" )      <*>
        v .: "headers"                 <*>
        (pack <$> v .: "queryString" ) <*>
        (pack <$> v .: "url" )         <*>
        (pure "")


instance FromJSON Har_Log where 

    parseJSON (Object v) = Har_Log  <$> 
        v .: "entries"              <*>
        v .: "pages"                <*>
        v .: "browser"              <*>
        (pack <$> v .: "version")   <*>
        v .: "creator"


instance FromJSON Har_Page where 

    parseJSON (Object v) = Har_Page  <$>
        (pack <$> v .: "_startedDateTime") <*>
        v .: "pageTimings" <*>
        (pack <$> v .: "pageId") <*>
        (pack <$> v .: "title")


instance FromJSON  Har_Entry where 

    parseJSON (Object v) = Har_Entry  <$>
        v .: "request"   <*>
        v .: "response"


instance FromJSON Har_Outer where 

    parseJSON (Object v) = Har_Outer <$> 
        v .: "log"
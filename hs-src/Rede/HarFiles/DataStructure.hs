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


import           Control.Lens.TH(makeLenses)
import qualified Control.Lens as L

import Data.ByteString(ByteString)


-- Which kind of string?
type HereString = ByteString


data Har_Log = Har_Log {
    _entries           :: [Har_Entry]
    ,_pages            :: [Har_Page]
    ,_browser          :: Har_VersionPair
    ,_version          :: HereString 
    ,_creator          :: Har_VersionPair
   }


data Har_Headers = Har_Headers {
    _headerName :: HereString
    ,_headerValue :: HereString 
    }


data Har_Response = Har_Response {
    _status       :: Int 
    ,_content     :: HereString
    ,_respHeaders :: Har_Headers
    }


data Har_Request = Har_Request {
    _method       :: HereString
    ,_reqHeaders  :: Har_Headers
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
makeLenses ''Har_Headers
makeLenses ''Har_VersionPair
makeLenses ''Har_Log
makeLenses ''Har_Request
makeLenses ''Har_PageTimings
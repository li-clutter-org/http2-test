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
    ,onLoad 
    ,vpName 
    ,vpVersion
    ) where 


import           Control.Applicative
import           Control.Monad
import qualified Control.Lens        as L
import           Control.Lens        ( (^.), to )
import           Control.Lens.TH     (makeLenses)
import           Text.Printf         (printf)

import           Data.Aeson
import           Data.Aeson.Types       (Parser)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8  (pack)
import           Data.Text(Text)


-- Which kind of string?
type HereString = ByteString


data Har_Log = Har_Log {
    _entries           :: ![Har_Entry]
    ,_pages            :: ![Har_Page]
    ,_browser          :: !Har_VersionPair
    ,_version          :: !HereString 
    ,_creator          :: !Har_VersionPair
   }


data Har_Header = Har_Header {
    _headerName   :: !HereString
    ,_headerValue :: !HereString 
    }


data Har_Response = Har_Response {
    _status       :: !Int 
    ,_content     :: !Har_Content
    ,_respHeaders :: ![Har_Header]
    }


-- There are more fields hidden in this...
data Har_Content   = Har_Content {
    _contentText  :: ! (Maybe HereString)
    }


data Har_Request = Har_Request {
    _method       :: !HereString
    ,_reqHeaders  :: ![Har_Header]
    ,_queryString :: ![Har_QueryString]
    ,_reqUrl      :: !HereString
    ,_reqBody     :: !HereString
    }


data Har_Outer = Har_Outer {
    _harLog:: !Har_Log
    }


data Har_Page = Har_Page {
    _startedDateTime   :: !HereString 
    ,_pageTimings      :: !Har_PageTimings
    ,_pageId           :: !HereString
    ,_title            :: !HereString
   }


data Har_QueryString = Har_QueryString {
   _qsName               ::  !HereString 
   ,_qsValue             ::  !HereString
   }


data Har_PageTimings = Har_PageTimings {
    -- Milliseconds
    _onContentLoad     :: !Int 
    --,_comment          :: !HereString 
    -- Milliseconds
    ,_onLoad           :: !Int 
   }


data Har_VersionPair = Har_VersionPair {
   _vpVersion          :: !HereString 
   ,_vpName            :: !HereString
   }


-- I'm only taking some of the fields on this object....
data Har_Entry = Har_Entry {
    _request           :: !Har_Request
    ,_response         :: !Har_Response
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
makeLenses ''Har_QueryString
makeLenses ''Har_Content

errorParse  :: Value -> Parser a
errorParse  x = error $ printf "NotGoodHere: %s" $ show x


instance FromJSON Har_VersionPair where 

    parseJSON (Object v)  = Har_VersionPair <$>
        ( pack <$>  v .: "version" )    <*>
        ( pack <$>  v .: "name"    )

    parseJSON  x =  errorParse x


instance FromJSON Har_PageTimings where 

    parseJSON (Object v)  = Har_PageTimings <$> 
         v .: "onContentLoad"          <*>
         -- (pack <$> v .: "comment")     <*>
         v .: "onLoad"


instance FromJSON Har_Header where 

    parseJSON (Object v) = Har_Header <$> 
        (pack <$> v .: "name"  )       <*>
        (pack <$> v .: "value" )        

    parseJSON  x         = errorParse x


instance FromJSON Har_Response where 

    parseJSON (Object v) = Har_Response <$>
        v .: "status"                  <*>
        v .: "content"                 <*>
        v .: "headers"


instance FromJSON Har_Request where 

    parseJSON (Object v) = Har_Request <$> 
        (pack <$> v .: "method" )      <*>
        v .: "headers"                 <*>
        v .: "queryString"             <*>
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
        (pack <$> v .: "startedDateTime") <*>
        v .: "pageTimings" <*>
        (pack <$> v .: "id") <*>
        (pack <$> v .: "title")


instance FromJSON  Har_Entry where 

    parseJSON (Object v) = Har_Entry  <$>
        v .: "request"   <*>
        v .: "response"


instance FromJSON Har_QueryString where 

    parseJSON (Object v) = Har_QueryString <$>
        (pack <$> v .: "name" )     <*>
        (pack <$> v .: "value")


instance FromJSON Har_Content where 
    parseJSON (Object v) = Har_Content <$>
        ( (liftA pack) <$> v .:? "text" )


instance FromJSON Har_Outer where 

    parseJSON (Object v) = Har_Outer <$> 
        v .: "log"


------ Some small functions to try this out 
test_01 :: IO ()
test_01 = do 
    file_contents <- LB.readFile "tests/bitbucket.org.har"
    case (eitherDecode file_contents :: Either String Har_Outer ) of 
    
        Right doc_model -> do 
            putStrLn $ printf "File has %d entries" $ doc_model ^. harLog . entries . to length

        Left  msg       -> do 
            putStrLn $ printf "Apparently parsing failed: %s" msg
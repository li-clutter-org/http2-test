{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.HarFiles.DataStructure(

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
import qualified Data.Map.Strict        as M


import           Rede.MainLoop.Tokens   (UnpackedNameValueList)



-- How do we identify resources that are being 
-- asked for? Plain ByteString? Wecan start this way...
-- if query-string reordering is an issue, we can supersede
-- this type somehow... 
newtype ResourceHandle = ResourceHandle B.ByteString 
    deriving (Eq, Show, Ord)


-- Make a new resource handle from a long url. 
-- TODO here: some parts of the url, like query strings, 
-- should be actually compared under normalization. 
makeResourceHandle :: B.ByteString -> ResourceHandle
makeResourceHandle url = ResourceHandle url


-- | This is what the webserver will use to serve 
--   resources... or some sort of dictionary of this
--   in all probability.
data ServedEntry = ServedEntry {

    -- Status to return to the server
    _sreStatus :: !Int

    -- Headers to return to the server
    ,_sreHeaders :: !UnpackedNameValueList

    -- And other contents which are part of the response,
    -- also to return to the server.
    ,_sreContents :: B.ByteString
    } 


-- Here: translate a .HAR file to a lookup function. It may happen 
-- that this function doesn't find the resource, in that case return 
-- Nothing. Another level should decide what response to cook on 
-- that particular scenario. 
resolveFromHar :: Har_Outer -> ResourceHandle -> Maybe ServedEntry
resolveFromHar har_document resource_handle = 
    M.lookup resource_handle doc_dic
  where
    doc_dic = M.fromList $ extractPairs har_document


extractPairs :: Har_Outer -> [(ResourceHandle, ServedEntry)]
extractPairs har_document = 
    map pairFromEntry doc_entries
  where 
    -- Using a lens to fetch the entries from the document. 
    -- The parenthesis are not needed, except as documentation
    doc_entries = har_document ^. (harLog . entries)


docFromEntry :: Har_Entry -> (ResourceHandle, ServedEntry)
docFromEntry e = (
    handleFromMethodAndUrl
        (req ^. method)
        (req ^. reqUrl )
    , servedEntryFromStatusHeadersAndContents
        (resp ^. status)
        (resp ^. respHeaders . L.to harHeadersToUVL)
        (resp ^. content)
    )
  where 
    req  = e ^. request
    resp = e ^. response


harHeadersToUVL :: [Har_Header] -> UnpackedNameValueList
harHeadersToUVL = error "Not implemented"


handleFromMethodAndUrl :: HereString -> HereString -> ResourceHandle
handleFromMethodAndUrl method url = 
    makeResourceHandle $ method B.(++) url


servedEntryFromStatusHeadersAndContents :: Int
    -> UnpackedNameValueList 
    -> B.ByteString 
    -> ServedEntry
servedEntryFromStatusHeadersAndContents status unvl contents = 
     ServedEntry status unvl contents


-- Implementation details ####################################

-- String type to be used when de-serializing an 
-- element from JSON har
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
makeLenses ''ServedEntry

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


test_02 :: IO Har_Outer
test_02 = do 
    -- Can we hook with urls?
    file_contents <- LB.readFile "tests/bitbucket.org.har"
    case (decode file_contents :: Maybe Har_Outer ) of 

        Just doc_model -> return doc_model

        Nothing -> error "Just got an error"

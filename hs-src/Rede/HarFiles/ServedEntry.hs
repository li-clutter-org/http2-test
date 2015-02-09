{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Rede.HarFiles.ServedEntry(
    sreStatus 
    ,sreHeaders
    ,sreContents
    ,sreHost
    ,resolveFromHar

    ,ServedEntry
    ,ResolveCenter
    ) where 


-- import           Control.Applicative
-- import           Control.Monad
import qualified Control.Lens           as L
import           Control.Lens           ( (^.) )
import           Control.Lens.TH        (makeLenses)
-- import           Text.Printf            (printf)

import           Data.Maybe             (fromMaybe)
import qualified Data.ByteString        as B
import           Network.URI            (parseURI, uriAuthority, uriRegName)
-- import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8  (unpack, pack)
-- import           Data.Text(Text)
import qualified Data.Map.Strict        as M


import           Rede.MainLoop.Tokens   (UnpackedNameValueList(..))

import Rede.HarFiles.JSONDataStructure 


-- How do we identify resources that are being 
-- asked for? Plain ByteString? Wecan start this way...
-- if query-string reordering is an issue, we can supersede
-- this type somehow... 
newtype ResourceHandle = ResourceHandle B.ByteString 
    deriving (Eq, Show, Ord)


-- | Individual entries used on resolution
data ServedEntry = ServedEntry {

    -- Status to return to the server
    _sreStatus :: !Int

    -- Headers to return to the server
    ,_sreHeaders :: !UnpackedNameValueList

    -- And other contents which are part of the response,
    -- also to return to the server.
    ,_sreContents :: !B.ByteString

    -- Let's just keep here the host 
    ,_sreHost :: !B.ByteString
    } 


-- | Everything needed to solve things out
data ResolveCenter = ResolveCenter {
    -- Things I'm actually going to serve
    _servedResources ::  M.Map ResourceHandle ServedEntry

    -- Some other results, like the number of resources that 
    -- can't be served because they are in the wrong HTTP method 
    -- or protocol.
    
    }


makeLenses ''ServedEntry
makeLenses ''ResolveCenter


-- Here: translate a .HAR file to a lookup function. It may happen 
-- that this function doesn't find the resource, in that case return 
-- Nothing. Another level should decide what response to cook on 
-- that particular scenario. 
--
-- TODO: Entry filtering made down below made need to be reported,
-- and for that, this interface changed.
resolveFromHar :: ResolveCenter -> ResourceHandle -> Maybe ServedEntry
resolveFromHar resolve_center resource_handle = 
    M.lookup resource_handle doc_dic
  where
    doc_dic = _servedResources resolve_center


createResolveCenter :: Har_Outer -> ResolveCenter
createResolveCenter har_document = 
    ResolveCenter $ M.fromList $ extractPairs har_document


extractPairs :: Har_Outer -> [(ResourceHandle, ServedEntry)]
extractPairs har_document = 
    map docFromEntry $ filter entryCanBeServed  doc_entries
  where 
    -- Using a lens to fetch the entries from the document. 
    -- The parenthesis are not needed, except as documentation
    doc_entries = har_document ^. (harLog . entries)


-- Right now, we will be filtering out requests which are based on 
-- methods other than GET
entryCanBeServed :: Har_Entry -> Bool 
entryCanBeServed har_entry = http_method == "GET"
  where 
    http_method = har_entry ^. request.method


docFromEntry :: Har_Entry -> (ResourceHandle, ServedEntry)
docFromEntry e = (
    handleFromMethodAndUrl
        (req ^. method)
        the_url
    , servedEntryFromStatusHeadersAndContents
        (resp ^. status)
        (resp ^. respHeaders . L.to harHeadersToUVL)
        content_text
        the_url
    )
  where 
    the_url      = (req ^. reqUrl )
    req          = e ^. request
    resp         = e ^. response
    content_text =  fromMaybe "" (resp ^. content . contentText )


harHeadersToUVL :: [Har_Header] -> UnpackedNameValueList
harHeadersToUVL h = UnpackedNameValueList $ map 
    (\ har_header ->   ( (har_header ^. headerName ), (har_header ^. headerValue) )
    ) h
    

handleFromMethodAndUrl :: HereString -> HereString -> ResourceHandle
handleFromMethodAndUrl methodx url = 
    ResourceHandle $ methodx `B.append` url


servedEntryFromStatusHeadersAndContents :: Int
    -> UnpackedNameValueList 
    -> B.ByteString 
    -> B.ByteString
    -> ServedEntry
servedEntryFromStatusHeadersAndContents statusx unvl contents the_url = 
    ServedEntry statusx unvl contents host_of_url
  where 
    Just uri    = parseURI $ unpack the_url
    Just auth   = uriAuthority uri
    host_of_url = pack $ uriRegName auth
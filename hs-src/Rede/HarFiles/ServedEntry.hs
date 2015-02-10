{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

module Rede.HarFiles.ServedEntry(
    sreStatus 
    ,sreHeaders
    ,sreContents
    ,sreHost
    ,resolveFromHar
    ,createResolveCenter
    ,servedResources
    ,allSeenHosts
    ,createResolveCenterFromFilePath
    ,resourceHandleToByteString
    ,hostsFromHarFile

    ,ServedEntry  (..)
    ,ResolveCenter(..)
    ) where 


import           Control.Exception
import qualified Control.Lens           as L
import           Control.Lens           ( (^.) )
import           Control.Lens.TH        (makeLenses)

import           Data.Typeable
import           Data.Maybe             (fromMaybe)
import           Data.Aeson             (decode)
import qualified Data.ByteString        as B
import           Network.URI            (parseURI, uriAuthority, uriRegName)
import qualified Data.ByteString.Lazy   as LB
import           Data.ByteString.Char8  (unpack, pack)
-- import           Data.Text(Text)
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S


import           Rede.MainLoop.Tokens   (UnpackedNameValueList(..))

import Rede.HarFiles.JSONDataStructure 


-- How do we identify resources that are being 
-- asked for? Plain ByteString? Wecan start this way...
-- if query-string reordering is an issue, we can supersede
-- this type somehow... 
newtype ResourceHandle = ResourceHandle B.ByteString 
    deriving (Eq, Show, Ord)


resourceHandleToByteString :: ResourceHandle -> B.ByteString
resourceHandleToByteString (ResourceHandle bs) = bs


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

    -- A list with all the hosts we have to emulate, without duplicates
    ,_allSeenHosts :: [B.ByteString]

    -- Some other results, like the number of resources that 
    -- can't be served because they are in the wrong HTTP method 
    -- or protocol.
    
    }


data BadHarFile = BadHarFile B.ByteString
    deriving (Show, Typeable)

instance Exception BadHarFile


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
    ResolveCenter  
        (M.fromList resource_pairs) -- <- Creates a dictionary
        unduplicated_hosts
  where 
    resource_pairs = extractPairs har_document
    all_seen_hosts = map (L.view ( L._2 . sreHost) ) resource_pairs 
    unduplicated_hosts = (S.toList . S.fromList) all_seen_hosts


createResolveCenterFromFilePath :: B.ByteString -> IO ResolveCenter
createResolveCenterFromFilePath filename = do 
    file_contents <- LB.readFile $ unpack filename
    case (decode file_contents :: Maybe Har_Outer ) of 

        Just doc_model -> return $ createResolveCenter doc_model

        Nothing -> throw $ BadHarFile filename


-- Convenience function to extract all the hosts from a .har file. 
-- Not very efficient.
hostsFromHarFile :: FilePath -> IO [B.ByteString]
hostsFromHarFile har_filename = do
    resolve_center <- createResolveCenterFromFilePath $ pack har_filename
    return $ resolve_center ^. allSeenHosts


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
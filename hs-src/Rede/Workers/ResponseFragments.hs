{-# LANGUAGE OverloadedStrings #-}

module Rede.Workers.ResponseFragments (
    RequestMethod(..)

    ,simpleResponse
    ,getUrlFromHeaders
    ,ajaxResponseWithLength
    ,getMethodFromHeaders
    ) where 


import Data.Conduit (yield, Source)

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import Rede.MainLoop.CoherentWorker


-- TODO: Add the others....
data RequestMethod = 
    Get_RM
    |Put_RM
    |Post_RM


trivialHeaders :: Int -> Int -> [(B.ByteString, B.ByteString)] 
trivialHeaders status_code content_length = [
    (":status", (pack . show) status_code ),
    ("server",  "reh0m" ),
    ("content-type", "plain/text" ),
    ("content-length", (pack.show) content_length)
    ]

ajaxHeaders :: Int -> Int -> B.ByteString -> [(B.ByteString, B.ByteString)] 
ajaxHeaders status_code content_length content_type = [
    (":status", (pack . show) status_code ),
    ("content-length", (pack . show) content_length),
    ("content-type", content_type),
    ("access-control-allow-origin", "*"),
    ("server",  "reh0m" )
    ]


simpleResponse :: Int -> B.ByteString -> PrincipalStream
simpleResponse status_code text = 
    let 
        data_and_conclussion :: Source IO B.ByteString
        data_and_conclussion = yield text
    in (trivialHeaders status_code (B.length text), [], data_and_conclussion) 


ajaxResponseWithLength ::  Int -> Int -> B.ByteString -> B.ByteString -> PrincipalStream 
ajaxResponseWithLength status_code content_length content_type text = 
    let 
        data_and_conclussion :: Source IO B.ByteString
        data_and_conclussion = yield text
    in (ajaxHeaders status_code content_length content_type, [], data_and_conclussion) 


getUrlFromHeaders :: Headers -> B.ByteString
getUrlFromHeaders headers = let 
    (Just path)  = getHeaderFromFlatList headers ":path"
  in path


getMethodFromHeaders :: Headers -> RequestMethod
getMethodFromHeaders headers = let 
    (Just method) = getHeaderFromFlatList headers ":method"
  in case method of 
    m | m == "get"   -> Get_RM
    m | m == "post"  -> Post_RM
    m | m == "put"   -> Put_RM
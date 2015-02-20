{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Rede.Workers.HarWorker(
    harCoherentWorker

    ,HarWorkerServicePocket
    ,HarWorkerSessionPocket
    ,HarWorkerParams (..)

    ) where 



import           Control.Monad.IO.Class

import qualified Data.ByteString              as B
import           Data.ByteString.Char8        (pack, unpack)
import           Data.Conduit
import qualified Data.Set                     as S

import           Control.Lens                 ((^.))
import qualified Control.Lens                 as L
import qualified Network.URI                  as U
import           Text.Printf


import           Rede.HarFiles.ServedEntry
import           Rede.MainLoop.CoherentWorker (CoherentWorker, PrincipalStream,
                                               getHeaderFromFlatList)
import           Rede.MainLoop.StreamWorker   (send404)
import           Rede.MainLoop.Tokens         (StreamInputToken (..),
                                               StreamOutputAction (..),
                                               StreamWorker,
                                               StreamWorkerClass (..),
                                               UnpackedNameValueList (..),
                                               getHeader)
import           Rede.Utils                   (lowercaseText)
import           Rede.Workers.VeryBasic       (bad404ResponseData,
                                               bad404ResponseHeaders)




-- We use one of these for each session
data HarWorkerSessionPocket  = HarWorkerSessionPocket {
  }


-- And one of these to configure the entire service, once 
-- a .har file is known.
data HarWorkerServicePocket   = HarWorkerServicePocket    {
    -- All resources to be served from here
    _resolveCenter      :: ResolveCenter
  }


data HarWorkerParams = HarWorkerParams {
    -- Where is the .har file I'm going to use?
    _harFilePath :: FilePath  
  }


L.makeLenses ''HarWorkerServicePocket
L.makeLenses ''HarWorkerSessionPocket
L.makeLenses ''HarWorkerParams


-- This will be soon deprecated
instance StreamWorkerClass HarWorkerParams HarWorkerServicePocket HarWorkerSessionPocket where
 
    -- initService :: HarWorkerParams -> IO servicePocket
    initService har_worker_params = do 
        let har_file_path = har_worker_params ^. harFilePath

        resolve_center <- createResolveCenterFromFilePath $ pack har_file_path
        -- dumpPresentHandles resolve_center

        return $ HarWorkerServicePocket {
            _resolveCenter       = resolve_center
          }

    -- initSession :: servicePocket -> IO sessionPocket
    initSession _ = return $ HarWorkerSessionPocket {}

    -- initStream  :: servicePocket -> sessionPocket ->  IO StreamWorker
    initStream service_pocket _ = return $ 
        harWorker resolve_center 
      where 
        resolve_center = service_pocket ^. resolveCenter


harWorker :: ResolveCenter -> StreamWorker
harWorker resolve_center = do 
    -- Got a token?
    input_token_maybe <- await 

    case input_token_maybe of
        Nothing     ->  do 
            return ()

        Just (Headers_STk headers) -> do
            -- Debug: say 
            liftIO $ putStrLn $ printf "Got request to %s" $ show resource_handle
            let maybe_served_entry  = resolver resource_handle :: Maybe ServedEntry

            case maybe_served_entry of 

                Just served_entry -> do
                    liftIO $ putStrLn $ show adapted_headers
                    yield $ SendHeaders_SOA adapted_headers
                    yield $ SendData_SOA    (served_entry ^. sreContents)
                    yield $ Finish_SOA
                    return ()
                  where 
                    adapted_headers = adaptHeaders (served_entry ^. sreStatus ) (served_entry ^. sreHeaders)

                Nothing -> 
                    send404
 
          where 
            -- Let's build a complete url
            (Just path)                            = getHeader headers ":path"
            (Just host)                            = getHeader headers ":host"
            Just (U.URI _ _ u_path u_query u_frag) = U.parseURIReference $ unpack path
            complete_url                           = U.URI {
                U.uriScheme     = "https:"
                ,U.uriAuthority = Just $ U.URIAuth {
                    U.uriUserInfo = ""
                    ,U.uriRegName = unpack host 
                    ,U.uriPort    = ""
                    }
                ,U.uriPath      = u_path
                ,U.uriQuery     = u_query 
                ,U.uriFragment  = u_frag 
              }

            -- This string includes the method in front of the schema and everything else...
            resource_handle     = handleFromMethodAndUrl method $ (pack.show) complete_url
            
            (Just method)   = getHeader headers ":method"
  where 
    resolver x = resolveFromHar resolve_center x



-- dumpPresentHandles :: ResolveCenter -> IO ()
-- dumpPresentHandles resolve_center = do 
--     mimic_data_dir <- mimicDataDir
--     handles_filename <- return $ mimic_data_dir </> "har_handles.txt"
--     B.writeFile handles_filename $ B.intercalate "\n" $ 
--         map 
--             resourceHandleToByteString 
--             (resolve_center ^. servedResources . L.to M.keys)


adaptHeaders :: Int -> UnpackedNameValueList -> UnpackedNameValueList 
adaptHeaders status_code (UnpackedNameValueList raw_headers) = let
    -- Let's remove some headers....
    -- Connection, Host, Keep-Alive, Proxy-Connection, Transfer-Encoding, Accept-Ranges
    no_oniuxus_headers = [ (x,y) | (x,y) <- raw_headers, x `S.notMember` headers_to_remove]

    -- Add 'status' header
    with_status  = (":status", pack $ show status_code):no_oniuxus_headers
    with_version = (":version", "HTTP/1.1"):with_status

    headers_to_send = [ (lowercaseText x, y) | (x,y) <- with_version ]

    headers_to_remove = S.fromList [
        ":status",
        ":version",
        "connection",
        "host",
        "keep-alive",
        "content-encoding",  -- <-- gzip compression goes here
        "date",
        "proxy-connection",
        "transfer-encoding",
        "accept-ranges"] :: S.Set B.ByteString

    -- And be sure to put everything lowercase 
  in 
    UnpackedNameValueList headers_to_send
    

harCoherentWorker :: ResolveCenter -> CoherentWorker 
harCoherentWorker resolve_center input_headers = do 

    liftIO $ putStrLn $ "headers: " ++ (show input_headers)
    liftIO $ putStrLn $ "Got request to %s" ++(show resource_handle)
    let 
        maybe_served_entry  = resolver resource_handle :: Maybe ServedEntry
        resolver = resolveFromHar resolve_center
    -- Not pushing any streams now.... 
    let pushed_streams = []

    case maybe_served_entry of 

        Just served_entry -> let 
                contents = (served_entry ^. sreContents)
                UnpackedNameValueList adapted_headers = adaptHeaders (served_entry ^. sreStatus ) (served_entry ^. sreHeaders)
            in return (adapted_headers , pushed_streams, yield contents)

        Nothing -> return bad404PrincipalStream


  where 
    (Just path)                            = getHeaderFromFlatList input_headers ":path"
    (Just host)                            = getHeaderFromFlatList input_headers ":authority"
    Just (U.URI _ _ u_path u_query u_frag) = U.parseURIReference $ unpack path
    complete_url                           = U.URI {
        U.uriScheme     = "https:"
        ,U.uriAuthority = Just $ U.URIAuth {
            U.uriUserInfo = ""
            ,U.uriRegName = unpack host 
            ,U.uriPort    = ""
            }
        ,U.uriPath      = u_path
        ,U.uriQuery     = u_query 
        ,U.uriFragment  = u_frag 
      }

    -- This string includes the method in front of the schema and everything else...
    resource_handle     = handleFromMethodAndUrl method $ (pack.show) complete_url
    
    (Just method)   = getHeaderFromFlatList input_headers ":method"


bad404PrincipalStream :: PrincipalStream 
bad404PrincipalStream = 
    (
        bad404ResponseHeaders ,
        [],
        yield bad404ResponseData
    )
{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Rede.Workers.HarWorker(

    HarWorkerServicePocket
    ,HarWorkerSessionPocket
    ,HarWorkerParams (..)
    ) where 


import Rede.HarFiles.ServedEntry

import           Control.Monad.IO.Class
import           Control.Exception
-- import           Control.Monad

import qualified Data.ByteString            as B
import qualified Data.Map.Strict            as M
import           Data.ByteString.Char8      (pack, unpack)
import           Data.Typeable
import           Data.Conduit

import           Text.Printf
-- import           Data.List                  (find, isInfixOf, isSuffixOf)
import qualified Network.URI                as U
import           System.Posix.Env.ByteString (getEnv) 
-- import           System.Directory           (doesFileExist)
import           System.FilePath
-- import qualified Data.HashTable.IO          as H
-- import           Control.Concurrent.MVar
import qualified Control.Lens        as L
import           Control.Lens        ( (^.) )
   


import           Rede.MainLoop.ConfigHelp
import           Rede.MainLoop.Tokens       (StreamInputToken       (..)
                                             ,StreamOutputAction    (..)
                                             ,StreamWorker
                                             ,UnpackedNameValueList (..)
                                             ,StreamWorkerClass     (..)
                                             
                                             ,getHeader)



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


instance StreamWorkerClass HarWorkerParams HarWorkerServicePocket HarWorkerSessionPocket where
 
    -- initService :: HarWorkerParams -> IO servicePocket
    initService har_worker_params = do 
        let har_file_path = har_worker_params ^. harFilePath

        resolve_center <- createResolveCenterFromFilePath $ pack har_file_path
        dumpPresentHandles resolve_center

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
            liftIO $ putStrLn $ printf "Got request to %s" $ unpack complete_url_bs
            yield $ SendHeaders_SOA $ UnpackedNameValueList  [
                 (":status", "200")
                ,(":version", "HTTP/1.1")
                ,("content-length", "10")
                ,("content-type", "text/html")
                ,("server", "ReHv0.2")
                ]
            yield $ SendData_SOA "0123456789"
            yield $ Finish_SOA
 
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
            complete_url_bs     = B.append method $ pack $ show complete_url
            
            (Just method)   = getHeader headers ":method"




dumpPresentHandles :: ResolveCenter -> IO ()
dumpPresentHandles resolve_center = do 
    mimic_data_dir <- mimicDataDir
    handles_filename <- return $ mimic_data_dir </> "har_handles.txt"
    B.writeFile handles_filename $ B.intercalate "\n" $ 
        map 
            resourceHandleToByteString 
            (resolve_center ^. servedResources . L.to M.keys)    
{-# LANGUAGE OverloadedStrings #-}
module Rede.Research.Main(research) where 


import           Control.Concurrent.Chan
import qualified Data.ByteString              as B

-- import qualified Data.ByteString.Lazy            as BL
-- import           Data.Foldable                   (foldMap)

-- import           System.Directory
-- import           System.FilePath
-- import           System.IO
-- import           System.Process

-- import           Options.Applicative

-- import           Rede.SimpleHTTP1Response        (exampleHTTP11Response)

-- import           Rede.HarFiles.ServedEntry       (createResolveCenterFromFilePath,
--                                                   hostsFromHarFile)
-- import           Rede.MainLoop.CoherentWorker    (CoherentWorker)
import           Rede.MainLoop.ConfigHelp     (getCertFilename,
                                               getMimicPostInterface,
                                               getMimicPostPort,
                                               getPrivkeyFilename)
import           Rede.MainLoop.OpenSSL_TLS    (tlsServeWithALPN)
import           Rede.Research.ResearchWorker (runResearchWorker)

import           Rede.Http2.MakeAttendant     (http2Attendant)




research :: String -> IO ()
research mimic_config_dir  = do 
  -- TODO: Generate .har from url


    -- Things to do: 
    -- First publish this url to the capture webserver
    url_chan <- newChan

    publishUrlToCaptureWebserver mimic_config_dir url_chan


    -- Then wait for a POST with the contents of the website. 


    -- Then publish a task definition file to the .har colaborator 


    -- When confirmation of .har colaborator is received, publish a task definition 
    -- file to the fake -loader 



publishUrlToCaptureWebserver :: String -> Chan B.ByteString -> IO ()
publishUrlToCaptureWebserver mimic_config_dir url_chan  = do
    post_port <- getMimicPostPort mimic_config_dir
    iface <- getMimicPostInterface mimic_config_dir
    let 
        priv_key_filename = getPrivkeyFilename mimic_config_dir
        cert_filename  = getCertFilename mimic_config_dir

    http2worker <- runResearchWorker url_chan

    tlsServeWithALPN  cert_filename priv_key_filename iface [ 
         ("h2-14", http2Attendant http2worker)
        ] post_port  

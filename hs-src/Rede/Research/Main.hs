
module Rede.Research.Main(research) where 


import qualified Data.ByteString                 as B
import qualified Data.ByteString.Builder         as Bu
import           Data.ByteString.Char8           (pack)
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (foldMap)
import           Data.Monoid

import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import           Options.Applicative

import           Rede.SimpleHTTP1Response        (exampleHTTP11Response)

import           Rede.HarFiles.ServedEntry       (createResolveCenterFromFilePath,
                                                  hostsFromHarFile)
import           Rede.MainLoop.CoherentWorker    (CoherentWorker)
import           Rede.MainLoop.ConfigHelp        (configDir, getInterfaceName,
                                                  getMimicPort, mimicDataDir,
                                                  getMimicPostPort, getMimicPostInterface,
                                                  getPrivkeyFilename,
                                                  getCertFilename
                                                  )
import           Rede.MainLoop.OpenSSL_TLS        (tlsServeWithALPNOnce)
import           Rede.MainLoop.PushPullType
import           Rede.Workers.AcceptCoherentPost (acceptCoherentPost, informUrlToDownload)
import           Rede.Workers.HarWorker          (harCoherentWorker)

import           Rede.Http2.Framer               (wrapSession)
import           Rede.Http2.MakeAttendant        (http2Attendant)



research :: String -> String -> FilePath -> IO ()
research mimic_config_dir url_to_research har_filename = do 
    -- Things to do: 
    -- First publish this url to the capture webserver
    publishUrlToCaptureWebserver mimic_config_dir $ pack url_to_research


    -- Then wait for a POST with the contents of the website. 
    acceptPostWebserver mimic_config_dir har_filename


    -- Then publish a task definition file to the .har colaborator 


    -- When confirmation of .har colaborator is received, publish a task definition 
    -- file to the fake -loader 

publishUrlToCaptureWebserver :: String -> B.ByteString -> IO ()
publishUrlToCaptureWebserver mimic_config_dir url  = do
    post_port <- getMimicPostPort mimic_config_dir
    iface <- getMimicPostInterface mimic_config_dir
    let 
        priv_key_filename = getPrivkeyFilename mimic_config_dir
        cert_filename  = getCertFilename mimic_config_dir

        http2worker = informUrlToDownload url

    tlsServeWithALPNOnce  cert_filename priv_key_filename iface [ 
         ("h2-14", http2Attendant http2worker)
        ] post_port  


acceptPostWebserver :: String ->  FilePath -> IO ()
acceptPostWebserver mimic_config_dir har_filename  = do
    post_port <- getMimicPostPort mimic_config_dir
    iface <- getMimicPostInterface mimic_config_dir
    let 
        priv_key_filename = getPrivkeyFilename mimic_config_dir
        cert_filename  = getCertFilename mimic_config_dir

        http2worker = acceptCoherentPost har_filename
 

    tlsServeWithALPNOnce  cert_filename priv_key_filename iface [ 
         ("h2-14", http2Attendant http2worker)
        ] post_port  

-- post_port <- getMimicPostPort mimic_config_dir
--             iface <- getMimicPostInterface mimic_config_dir
--             putStrLn $  "Mimic post port: " ++ (show post_port)    
--             putStrLn $  "Using post interface: " ++ (show iface)

--             let 
--                 priv_key_filename = getPrivkeyFilename mimic_config_dir
--                 cert_filename  = getCertFilename mimic_config_dir

--             putStrLn $ "Chosen cert. at file: " ++ (show cert_filename)
--             putStrLn $ "... with private key: " ++ (show priv_key_filename)

--             let 
--                 http2worker = acceptCoherentPost har_filename

--             tlsServeWithALPN  cert_filename priv_key_filename iface [ 

--                  ("h2-14", http2Attendant http2worker)

--                 ] post_port          
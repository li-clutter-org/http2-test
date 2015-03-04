{-# LANGUAGE OverloadedStrings #-}
module Rede.Research.Main(research) where 


import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan
import qualified Data.ByteString              as B

import           Rede.MainLoop.ConfigHelp     (getCertFilename,
                                               getMimicPostInterface,
                                               getMimicPostPort,
                                               configDir,
                                               getPrivkeyFilename)
import           Rede.MainLoop.OpenSSL_TLS    (tlsServeWithALPN, FinishRequest(..))
import           Rede.Research.ResearchWorker (runResearchWorker,
                                               spawnHarServer)

import           Rede.Http2.MakeAttendant     (http2Attendant)
import           Rede.HarFiles.ServedEntry    (ResolveCenter)





research :: FilePath -> IO ()
research mimic_dir  = do 
    let 
        mimic_config_dir = configDir mimic_dir
    url_chan <- newChan
    resolve_center_chan <- newChan 
    finish_request_chan <- newChan

    forkIO $ spawnHarServer mimic_dir resolve_center_chan finish_request_chan

    publishUrlToCaptureWebserver 
          mimic_config_dir 
          url_chan 
          resolve_center_chan 
          finish_request_chan


publishUrlToCaptureWebserver :: String -> Chan B.ByteString -> Chan ResolveCenter -> Chan FinishRequest -> IO ()
publishUrlToCaptureWebserver mimic_config_dir url_chan  resolve_center_chan finish_request_chan = do
    post_port <- getMimicPostPort mimic_config_dir
    iface <- getMimicPostInterface mimic_config_dir
    let 
        priv_key_filename = getPrivkeyFilename mimic_config_dir
        cert_filename  = getCertFilename mimic_config_dir

    http2worker <- runResearchWorker url_chan resolve_center_chan finish_request_chan

    tlsServeWithALPN  cert_filename priv_key_filename iface [ 
         ("h2-14", http2Attendant http2worker)
        ] post_port  

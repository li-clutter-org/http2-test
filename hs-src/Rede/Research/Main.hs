{-# LANGUAGE OverloadedStrings #-}
module Rede.Research.Main(research) where 


import           Control.Concurrent           (forkIO)
-- import           Control.Concurrent.Chan
-- import           Control.Concurrent.MVar  
import           Control.Concurrent.STM.TMVar (newEmptyTMVar, TMVar)
import           Control.Concurrent.STM       (atomically)
import           System.FilePath
-- import qualified Data.ByteString              as B
import           Data.ByteString.Char8        (pack)

import           System.Log.Logger  
import           Rede.MainLoop.ConfigHelp     (getCertFilename,
                                               getMimicPostInterface,
                                               getMimicPostPort,
                                               configDir,
                                               getPrivkeyFilename)
import           Rede.MainLoop.OpenSSL_TLS    (tlsServeWithALPN, FinishRequest(..))
import           Rede.Research.ResearchWorker (runResearchWorker,
                                               spawnHarServer)

import           SecondTransfer.Http2         (http2Attendant)
import           Rede.HarFiles.ServedEntry    (ResolveCenter)


research :: FilePath -> IO ()
research mimic_dir  = do 
    let 
        mimic_config_dir = configDir mimic_dir
    resolve_center_chan <- atomically $ newEmptyTMVar 
    finish_request_chan <- atomically $ newEmptyTMVar

    forkIO $ spawnHarServer mimic_dir resolve_center_chan finish_request_chan

    setupAndRun 
        mimic_dir
        mimic_config_dir 
        resolve_center_chan 
        finish_request_chan


setupAndRun :: FilePath -> FilePath -> TMVar ResolveCenter -> TMVar FinishRequest -> IO ()
setupAndRun mimic_dir mimic_config_dir resolve_center_chan finish_request_chan = do
    post_port <- getMimicPostPort mimic_config_dir
    infoM "ResearchWorker" $ "Control port: " ++ (show post_port)
    iface <- getMimicPostInterface mimic_config_dir
    let 
        priv_key_filename = getPrivkeyFilename mimic_config_dir
        cert_filename  = getCertFilename mimic_config_dir
        research_dir = mimic_dir </> "hars/"

    http2worker <- runResearchWorker
                       resolve_center_chan
                       finish_request_chan
                       research_dir
                       (pack iface)

    tlsServeWithALPN  cert_filename priv_key_filename iface [ 
         ("h2-14", http2Attendant http2worker)
        ] post_port  

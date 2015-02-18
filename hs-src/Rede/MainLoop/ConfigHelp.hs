module Rede.MainLoop.ConfigHelp(
	configDir
	,mimicDataDir
	,wwwDir
	,getServedHost
	,getServedPort
	,getHostPort
	,getInterfaceName
	,getMimicPort
	,getPrivkeyFilename
	,getCertFilename

	,HostPort
	) where 


import System.FilePath
import System.Environment
import Control.Exception
import System.Directory


type HostPort = (String, Int)


kelDataDir :: IO FilePath
kelDataDir = 
	catch 
		(getEnv "KEL_DATA_DIR")
		noEnvErrorHandler


mimicDataDir :: IO FilePath
mimicDataDir = 
	catch 
		(getEnv "MIMIC_DATA_DIR")
		noEnvErrorHandler


configDir :: FilePath -> FilePath 
configDir base_dir = do 
	base_dir </> "config"


wwwDir :: IO FilePath
wwwDir = do 
	data_dir <- kelDataDir 
	return $ data_dir </> "www"


noEnvErrorHandler :: IOError -> IO FilePath 
noEnvErrorHandler _ = do 
	putStrLn "NoEnvironment"
	getCurrentDirectory


getServedHost :: FilePath -> IO String 
getServedHost config_dir = do 
	contents <- readFile $ config_dir </> "servedhost.conf"
	return $ contents


getCertFilename :: FilePath -> FilePath 
getCertFilename config_dir = config_dir </> "servercert.pem"


getPrivkeyFilename :: FilePath -> FilePath 
getPrivkeyFilename config_dir = config_dir </> "privkey.pem"


getInterfaceName :: FilePath -> IO String 
getInterfaceName config_dir = do 
	contents <- readFile $ config_dir </> "useinterface.conf"
	return $ contents


getServedPort :: FilePath -> IO Int 
getServedPort config_dir = do 
	contents <- readFile $ config_dir </> "port.conf"
	return $ read contents


getHostPort :: FilePath -> IO HostPort
getHostPort config_dir = do 
	served_host <- getServedHost config_dir
	served_port <- getServedPort config_dir

	return (served_host, served_port)


getMimicPort :: IO Int
getMimicPort = do 
	mimic_dir <- mimicDataDir
	let config_dir = configDir mimic_dir
	contents <- readFile $ config_dir </> "port.conf"
	return $ read contents

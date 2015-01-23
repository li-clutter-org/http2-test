module Rede.MainLoop.ConfigHelp(
	configDir
	,wwwDir
	,getServedHost
	,getServedPort
	,getHostPort
	,getInterfaceName

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


configDir :: IO FilePath 
configDir = do 
	data_dir <- kelDataDir 
	return $ data_dir </> "config"


wwwDir :: IO FilePath
wwwDir = do 
	data_dir <- kelDataDir 
	return $ data_dir </> "www"


noEnvErrorHandler :: IOError -> IO FilePath 
noEnvErrorHandler _ = do 
	putStrLn "NoEnvironment"
	getCurrentDirectory


getServedHost :: IO String 
getServedHost = do 
	config_dir <- configDir 
	contents <- readFile $ config_dir </> "servedhost.conf"
	return $ contents


getInterfaceName :: IO String 
getInterfaceName = do 
	config_dir <- configDir 
	contents <- readFile $ config_dir </> "useinterface.conf"
	return $ contents




getServedPort :: IO Int 
getServedPort = do 
	config_dir <- configDir 
	contents <- readFile $ config_dir </> "port.conf"
	return $ read contents


getHostPort :: IO HostPort
getHostPort = do 
	served_host <- getServedHost
	served_port <- getServedPort

	return (served_host, served_port)
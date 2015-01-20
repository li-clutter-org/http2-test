module Rede.MainLoop.ConfigHelp(
	configDir
	,wwwDir
	) where 

import System.FilePath
import System.Environment
import Control.Exception
import System.Directory


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

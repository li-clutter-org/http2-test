module SpdyPing.MainLoop.ConfigHelp(configDir) where 

-- import System.FilePath
import System.Environment
import Control.Exception
import System.Directory


configDir :: IO FilePath 
configDir = do 
	catch 
		(getEnv "KEL_DATA_DIR")
		noEnvErrorHandler


noEnvErrorHandler :: IOError -> IO FilePath 
noEnvErrorHandler _ = do 
	putStrLn "NoEnvironment"
	getCurrentDirectory

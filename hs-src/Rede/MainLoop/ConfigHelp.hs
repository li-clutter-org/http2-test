{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}

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
	,getMimicPostPort
	,getMimicPostInterface
	,readRedisConfig

	,HostPort
	) where 


import           Control.Exception

import           System.Directory
import           System.Environment
import           System.FilePath

import qualified Database.Redis        as Re


import           Control.Applicative
import           Control.Lens          ((^.))
import           Control.Lens.TH       (makeLenses)


import           Data.Typeable
import           Data.Aeson
import           Data.Aeson.Types      ()
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as LB
import           Data.ByteString.Char8 (pack)

import           Rede.Utils            (stripString)


data BadAesonFile = BadAesonFile B.ByteString
    deriving (Show, Typeable)

instance Exception BadAesonFile

type HostPort = (String, Int)

data SimpleRedisConnectInfo = SimpleRedisConnectInfo {
	_connectHost_SRCI      :: Re.HostName
	,_connectPort_SRCI     :: Int
	,_connectDatabase_SRCI :: Int
	}

makeLenses ''SimpleRedisConnectInfo


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


getMimicPostPort :: FilePath -> IO Int
getMimicPostPort config_dir = do 
	contents <- readFile $ config_dir </> "post-port.conf"
	return $  read contents


getMimicPostInterface :: FilePath -> IO String
getMimicPostInterface config_dir = do 
	contents <-  readFile $ config_dir </> "post-interface.conf"
	return $ stripString contents


getRedisConfigPlace :: FilePath -> FilePath 
getRedisConfigPlace config_dir = do 
	config_dir </> "redis.conf"


readRedisConfig :: FilePath -> IO Re.ConnectInfo 
readRedisConfig config_dir = do 
	contents <- LB.readFile $ getRedisConfigPlace config_dir
	case (eitherDecode contents :: Either String SimpleRedisConnectInfo) of 

		Right json_doc -> do 
			return $ toConnectionInfo json_doc 

		Left msg -> do 
			throwIO $ BadAesonFile $ pack msg


instance FromJSON SimpleRedisConnectInfo where 

	parseJSON (Object v) = SimpleRedisConnectInfo <$> 
		(  v .:  "server"     )       <*> 
		(  v .:  "port"       )       <*>
		(  v .:? "dbno" .!= 0 )


toConnectionInfo :: SimpleRedisConnectInfo -> Re.ConnectInfo 
toConnectionInfo srci = Re.defaultConnectInfo {
	Re.connectHost     = srci ^. connectHost_SRCI,
	Re.connectPort     = Re.PortNumber $ fromIntegral $ srci ^. connectPort_SRCI,
	Re.connectDatabase = fromIntegral $ srci ^. connectDatabase_SRCI
	}
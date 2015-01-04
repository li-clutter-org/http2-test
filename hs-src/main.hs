{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Options.Applicative
import qualified Data.ByteString as B
import qualified Network.Connection as N
import Data.Default
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import System.X509 (getSystemCertificateStore)
import qualified SpdyPing.TLSConnect as SP

data CmdConfig = CmdConfig
  { host :: String
  , port :: Int }
  deriving Show

sample :: Parser CmdConfig
sample = CmdConfig
     <$> strOption
         ( long "host"
        <> short 's'
        <> metavar "HOST"
        <> help "Target for the Ping" )
     <*> ((fromIntegral . toInteger . read)
     <$> strOption
         ( long "port"
        <> short 'p'
        <> help "Port to connect to" ))

greet :: CmdConfig -> IO ()
greet (CmdConfig h p) = do
    ctx <- N.initConnectionContext
    scs <- getSystemCertificateStore
    con <- N.connectTo ctx $ N.ConnectionParams 
        {
            N.connectionHostname = h
            , N.connectionPort = fromInteger $ fromIntegral p
            , N.connectionUseSecure = Just $ N.TLSSettings ( SP.makeTLSParamsForSpdy
                ctx (h,p) scs)
            , N.connectionUseSocks = Nothing
        }
    N.connectionPut con (B.singleton 0xa)
    r <- N.connectionGet con 1
    putStrLn $ show r
    N.connectionClose con
    return ()

main :: IO ()
main = execParser opts >>= greet

opts :: ParserInfo CmdConfig
opts = info (sample <**> helper)
  ( fullDesc
 <> progDesc "Ping a SPDY host"
 <> header "hello - a test for optparse-applicative" )


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Options.Applicative
import qualified Data.ByteString as B
import qualified Network.Connection as N
import System.X509 (getSystemCertificateStore)
import qualified SpdyPing.TLSConnect as SP
import SpdyPing.Utils(strToInt)

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
     <*> ( strToInt
     <$> strOption
         ( long "port"
        <> short 'p'
        <> help "Port to connect to" ))

greet :: CmdConfig -> IO ()
greet CmdConfig{ host=h, port=p} = do
    ctx <- N.initConnectionContext
    scs <- getSystemCertificateStore
    con <- N.connectTo ctx $ N.ConnectionParams 
        {
            N.connectionHostname = h
            , N.connectionPort = fromInteger $ fromIntegral p
            , N.connectionUseSecure = Just $ N.TLSSettings ( SP.makeTLSParamsForSpdy
                 (h,p) scs)
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


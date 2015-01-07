{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Network.Connection  as N
import           Options.Applicative
import qualified SpdyPing.TLSConnect as SP
import           SpdyPing.Utils      (strToInt)

import           System.X509         (getSystemCertificateStore)
import           SpdyPing.Subprograms.BasicPing(basicPingProgram)


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
    basicPingProgram (N.connectionGet con 4096) (N.connectionPut con)
      -- putStrLn $ show $ readFrame $ LB.fromChunks [r]
    N.connectionClose con
    return ()


main :: IO ()
main = execParser opts >>= greet


opts :: ParserInfo CmdConfig
opts = info (sample <**> helper)
  ( fullDesc
 <> progDesc "Ping a SPDY host"
 <> header "Let's learn everything there is to learn about that host..." )


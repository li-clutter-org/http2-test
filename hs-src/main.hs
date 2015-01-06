{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy as LB
import Data.Binary.Put(runPut)
-- import Data.Binary.Get(runGet)
import Data.Binary(put)
import qualified Network.Connection  as N
import           Options.Applicative
import qualified SpdyPing.TLSConnect as SP
import           SpdyPing.Utils      (strToInt)
import           SpdyPing.Framing.Ping
import           SpdyPing.Framing.Frame(measure)
import           SpdyPing.Framing.AnyFrame
import           System.X509         (getSystemCertificateStore)
import           SpdyPing.MainLoop 


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

pingOne :: B.ByteString
pingOne = B.concat . LB.toChunks $ runPut $ put $ pingFrame 5


-- withLengthRead :: (IO B.ByteString) 
--                   -> (LB.ByteString -> Maybe Int)
--                   -> IO LB.ByteString
-- withLengthRead retriever lengthSearch 


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
    N.connectionPut con pingOne
    sequence $ repeat $ do
      r <- N.connectionGet con (measure $ pingFrame 0)
      putStrLn $ show $ readFrame $ LB.fromChunks [r]
    N.connectionClose con
    return ()

main :: IO ()
main = execParser opts >>= greet

opts :: ParserInfo CmdConfig
opts = info (sample <**> helper)
  ( fullDesc
 <> progDesc "Ping a SPDY host"
 <> header "hello - a test for optparse-applicative" )


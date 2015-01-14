
{-# LANGUAGE OverloadedStrings #-}

import SpdyPing.MainLoop.Tls(
    readyTCPSocket 
    ,tcpServe
    ,enchantSocket
    ,buildContextParams)

import           Control.Concurrent
import qualified Control.Exception            as E
import qualified Data.ByteString.Lazy         as BL
import qualified Network.TLS                  as T
import           SpdyPing.SimpleHTTP1Response (exampleHTTP11Response)
import           System.Exit
import           System.Posix.Signals

-- import System.IO

-- import Network.Socket

-- main :: IO ()
-- main = do
--   tid <- myThreadId
--   installHandler keyboardSignal (Catch (E.throwTo tid ExitSuccess)) Nothing
--   threadDelay (10000000)

main :: IO ()
main = do 
    tid <- myThreadId
    installHandler keyboardSignal (Catch (do 
        E.throwTo tid ExitSuccess
        )) Nothing
    putStrLn "Hello world"
    listening_socket <- readyTCPSocket "127.0.0.1" 1060
    putStrLn "Socket was opened"
    server_params <- buildContextParams
    tcpServe listening_socket $ \ s -> do 
        putStrLn "Connection opened"
        -- h <- socketToHandle s ReadWriteMode 
        -- hPutStrLn h "heythere"
        -- hClose h
        E.catch (do
            ctx <- enchantSocket s server_params
            putStrLn "Socket enchanted"
            -- Time to say something
            T.sendData ctx $ BL.fromChunks [exampleHTTP11Response]
            ) 
            excHandler


excHandler :: E.SomeException -> IO () 
excHandler (E.SomeException e) = 
    putStrLn $ show e


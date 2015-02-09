
{-# LANGUAGE OverloadedStrings #-}

import Rede.MainLoop.Tls(
    tlsServeProtocols
    )


import qualified Data.ByteString.Lazy         as BL


import           Options.Applicative       


import           Rede.SimpleHTTP1Response (exampleHTTP11Response)

import           Rede.MainLoop.PushPullType
import           Rede.MainLoop.Conduit
import           Rede.MainLoop.Tokens
import           Rede.MainLoop.ConfigHelp (getMimicPort, getInterfaceName)

import           Rede.SpdyProtocol.Session(basicSession)
import           Rede.Workers.HarWorker(HarWorkerServicePocket)
import           Rede.SpdyProtocol.Framing.ChunkProducer(chunkProducerHelper)


-- What is the program going to do?
data ProgramAction = 
    OutputHosts_PA
    |ServeHar_PA


actionStrToAction :: String -> ProgramAction
actionStrToAction "output-hosts" = OutputHosts_PA 
actionStrToAction "serve"        = ServeHar_PA
actionStrToAction _              = error "Action doesn't exist"


data Program  = Program {
    action   :: ProgramAction
    ,harFile :: String
    }


programParser :: Parser Program
programParser = Program <$> (
    actionStrToAction <$>
        strOption
             ( long "action"
                <> metavar "ACTION"
                <> help    "What's the program going to do" )
    ) <*> (
        strOption
            ( long "har-file"
               <>  metavar "HARFILE"
               <>  help    "Har file to process"
            )
    )


main :: IO ()
main = do
    port  <-  getMimicPort
    iface <-  getInterfaceName
    tlsServeProtocols [ 
        ("spdy/3.1",spdyAttendant)
        ,("http/1.1",httpAttendant) 
        ] iface port


-- The "PushAction" is a callback that can pull bytes from 
-- some medium (a network socket, for example), while the 
-- PullAction is the opposite. Here I'm saying this: give me two 
-- callbacks for getting and sending data, and I will take care of 
-- the rest.
httpAttendant :: PushAction -> PullAction -> IO ()
httpAttendant push _ = 
    push $ BL.fromChunks [exampleHTTP11Response]


spdyAttendant :: PushAction -> PullAction -> IO () 
spdyAttendant push pull = do 
    fs_worker_service_pocket <- initService :: IO HarWorkerServicePocket
    activateSessionManager  
        id 
        (basicSession fs_worker_service_pocket) 
        push 
        pull
        chunkProducerHelper

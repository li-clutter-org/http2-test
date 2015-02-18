-- This one is in charge of taking and producing frames 
{-# LANGUAGE OverloadedStrings, StandaloneDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Rede.Http2.Framer (
    wrapSession
    ) where 



import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class       (liftIO)
import           Data.Binary                  (decode)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB
import           Data.Conduit
import           Data.Typeable                (Typeable)

import qualified Network.HTTP2                as NH2


import           Rede.Http2.Session
import           Rede.MainLoop.CoherentWorker (CoherentWorker)
import qualified Rede.MainLoop.Framer         as F
import           Rede.MainLoop.PushPullType   (Attendant, PullAction,
                                               PushAction)
import           Rede.Utils                   (Word24, word24ToInt)


http2PrefixLength :: Int
http2PrefixLength = B.length NH2.connectionPreface


data BadPrefixException = BadPrefixException 
    deriving (Show, Typeable)

instance Exception BadPrefixException


wrapSession :: CoherentWorker -> Attendant
wrapSession coherent_worker push_action pull_action = do

    putStrLn "Wrap session"

    -- ?
    let session_start = SessionStartData {}

    (session_input, session_output) <- http2Session coherent_worker session_start

    putStrLn "Session start returned"

    forkIO $ inputGatherer pull_action session_input 
    forkIO $ outputGatherer push_action session_output

    putStrLn "Workers sparked"

    return ()


http2FrameLength :: F.LengthCallback
http2FrameLength bs | (B.length bs) >= 3     = let
    word24 = decode input_as_lbs :: Word24
    input_as_lbs = LB.fromStrict bs
  in 
    Just $ (word24ToInt word24) + 9 -- Nine bytes that the frame header always uses
http2FrameLength _ = Nothing


inputGatherer :: PullAction -> SessionInput -> IO ()
inputGatherer pull_action session_input = do 
    -- We can start by reading off the prefix....
    (prefix, remaining) <- F.readLength http2PrefixLength pull_action

    if prefix /= NH2.connectionPreface 
      then 
        throwIO BadPrefixException
      else 
        return ()

    -- Can I get a whole packer?
    let source = F.readNextChunk http2FrameLength remaining pull_action
    source $$ consume 
  where 
    consume = do 
        maybe_bytes <- await 
        -- Deserialize

        case maybe_bytes of 

            Just bytes -> do
                let 
                    error_or_frame = NH2.decodeFrame some_settings bytes
                    -- TODO: See how we can change these....
                    some_settings = NH2.defaultSettings

                case error_or_frame of 

                    Left some_error -> do 
                        liftIO $ putStrLn $ "Got an error: " ++ (show some_error)

                    Right a_frame   -> do 
                        -- TODO: Check if we receive a settings frame, and consequently change it....

                        liftIO $ sendFrametoSession session_input a_frame

                consume 

            Nothing    -> 
                -- We may as well exit this thread
               return ()


outputGatherer :: PushAction -> SessionOutput -> IO ()
outputGatherer push_action session_output = do 

    -- We start by sending a settings frame 
    pushFrame 
        (NH2.EncodeInfo NH2.defaultFlags (NH2.toStreamIdentifier 0) Nothing)
        (NH2.SettingsFrame [])

    loopPart

  where 

    pushFrame p1 p2 = do
        let bs = LB.fromStrict $ NH2.encodeFrame p1 p2
        push_action bs

    loopPart = do 

        command_or_frame  <- getFrameFromSession session_output

        case command_or_frame of 

            Left cmd -> do 
                -- TODO: This is just a quickie behavior I dropped 
                -- here, semantics need to be different probably.
                putStrLn $ "Received a command... terminating " ++ (show cmd)

            Right (p1, p2) -> do 
                pushFrame p1 p2 

                loopPart

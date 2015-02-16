-- This one is in charge of taking and producing frames 
{-# LANGUAGE OverloadedStrings #-}
module Rede.Http2.Framer (
    wrapSession
    ) where 



import           Control.Concurrent
import           Control.Monad.IO.Class       (liftIO)
import           Data.Binary                  (decode)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB
import           Data.Conduit

import qualified Network.HTTP2                as NH2


import           Rede.Http2.Session
import           Rede.MainLoop.CoherentWorker (CoherentWorker)
import qualified Rede.MainLoop.Framer         as F
import           Rede.MainLoop.PushPullType   (Attendant, PullAction,
                                               PushAction)
import           Rede.Utils                   (Word24, word24ToInt)


wrapSession :: CoherentWorker -> Attendant
wrapSession coherent_worker push_action pull_action = do

    -- ?
    let session_start = SessionStartData {}

    (session_input, session_output) <- http2Session coherent_worker session_start

    forkIO $ inputGatherer pull_action session_input 
    forkIO $ outputGatherer push_action session_output

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
    -- Can I get a whole packer?
    let source = F.readNextChunk http2FrameLength "" pull_action
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

    command_or_frame  <- getFrameFromSession session_output

    case command_or_frame of 

        Left cmd -> do 
            -- TODO: This is just a quickie behavior I dropped 
            -- here, semantics need to be different probably.
            putStrLn $ "Received a command... terminating " ++ (show cmd)

        Right (p1, p2) -> do 
            -- TODO: This can be optimized.... 
            let bs = LB.fromStrict $ NH2.encodeFrame p1 p2
            push_action bs 

    outputGatherer push_action session_output


{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
             FlexibleInstances, MultiParamTypeClasses,
             TemplateHaskell
 #-}

{- 

This module plugs a worker (check module ...MainLoop.Tokens, and there type StreamWorker) 
and adapts it so that it speaks in frames. So, it implements translation between the 
generic tokens present at Tokens.hs and the frames. 

-}

module Rede.Http2.Streams.State(
    initStreamState
    ,inputPlug

    ,StreamStage
    ,StreamStateT
    ) where 


import qualified Blaze.ByteString.Builder            as Bu
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Lens
import qualified Control.Lens                        as L
-- import           Control.Monad.Morph                 (MFunctor)
-- import           Data.BitSet.Generic                 (singleton)
import qualified Data.ByteString                     as B
-- import qualified Data.ByteString.Lazy                as LB
import           Data.Conduit
-- import           Data.Conduit.Lift                   (readerC)
-- import           Data.Default
import qualified Data.HashTable.IO                   as H
import           Data.IORef

import           Data.Monoid                         (mappend, mempty)

-- import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader



-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HTTP2            as NH2
import qualified Network.HPACK            as HP

-- import           Rede.MainLoop.StreamPlug (
--                                           StreamId (..), 
--                                           -- StreamPlug (..)
--                                           )
import           Rede.MainLoop.Tokens     (StreamInputToken(..)
                                           , StreamOutputAction(..)
                                           , UnpackedNameValueList(..)
                                           )



-- A dictionary from local id to (global) stream id
type PushStreamsHashTable = H.BasicHashTable Int Int


-- * Stream state

-- | Section 5.1 of HTTP/2 spec... there are seven stream stages...
--   High level view
data StreamStage = 
     Idle_StS                  -- ^  Default state, stream has never been seen
    |ReservedLocal_StS         -- ^ This side started a push
    |ReservedRemote_StS        -- ^ The other side started a push
    |HalfClosedRemote_StS      -- ^ The other side can not send data
    |HalfClosedLocal_StS       -- ^ This side can not send data
    |Open_StS                  -- ^ Both sides can send data
    |Closed_StS                -- ^ Neither side can send data, the stream id can't be used again


-- | This state machine is what results from interpreting END_STREAM and END_HEADER flags 
--   in the spec
data StreamEvent = 
     HarmlessHeader_SE           -- A header or continuation without any END_* flag set
    |HeadersAndStreamEnds_SE     -- A header with both END_* flags set
    |HeadersEnd_SE               -- A header or continuation with END_HEADERS set
    |StreamEnds_SE               -- A header or data with END_STREAM


-- | I'm ignoring 1xx pieces here which are specifically allowed by the HTTP/2 spec.
data StreamLifeCycle = 
    StreamIdle_SaR             
    |HeadersCome_SaR
    |DataCome_SaR
    |FinalHeadersCome_SaR
    |StreamClosing_SaR
    |StreamClosed_SaR



-- A simple state machine....
nextStageAtReception ::   StreamLifeCycle         -> StreamEvent              -> StreamLifeCycle
nextStageAtReception      StreamIdle_SaR            HarmlessHeader_SE           = HeadersCome_SaR 
nextStageAtReception      HeadersCome_SaR           HeadersEnd_SE               = DataCome_SaR
nextStageAtReception      HeadersCome_SaR           HarmlessHeader_SE           = HeadersCome_SaR
nextStageAtReception      _                         HeadersAndStreamEnds_SE     = StreamClosed_SaR
nextStageAtReception      StreamIdle_SaR            HeadersEnd_SE               = DataCome_SaR
nextStageAtReception      StreamIdle_SaR            StreamEnds_SE               = StreamClosing_SaR
nextStageAtReception      StreamClosing_SaR         HarmlessHeader_SE           = StreamClosed_SaR
nextStageAtReception      DataCome_SaR              StreamEnds_SE               = StreamClosing_SaR
-- Todo: Throw exceptions here....
nextStageAtReception      DataCome_SaR              HarmlessHeader_SE           = error "This header should terminate the stream"
nextStageAtReception      StreamClosing_SaR         StreamEnds_SE               = error "Stream was already signaled for ending"
nextStageAtReception      DataCome_SaR              HeadersEnd_SE               = error "That's a protocol error: stream should also end"



-- Shared information between the input plug (the translator from frames 
-- to StreamInput tokens) and the output plug (the translator from OutputAction to
-- output frames).
-- 
--  This resource should be created by the session manager when a new 
--  stream is established.
--
--    IORef variables denote "streamlocal" state
--    MVar variables denote state that is shared with other streams.
data StreamState = StreamState {
    _stage      :: IORef StreamStage

    -- This main stream Id. This should be an odd number, because
    -- the stream is always started by the browser
    , _sstreamId  :: Int

    -- Did I Acknowledged the string already with SynReply?
    , _mustAck    :: IORef Bool

    -- What should I do when this stream finishes?
    , _finalizer  :: IO ()

    -- A dictionary from local to global id of the streams...
    -- I need this because this stream may need to refer to 
    -- other streams, as dependent ones for example. 
    , _pushStreamHashTable :: PushStreamsHashTable  

    -- What should be the id of the next pushed stream?
    -- I may need this when pushing stuff....
    , _nextPushStreamId  :: MVar Int 

    , _streamLifeCycle :: IORef StreamLifeCycle

    -- The decoding context for the entire session. If the implementation is OK, this 
    -- should never block. Notice that this MVar is shared among all the streams (it is 
    -- for the entire session), so be careful when using...
    , _headersDecoding :: MVar HP.DynamicTable
}

L.makeLenses ''StreamState

type StreamStateT = ReaderT StreamState


-- Feeds some data to the monad and runs it.... 
initStreamState :: MonadIO m => Int -> (IO () ) -> MVar Int -> MVar HP.DynamicTable ->StreamStateT m a -> m a 
initStreamState stream_id finalizer next_pushed_stream headers_decoding sm  = do 
    stage_ref        <- liftIO $ newIORef Open_StS
    must_ack_ref     <- liftIO $ newIORef True
    push_stream_hash <- liftIO $ H.new 

    let stream_state = StreamState {

        -- Is this correct?
        _stage      = stage_ref

        ,_sstreamId = stream_id 
        ,_mustAck   = must_ack_ref

        ,_finalizer = finalizer
        ,_pushStreamHashTable = push_stream_hash
        ,_nextPushStreamId = next_pushed_stream
        ,_headersDecoding = headers_decoding
        }
    runReaderT sm stream_state


-- Not sure about this typedef yet....at any rate, when this is in 
-- use, other streams should be blocked waiting for its release
type DecoderSharedState = MVar HP.DynamicTable 


-- Simple alias for another function. The plug takes a few frames and then exits. That what it 
-- is supposed to do.
-- makeInputPlug        :: DecoderSharedState -> Conduit NH2.Frame (StreamStateT IO) StreamInputToken
-- makeInputPlug = startStream


-- outputPlug = error "Not implemented"


classifyFrame :: NH2.Frame -> NH2.FrameTypeId 
classifyFrame (NH2.Frame _ payload) =  NH2.framePayloadToFrameTypeId payload


frameOpensStream :: NH2.Frame -> Bool 
frameOpensStream frame = (classifyFrame frame) == NH2.FrameHeaders


-- We can deal with the typical GET first.... so the first kind of frame we need 
-- to take care of is the HEADERS frame and 
inputPlug  :: Conduit NH2.Frame (StreamStateT IO) StreamInputToken
inputPlug  = do 
    headers_decoding_mvar <- lift $ view headersDecoding
    headers_decoding <- liftIO $ takeMVar headers_decoding_mvar
    new_headers_decoding <- startStreamOrFailWithBuilder headers_decoding mempty
    liftIO $ putMVar headers_decoding_mvar new_headers_decoding

  where 
    startStreamOrFailWithBuilder :: HP.DynamicTable 
                                    -> Bu.Builder 
                                    --          v- input  v- output        v - *->* monad    v- result type
                                    -> ConduitM NH2.Frame StreamInputToken (StreamStateT IO) HP.DynamicTable
    startStreamOrFailWithBuilder headers_decoding builder = 
        do 
            maybe_frame <- await 

            case maybe_frame of 

                Just frame -> 
                    case headerBlockFromFrame frame  of 

                        Just block_fragment  | not (frameEndsHeaders frame) -> 
                            startStreamOrFailWithBuilder  
                                headers_decoding
                                (builder `mappend` (fromByteString block_fragment))

                        -- TODO: Analyze stream end flags and change states and all of that.
                        -- If we are arriving here, this frame ends the headers and we can get into the 
                        -- business of passing those headers downstream. 
                        Just block_fragment                              -> do 
                            let headers_block = Bu.toByteString $ builder `mappend` (fromByteString block_fragment)

                            (new_dynamic_table, headers) <- liftIO $ HP.decodeHeader headers_decoding headers_block 
                            yield $ Headers_STk $ UnpackedNameValueList headers 
                            return new_dynamic_table

                        Nothing                                          ->
                            -- How typical of a fragment this would be? -- This is 
                            -- actually a RuntimeError, but for now I'm treating it as an internal one...
                            error "Headers don't finish correctly, a following frame has been found"

                Nothing      -> 
                            error "Headers didn't finish, no following frame has been found"
                    


    -- case frame of 
    -- Flags here are a little bit disjoint... will have to live with that.
    -- ( AnyControl_AF (SynStream_ACF synstream) ) -> let 
    --        UncompressedKeyValueBlock  unvl                       = getCompressedHeaders synstream
    --        stream_unidirectional                                 = getFrameFlag synstream SyS.Unidirectional_SSVF
    --        stream_fin                                            = getFrameFlag synstream SyS.Fin_SSVF
    --     in 
    --       do 
    --         case (stream_unidirectional, stream_fin) of 
    --             (True, True)       ->  setCurrentStreamStage  Closed_StS
    --             (True, False)      ->  setCurrentStreamStage  CanOnlyReceive_StS
    --             (False, True)      ->  setCurrentStreamStage  CanOnlySend_StS
    --             (False, False)     ->  setCurrentStreamStage  Open_StS
    --         -- liftIO $ putStrLn "Frame translated"
    --         return $ Headers_STk unvl
  -- where 


outputPlug :: Conduit StreamOutputAction (StreamStateT IO) NH2.Frame 
outputPlug = do 
    maybe_action <- await 
    current_stream_id <- lift $ view sstreamId
    case maybe_action of 

        -- Stream worker asks to send headers
        Just (SendHeaders_SOA unmvl) -> do 
            -- TODO: MUST check that the stream be openened to 
            --       send, take measure if not!!

            case current_stage of 

                StreamIdle_SaR  -> do
                    _sendHeadersFrame current_stream_id unmvl
                    advanceState HarmlessHeader_SE

                    -- good!
                    continue 

                HeadersCome_SaR -> do
                    _sendContinuationFrame current_stream_id unmvl
                    advanceState HarmlessHeader_SE

                    -- good!
                    continue 

                DataCome_SaR -> do 
                    _sendEndStreamHeadersFrame  current_stream_id unmvl
                    advanceState StreamEnds_SE

                    -- good!
                    continue 

                StreamClosing_SaR -> do 
                    _sendContinuationFrame current_stream_id unmvl
                    advanceState HarmlessHeader_SE

                    -- good!
                    continue 

                _                -> 
                    error "Throw a proper exception here, but can't send headers on the stream"


        -- Stream worker asks to send data 
        Just (SendData_SOA bs_data)     -> do
            error "-- TODO: Continue here"

        Just Finish_SOA                 -> do
            error "-- TODO: Continue here"

        -- Let's work with associated streams... 
        Just (SendAssociatedHeaders_SOA local_stream_id unmvl) -> do
            -- Sparks this to be sent on another stream... using this construct 
            -- will automatically close this stream for new headers!!

            _sendToAssociatedStream local_stream_id $ SendHeaders_SOA unmvl

            -- Get a global id for this dear ... 
            -- stream_id        <- lift getStreamId
            -- global_stream_id <- lift $ getGlobalPushStreamId local_stream_id 

            -- -- Now, I need to open a new stream for this.... 
            -- anyframe         <- lift $ prepareSynStreamFrame global_stream_id stream_id unmvl
            -- yield anyframe

            -- outputPlug


        Just (SendAssociatedData_SOA local_stream_id contents)  -> do 
            -- -- Global id 
            -- global_stream_id <- lift $ getGlobalPushStreamId local_stream_id

            -- -- anyframe         <- return $ prepareDataFrame contents global_stream_id
            -- -- yield anyframe
            -- yieldDataFramesFromData contents global_stream_id
            -- outputPlug


        Just (SendAssociatedFinish_SOA local_stream_id)  -> do 
            -- global_stream_id  <- lift $ getGlobalPushStreamId local_stream_id
            -- yield $ prepareFinishDataFrame global_stream_id
            -- outputPlug


        -- Just something_else             -> do 
        --     liftIO $ putStrLn $ "Got: " ++ (show something_else)
        --     outputPlug

        Nothing                         -> 
            return ()
  where 
    -- Simple way of going tail-recursive
    continue = outputPlug

    advanceState :: StreamEvent -> Conduit StreamOutputAction (StreamStateT IO) NH2.Frame 
    advanceState stream_event = do 
        current_stage_ioref <- view streamLifeCycle
        liftIO $ do 
            current_stage <- readIORef current_stage_ioref
            let next_stage_at_reception = nextStageAtReception current_stage stream_event
            writeIORef current_stage_ioref next_stage_at_reception



headerBlockFromFrame                                          :: NH2.Frame -> Maybe B.ByteString
headerBlockFromFrame (NH2.Frame _ ( NH2.HeadersFrame _ block_fragment   ) )= Just block_fragment
headerBlockFromFrame (NH2.Frame _ ( NH2.ContinuationFrame block_fragment) )= Just block_fragment
headerBlockFromFrame _                                        = Nothing 


frameEndsHeaders  :: NH2.Frame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


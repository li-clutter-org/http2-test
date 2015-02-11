{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

{- 

This module plugs a worker (check module ...MainLoop.Tokens, and there type StreamWorker) 
and adapts it so that it speaks in frames. So, it implements translation between the 
generic tokens present at Tokens.hs and the frames. 

-}

module Rede.Http2.Streams.State(
    ) where 


import           Control.Monad.Morph               (MFunctor)
import           Data.BitSet.Generic               (singleton)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as LB
import qualified Data.ByteString.Builder           as Bu
import           Data.Conduit
import           Data.Default
import qualified Data.HashTable.IO                 as H
import           Data.IORef
import           Data.Monoid                       (mappend)

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader



-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HTTP2            as NH2
import qualified Network.HPACK            as HP

import           Rede.MainLoop.StreamPlug (
                                          StreamId (..), 
                                          -- StreamPlug (..)
                                          )
import           Rede.MainLoop.Tokens     (StreamInputToken(..), StreamOutputAction(..), UnpackedNameValueList)



-- A dictionary from local id to (global) stream id
type PushStreamsHashTable = H.BasicHashTable Int Int


-- * Stream state

-- | Section 5.1 of HTTP/2 spec... there are seven stream stages
data StreamStage = 
     Idle_StS                  -- ^  Default state, stream has never been seen
    |ReservedLocal_StS         -- ^ This side started a push
    |ReservedRemote_StS        -- ^ The other side started a push
    |HalfClosedRemote_StS      -- ^ The other side can not send data
    |HalfClosedLocal_StS       -- ^ This side can not send data
    |Open_StS                  -- ^ Both sides can send data
    |Closed_StS                -- ^ Neither side can send data, the stream id can't be used again


data StreamState = StreamState {
    stage      :: IORef StreamStage

    -- This main stream Id. This should be an odd number, because
    -- the stream is always started by the browser
    , sstreamId  :: Int

    -- Did I Acknowledged the string already with SynReply?
    , mustAck    :: IORef Bool

    -- What should I do when this stream finishes?
    , finalizer  :: IO ()

    -- A dictionary from local to global id of the streams...
    -- I need this because this stream may need to refer to 
    -- other streams, as dependent ones for example. 
    , pushStreamHashTable :: PushStreamsHashTable  

    -- What should be the id of the next pushed stream?
    -- I may need this when pushing stuff....
    , nextPushStreamId :: MVar Int 

    -- The decoding context for the entire session. If the implementation is OK, this 
    -- should never block 
    , headersDecoding :: MVar HP.DynamicTable
}


newtype MonadIO m => StreamStateT m a = StreamStateT (ReaderT StreamState m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)


instance MonadIO m => MonadIO (StreamStateT m) where 
    -- liftIO :: IO a -> m a 
    liftIO iocomp = StreamStateT (liftIO iocomp)     


-- instance StreamPlug (StreamStateT IO) NH2.Frame where 

-- Not sure about this typedef yet....at any rate, when this is in 
-- use, other streams should be blocked waiting for its release
type DecoderSharedState = MVar HP.DynamicTable 


-- Simple alias for another function. The plug takes a few frames and then exits. That what it 
-- is supposed to do.
makeInputPlug        :: DecoderSharedState -> Conduit NH2.Frame (StreamStateT IO) StreamInputToken
makeInputPlug = startStream


-- outputPlug = error "Not implemented"


classifyFrame :: NH2.Frame -> NH2.FrameTypeId 
classifyFrame (NH2.Frame _ payload) =  NH2.framePayloadToFrameTypeId payload


frameOpensStream :: NH2.Frame -> Bool 
frameOpensStream frame = (classifyFrame frame) == NH2.FrameHeaders


-- We can deal with the typical GET first.... so the first kind of frame we need 
-- to take care of is the HEADERS frame and 
startStream  :: DecoderSharedState -> Conduit NH2.Frame (StreamStateT IO) StreamInputToken
startStream  decoder_shared_state =  
    startStreamOrFailWithBuilder $ Bu.byteString ""
  where 
    startStreamOrFailWithBuilder builder = 
        do 
            maybe_frame <- await 

            case maybe_frame of 

                Just frame -> 
                    case headerBlockFromFrame frame  of 

                        Just block_fragment  | not (frameEndsHeaders frame) -> 
                            startStreamOrFailWithBuilder  (builder `mappend` (Bu.byteString block_fragment))

                        -- TODO: Analyze stream end flags and change states and all of that.
                        -- If we are arriving here, this frame ends the headers and we can get into the 
                        -- business of passing those headers downstream. 
                        Just block_fragment                              -> do 
                            let headers_block = LB.toStrict $ Bu.toLazyByteString ( builder `mappend` (Bu.byteString block_fragment))
                            headers <- lift $ headersFrom headers_block 
                            yield $ Headers_STk headers 
                            return ()

                        Nothing                                          ->
                            -- How typical of a fragment this would be?
                            return ()

                Nothing      -> 
                    return () 


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


headerBlockFromFrame                                          :: NH2.Frame -> Maybe B.ByteString
headerBlockFromFrame (NH2.Frame _ ( NH2.HeadersFrame _ block_fragment   ) )= Just block_fragment
headerBlockFromFrame (NH2.Frame _ ( NH2.ContinuationFrame block_fragment) )= Just block_fragment
headerBlockFromFrame _                                        = Nothing 


frameEndsHeaders  :: NH2.Frame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


headersFrom :: MonadIO m => B.ByteString -> StreamStateT m UnpackedNameValueList
headersFrom header_block = error "" 
    -- First get the decoding context...



-- * Functions carried over from the SPDY implementation
ioSet :: MonadIO m => (StreamState -> IORef a) -> a -> StreamStateT m ()
ioSet member new_val  = StreamStateT $ do 
    io_ref <- asks member
    liftIO $ writeIORef io_ref new_val


ioGet :: MonadIO m => (StreamState -> IORef a) -> StreamStateT m a
ioGet member = StreamStateT $ do 
    io_ref <- asks member
    liftIO $ readIORef io_ref
 

-- Member get/set from the stream state computation monad 
getCurrentStreamStage :: MonadIO m => StreamStateT m  StreamStage
getCurrentStreamStage = ioGet stage

setCurrentStreamStage :: MonadIO m => StreamStage -> StreamStateT m () 
setCurrentStreamStage = ioSet stage

getStreamId :: MonadIO m => StreamStateT m Int 
getStreamId = StreamStateT $ asks sstreamId

setMustAck  :: MonadIO m => Bool -> StreamStateT m ()
setMustAck = ioSet mustAck

getMustAck :: MonadIO m => StreamStateT m Bool 
getMustAck = ioGet mustAck

streamFinalize :: MonadIO m => StreamStateT m ()
streamFinalize = StreamStateT $ do 
    fin <- asks finalizer
    liftIO fin


initStreamState :: MonadIO m => Int -> (IO () ) -> MVar Int -> StreamStateT m a -> m a 
initStreamState stream_id fin next_pushed_stream (StreamStateT sm)  = do 
    s <- liftIO $ defaultStreamState stream_id fin next_pushed_stream
    runReaderT sm s


defaultStreamState :: Int -> (IO () ) ->  MVar Int -> IO StreamState 
defaultStreamState stream_id fin next_push_id = do
    stage_ioref <- newIORef Closed_StS
    ma                     <- newIORef True
    push_stream_hash_table <- H.new  
    return $ StreamState {
        stage                  = stage_ioref
        ,sstreamId             = stream_id
        ,mustAck               = ma
        ,finalizer             = fin
        ,pushStreamHashTable   = push_stream_hash_table
        ,nextPushStreamId      = next_push_id
    }


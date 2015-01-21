{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Rede.SpdyProtocol.Streams.State(
    defaultStreamState
    ,unpackRecvHeaders
    ,initStreamState
    ,packSendHeaders
    ,streamFinalize

    ,StreamStage(..)
    ,StreamState(..)
    ,StreamStateT(..)
    ) where 


import           Data.IORef
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import           Data.Default
import           Control.Monad.Morph       (MFunctor)
import qualified Data.Streaming.Zlib       as Z
-- import           System.FilePath           ((</>))
import           Data.Binary.Get           (runGet)
import           Data.Binary.Put           (runPut)
import qualified Data.Binary               as Bi
import           Data.Conduit
import           Data.BitSet.Generic(singleton)

import           Control.Exception(throwIO)
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Concurrent.MVar


-- import Rede.MainLoop.ConfigHelp(configDir)
import           Rede.MainLoop.StreamPlug
import           Rede.MainLoop.Tokens
import           Rede.SpdyProtocol.Framing.AnyFrame
import           Rede.SpdyProtocol.Framing.Frame
-- import           Rede.SpdyProtocol.Framing.GoAway
import           Rede.SpdyProtocol.Framing.KeyValueBlock
import           Rede.SpdyProtocol.Framing.SynReply
import           Rede.SpdyProtocol.Framing.Headers
import qualified Rede.SpdyProtocol.Framing.SynStream     as SyS
import           Rede.SpdyProtocol.Framing.DataFrame
import qualified Rede.SpdyProtocol.Framing.DataFrame     as DF


data StreamStage =
     Open_StS
    |CanOnlySend_StS
    |CanOnlyReceive_StS
    |Closed_StS
    ;


canSend :: StreamStage -> Bool 
canSend Open_StS             = True 
canSend CanOnlySend_StS      = True 
canSend _                    = False


data WindowBookkeeping = WindowBookkeeping {
      pastBytes   :: Int 
    , futureBytes :: Int 
}


instance Default WindowBookkeeping where 
    def = WindowBookkeeping {
        pastBytes = 0
        ,futureBytes = 0
    }


-- Due to the use of Z here, this struct only
-- makes sense inside the IO monad (or equivalent)
data StreamState = StreamState {
      stage      :: IORef StreamStage

    -- Not making much use of these right now...
    , receiveWin :: IORef WindowBookkeeping
    , sendWin    :: IORef WindowBookkeeping 

    -- These ones  are shared for the whole session
    , sendZlib   :: MVar Z.Deflate
    , recvZlib   :: MVar Z.Inflate 

    -- This main stream Id. This should be an odd number, because
    -- the stream is always started by the browser
    , sstreamId  :: Int

    -- Did I Acknowledged the string already with SynReply?
    , mustAck    :: IORef Bool

    -- What should I do when this stream finishes?
    , finalizer  :: IO ()

    -- Often, a subordinate stream will be also active. In the current 
    -- implementation, subordinate (pushed) streams are sequentialized 
    -- and completely nested on the main thread, so, for now at least, 
    -- we can warantee that there is only this one active...
    , currentPushStream :: IORef Int  

    -- What should be the id of the next pushed stream?
    , nextPushStreamId :: MVar Int 
    }


newtype MonadIO m => StreamStateT m a = StreamStateT (ReaderT StreamState m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor)


instance MonadIO m => MonadIO (StreamStateT m) where 
    -- liftIO :: IO a -> m a 
    liftIO iocomp = StreamStateT (liftIO iocomp) 


instance StreamPlug (StreamStateT IO) AnyFrame where
    inputPlug = do 
        maybe_anyframe <- await 
        case maybe_anyframe of
            Just anyframe -> do 
                stream_input_token <- lift $ anyFrameToInput anyframe
                yield stream_input_token
                inputPlug
            -- TODO: Close StreamWorker? 
            _             -> return ()

    outputPlug = do 
        maybe_action <- await 
        case maybe_action of 

            -- Stream worker asks to send headers
            Just (SendHeaders_SOA unmvl) -> do 
                -- TODO: MUST check that the stream be openened to 
                --       send, take measure if not!!
                current_stage <- lift getCurrentStreamStage  
                must_ack      <- lift getMustAck 
                case (current_stage, must_ack) of 

                    (s, True ) | canSend s -> do
                        any_frame <- lift $ prepareSynReplyFrame unmvl
                        yield any_frame

                    (s, False) | canSend s -> do
                        anyframe <- lift $ prepareHeadersFrame unmvl
                        yield anyframe
                    -- TODO: 10 000 000 cases more here that need to be populated...
                if must_ack 
                    then lift $ setMustAck False 
                    else return ()

                outputPlug

            -- Stream worker asks to send data 
            Just (SendData_SOA bs_data)     -> do
                liftIO $ putStrLn "Send main stream data"
                current_stage <- lift getCurrentStreamStage
                must_ack      <- lift getMustAck
                stream_id     <- lift getStreamId
                case (current_stage, must_ack) of 
                    (s,True) | canSend s -> do 
                        anyframe <- lift $ prepareSynReplyFrame (UnpackedNameValueList [])
                        yield anyframe
                        --yield $ prepareDataFrame bs_data stream_id
                        lift $ setMustAck False
                    (s,False) | canSend s -> do 
                        yield $ prepareDataFrame bs_data stream_id
                outputPlug

            Just Finish_SOA                 -> do
                current_stage <- lift getCurrentStreamStage
                must_ack      <- lift getMustAck
                stream_id     <- lift getStreamId
                case (current_stage, must_ack) of 

                    -- I'm finishing without sending any headers
                    (s,True) | canSend s -> do 
                        anyframe <- lift $ prepareSynReplyFrameAndFinish (UnpackedNameValueList [])
                        yield anyframe
                        lift $ setMustAck False

                    (s,False) | canSend s -> do 
                        yield $ prepareFinishDataFrame stream_id

                    _                     -> do

                        liftIO $ putStrLn "Protocol error: trying to send on closed stream"

                outputPlug

            Just something_else             -> do 
                liftIO $ putStrLn $ "Got: " ++ (show something_else)
                outputPlug

            Nothing                         -> 
                return ()


prepareDataFrame :: B.ByteString -> Int -> AnyFrame
prepareDataFrame bs_data stream_id = DataFrame_AF $ DataFrame {
                            DF.streamId        = stream_id
                            ,DF.dataFrameFlags = fbs0
                            ,DF.payload        = bs_data
                            }


prepareFinishDataFrame :: Int -> AnyFrame
prepareFinishDataFrame stream_id = DataFrame_AF $ DataFrame {
                            DF.streamId        = stream_id
                            ,DF.dataFrameFlags = singleton DF.Fin_F
                            ,DF.payload        = ""
                            }


anyFrameToInput :: MonadIO m => AnyFrame ->  StreamStateT m StreamInputToken
anyFrameToInput any_frame = 

    case any_frame of 

        ( AnyControl_AF (SynStream_ACF synstream) ) -> let 
               CompressedKeyValueBlock compressed_headers_bytestring = getCompressedHeaders synstream
               stream_unidirectional                                 = getFrameFlag synstream SyS.Unidirectional_SSVF
               stream_fin                                            = getFrameFlag synstream SyS.Fin_SSVF
            in 
              do 
                unvl <- unpackRecvHeaders compressed_headers_bytestring 
                case (stream_unidirectional, stream_fin) of 
                    (True, True)       ->  setCurrentStreamStage  Closed_StS
                    (True, False)      ->  setCurrentStreamStage  CanOnlyReceive_StS
                    (False, True)      ->  setCurrentStreamStage  CanOnlySend_StS
                    (False, False)     ->  setCurrentStreamStage  Open_StS
                liftIO $ putStrLn "Frame translated"
                return $ Headers_STk unvl


prepareSynReplyFrame :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareSynReplyFrame unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    syn_reply          <- return $ SynReplyFrame def stream_id compressed_headers
    return (AnyControl_AF (SynReplyFrame_ACF syn_reply))   


prepareHeadersFrame :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareHeadersFrame unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    headers_frame      <- return $ HeadersFrame def stream_id compressed_headers
    return $ wrapCF headers_frame 

prepareSynReplyFrameAndFinish :: UnpackedNameValueList -> StreamStateT IO AnyFrame 
prepareSynReplyFrameAndFinish unmvl = do 
    compressed_headers <- packSendHeaders unmvl
    stream_id          <- getStreamId
    syn_reply          <- return $ SynReplyFrame finish_prologue stream_id compressed_headers
    return (AnyControl_AF (SynReplyFrame_ACF syn_reply))
  where 
    finish_prologue = ControlFrame SynReply_CFT (fbs1 Fin_SRVF) 0   


unpackRecvHeaders :: MonadIO m => B.ByteString -> StreamStateT m UnpackedNameValueList
unpackRecvHeaders compressed_stuff = do 
    recv_zlib_mvar         <- StreamStateT $ asks recvZlib
    liftIO $ withMVar recv_zlib_mvar $ \ recv_zlib ->  do
        popper             <- Z.feedInflate recv_zlib compressed_stuff
        list_piece_1       <- exhaustPopper popper 
        latest             <- Z.flushInflate recv_zlib
        uncompressed_bytes <- return $ B.concat (list_piece_1 ++ [latest])  
        return $ runGet Bi.get $ LB.fromChunks [uncompressed_bytes]
   

exhaustPopper :: Z.Popper   -> IO [B.ByteString]
exhaustPopper popper = do 
    x                  <- popper 
    case x of 
        Z.PRDone            -> return []

        Z.PRNext bytestring -> do 
            more <- exhaustPopper popper 
            return $ (bytestring:more)

        Z.PRError e         -> do 
            -- When this happens, the only sensible 
            -- thing to do is throw an exception, and trash the entire 
            -- stream.... 
            throwIO  e


packSendHeaders :: MonadIO m => UnpackedNameValueList -> StreamStateT m CompressedKeyValueBlock
packSendHeaders uncompressed_uvl = do 
    uncompressed_bytes <- return $ LB.toStrict $ runPut $ Bi.put $ uncompressed_uvl
    send_zlib_mvar <- StreamStateT $ asks sendZlib
    liftIO $ do
        withMVar send_zlib_mvar $ \ send_zlib -> do
            popper       <-  Z.feedDeflate send_zlib uncompressed_bytes
            list_piece_1 <-  exhaustPopper popper 
            latest_piece <-  exhaustPopper $ Z.flushDeflate send_zlib
            return $ CompressedKeyValueBlock $ B.concat (list_piece_1 ++ latest_piece)
    


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


initStreamState :: MonadIO m => Int -> (IO () ) -> MVar Z.Deflate -> MVar Z.Inflate -> MVar Int -> StreamStateT m a -> m a 
initStreamState stream_id fin send_zlib recv_zlib next_pushed_stream (StreamStateT sm)  = do 
    s <- liftIO $ defaultStreamState stream_id fin send_zlib recv_zlib next_pushed_stream
    runReaderT sm s


defaultStreamState :: Int -> (IO () ) -> MVar Z.Deflate -> MVar Z.Inflate -> MVar Int -> IO StreamState 
defaultStreamState stream_id fin sendZlib_ recvZlib_ next_push_id = do
    stage_ioref <- newIORef Closed_StS
    rw                  <- newIORef def 
    sw                  <- newIORef def 
    ma                  <- newIORef True
    current_push_stream <- newIORef 0 
    return $ StreamState {
        stage              = stage_ioref
        ,receiveWin        = rw 
        ,sendWin           = sw 
        ,sendZlib          = sendZlib_
        ,recvZlib          = recvZlib_
        ,sstreamId         = stream_id
        ,mustAck           = ma
        ,finalizer         = fin
        ,currentPushStream = current_push_stream
        ,nextPushStreamId  = next_push_id
    }


{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell #-}
module Rede.Http2.Session(
    http2Session
    ,getFrameFromSession
    ,sendFrametoSession
    ) where


import qualified Blaze.ByteString.Builder            as Bu
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Control.Lens
import qualified Control.Lens                        as L


-- No framing layer here... let's use Kazu's Yamamoto library
import qualified Network.HTTP2            as NH2
import qualified Network.HPACK            as HP


import           Control.Monad                           (forever, liftM)
import           Control.Lens
import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Reader
import           Control.Exception(throwIO)

import           Data.Conduit
import           Data.Conduit.List                       (foldMapM)
import qualified Data.Streaming.Zlib                     as Z

-- import           Data.Conduit.Lift                       (distribute)
-- import qualified Data.Conduit.List                       as CL
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as LB
import           Data.ByteString.Char8                   (pack)
import           Control.Concurrent.MVar
import           Data.Void                               (Void)
import           Data.Default                            (def)

import           Data.Monoid
import qualified Data.Map                                as MA
import qualified Data.IntSet                             as NS
import           Data.Binary.Put                         (runPut)
import qualified Data.Binary                             as Bi
import           Data.Binary.Get                         (runGet)
import qualified Data.HashTable.IO          as H
import qualified Data.Dequeue               as D


import           Rede.MainLoop.CoherentWorker 
import           Rede.MainLoop.Tokens


type Frame = NH2.Frame


-- Whatever a worker thread is going to need comes here.... 
-- this is to make refactoring easier, but not strictly needed. 
data WorkerThreadEnvironment = WorkerThreadEnvironment {
    -- What's the header stream id?
    _streamId :: GlobalStreamId

    -- A full block of headers can come here
    , _headersOutput :: Chan (GlobalStreamId,Headers)

    -- And regular contents can come this way and thus be properly mixed
    -- with everything else.... for now... 
    ,_dataOutput :: Chan (GlobalStreamId, B.ByteString)

    ,_streamsCancelled_WTE :: MVar NS.IntSet

    }

makeLenses ''WorkerThreadEnvironment


-- Basically a couple of channels ... 
type Session = (SessionInput, SessionOutput)


-- From outside, one can only write to this one ... the newtype is to enforce 
--    this.
newtype SessionInput = SessionInput ( Chan (Either SessionInputCommand Frame) )
sendFrametoSession :: SessionInput  -> Frame -> IO ()
sendFrametoSession (SessionInput chan) frame = writeChan chan $ Right frame


-- From outside, one can only read from this one 
newtype SessionOutput = SessionOutput ( Chan (Either SessionOutputCommand Frame) )
getFrameFromSession :: SessionOutput -> IO (Either SessionOutputCommand Frame) 
getFrameFromSession (SessionOutput chan) = readChan chan


-- Here is how we make a session 
type SessionMaker = SessionStartData -> IO Session


-- Here is how we make a session wrapping a CoherentWorker
type CoherentSession = CoherentWorker -> SessionMaker 


type HashTable k v = H.CuckooHashTable k v


-- Blaze builder could be more proper here... 
type Stream2HeaderBlockFragment = HashTable GlobalStreamId B.ByteString


type WorkerMonad = ReaderT WorkerThreadEnvironment IO 


-- Have to figure out which are these...but I would expect to have things
-- like unexpected aborts here in this type.
data SessionInputCommand = 
    NullCmd_SIC


-- temporary
data  SessionOutputCommand = 
    NullCmd_SOC

-- TODO: Put here information needed for the session to work
data SessionStartData = SessionStartData {
    
    }

makeLenses ''SessionStartData


data SessionData = SessionData {
    _sessionInput                :: Chan (Either SessionInputCommand Frame)
    ,_sessionOutput              :: Chan (Either SessionOutputCommand Frame)

    -- Use to encode 
    ,_toEncodeHeaders            :: MVar HP.DynamicTable
    -- And used to decode
    ,_toDecodeHeaders            :: MVar HP.DynamicTable

    -- Used for decoding the headers
    ,_stream2HeaderBlockFragment :: Stream2HeaderBlockFragment

    -- Used for worker threads
    ,_forWorkerThread            :: WorkerThreadEnvironment

    ,_coherentWorker             :: CoherentWorker

    -- Some streams may be cancelled 
    ,_streamsCancelled :: MVar NS.IntSet
    }


makeLenses ''SessionData


flippedRunReader :: r -> ReaderT r m a -> m a
flippedRunReader  = (flip runReaderT)


--                                v- {headers table size comes here!!}
http2Session :: CoherentWorker -> SessionStartData -> IO Session
http2Session coherent_worker _ =   do 
    session_input   <- newChan
    session_output  <- newChan


    -- For incremental construction of headers...
    stream_request_headers <- H.new :: IO Stream2HeaderBlockFragment

    -- Warning: we should find a way of coping with different table sizes.
    decode_headers_table <- HP.newDynamicTableForDecoding 4096
    decode_headers_table_mvar <- newMVar decode_headers_table

    encode_headers_table <- HP.newDynamicTableForEncoding 4096
    encode_headers_table_mvar <- newMVar decode_headers_table

    -- These ones need independent threads taking care of sending stuff
    -- their way... 
    headers_output <- newChan :: IO (Chan (GlobalStreamId,Headers))
    data_output <- newChan :: IO (Chan (GlobalStreamId,B.ByteString))

    -- What about stream cancellation?
    cancelled_streams_mvar <- newMVar $ NS.empty :: IO (MVar NS.IntSet)

    let for_worker_thread = WorkerThreadEnvironment {
        _streamId = error "NotInitialized"  
        ,_headersOutput = headers_output
        ,_dataOutput = data_output
        ,_streamsCancelled_WTE = cancelled_streams_mvar
        }

    let session_data  = SessionData {
        _sessionInput                = session_input 
        ,_sessionOutput              = session_output
        ,_toDecodeHeaders            = decode_headers_table_mvar
        ,_toEncodeHeaders            = encode_headers_table_mvar
        ,_stream2HeaderBlockFragment = stream_request_headers
        ,_forWorkerThread            = for_worker_thread
        ,_coherentWorker             = coherent_worker
        ,_streamsCancelled           = cancelled_streams_mvar
        }


    let for_worker_thread = error "Fill me in "::  WorkerThreadEnvironment 

    -- Create an input thread that decodes frames...
    input_thread <- liftIO . forkIO $ runReaderT sessionInputThread session_data
 

    return ( (SessionInput session_input),
             (SessionOutput session_output) )



sessionInputThread :: ReaderT SessionData IO ()
sessionInputThread  = do 
    session_input             <- view sessionInput 
    
    decode_headers_table_mvar <- view toDecodeHeaders 
    stream_request_headers    <- view stream2HeaderBlockFragment
    cancelled_streams_mvar    <- view streamsCancelled
    coherent_worker           <- view coherentWorker

    for_worker_thread_uns     <- view forWorkerThread
    

    input                     <- liftIO $ readChan session_input
    case input of 

        Left _ -> do 
            -- Actually, I haven't even defined a type for these tokens....
            error "Received control token but don't know what to do with it..."

        Right frame | Just (stream_id, bytes) <- frameIsHeaderOfStream frame -> do 
            -- Just append the frames to streamRequestHeaders
            appendHeaderFragmentBlock stream_id bytes

            if frameEndsHeaders frame then 
              do
                -- Let's decode the headers
                let for_worker_thread = set streamId stream_id for_worker_thread_uns 
                headers_bytes <- getHeaderBytes stream_id
                dyn_table <- liftIO $ takeMVar decode_headers_table_mvar
                (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes
                -- Good moment to remove the headers from the table.... we don't want a space
                -- leak here 
                liftIO $ H.delete stream_request_headers stream_id
                liftIO $ putMVar decode_headers_table_mvar new_table

                -- I'm clear to start the worker, in its own thread
                -- !!
                liftIO $ forkIO $ runReaderT 
                    (workerThread header_list coherent_worker)
                    for_worker_thread 

                return ()
            else 
                -- Frame doesn't end the headers... it was added before... so
                -- probably do nothing 
                return ()
                
            continue 

        Right frame@(NH2.Frame _ (NH2.RSTStreamFrame error_code_id)) -> do
            liftIO $ putStrLn "Stream reset"
            cancelled_streams <- liftIO $ readMVar cancelled_streams_mvar
            let stream_id = streamIdFromFrame frame
            liftIO $ putMVar cancelled_streams_mvar $ NS.insert  stream_id cancelled_streams

            continue 

  where 

    continue = sessionInputThread



isStreamCancelled :: GlobalStreamId  -> WorkerMonad Bool 
isStreamCancelled stream_id = do 
    cancelled_streams_mvar <- view streamsCancelled_WTE
    cancelled_streams <- liftIO $ readMVar cancelled_streams_mvar
    return $ NS.member stream_id cancelled_streams


workerThread :: Headers -> CoherentWorker -> WorkerMonad ()
workerThread header_list coherent_worker =
  do
    headers_output <- view headersOutput
    stream_id <- view streamId
    (headers, pushed_streams, data_and_conclussion) <- liftIO $ coherent_worker header_list

    -- Now I send the headers, if that's possible at all
    liftIO $ writeChan headers_output (stream_id, headers)

    -- At this moment I should ask if the stream hasn't been cancelled by the browser before
    -- commiting to the work of sending addtitional data
    is_stream_cancelled <- isStreamCancelled stream_id
    if not is_stream_cancelled

      then 
        -- I have a beautiful source that I can de-construct...
        -- TODO: Optionally pulling data out from a Conduit ....
        -- liftIO ( data_and_conclussion $$ (_sendDataOfStream stream_id) )
        (transPipe liftIO data_and_conclussion) $$ (sendDataOfStream stream_id)
      else 

        return ()


--                                                       v-- comp. monad.
sendDataOfStream :: GlobalStreamId -> Sink B.ByteString (ReaderT WorkerThreadEnvironment IO) ()
sendDataOfStream stream_id = do
    data_output <- view dataOutput
    transPipe liftIO $ foldMapM $ \ bytes ->
        writeChan data_output (stream_id, bytes)


-- sendDataOfStream :: Sink     
difficultFunction :: (Monad m)
                  => ConduitM () a2 m r1 -> ConduitM a2 Void m r2
                  -> m (r2, Maybe r1)
difficultFunction l r = liftM (fmap getLast) $ runWriterT (l' $$ r')
  where
    l' = transPipe lift l >>= lift . tell . Last . Just
    r' = transPipe lift r



appendHeaderFragmentBlock :: GlobalStreamId -> B.ByteString -> ReaderT SessionData IO ()
appendHeaderFragmentBlock global_stream_id bytes = do 
    ht <- view stream2HeaderBlockFragment 
    maybe_old_block <- liftIO $ H.lookup ht global_stream_id
    new_block <- return $ case maybe_old_block of 

        Nothing -> bytes

        Just something -> something `B.append` bytes 

    liftIO $ H.insert ht global_stream_id new_block


getHeaderBytes :: GlobalStreamId -> ReaderT SessionData IO B.ByteString
getHeaderBytes global_stream_id = do 
    ht <- view stream2HeaderBlockFragment 
    Just bytes <- liftIO $ H.lookup ht global_stream_id
    return bytes


frameIsHeaderOfStream :: Frame -> Maybe (GlobalStreamId, B.ByteString)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.HeadersFrame _ block_fragment   ) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream _                                       
    = Nothing 


frameEndsHeaders  :: NH2.Frame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


streamIdFromFrame :: NH2.Frame -> GlobalStreamId
streamIdFromFrame (NH2.Frame (NH2.FrameHeader _ _ stream_id) _) = NH2.fromStreamIdentifier stream_id
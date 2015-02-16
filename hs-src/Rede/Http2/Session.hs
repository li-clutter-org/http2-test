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
import qualified Data.Conduit.Combinators                as Com
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


-- type Frame = NH2.Frame

-- Unfortunately the frame encoding API of Network.HTTP2 is a bit difficult to 
-- use :-( 
type OutputFrame = (NH2.EncodeInfo, NH2.FramePayload)
type InputFrame  = NH2.Frame


useChunkLength :: Int 
useChunkLength = 16384




-- Whatever a worker thread is going to need comes here.... 
-- this is to make refactoring easier, but not strictly needed. 
data WorkerThreadEnvironment = WorkerThreadEnvironment {
    -- What's the header stream id?
    _streamId :: GlobalStreamId

    -- A full block of headers can come here
    , _headersOutput :: Chan (GlobalStreamId, Headers)

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
newtype SessionInput = SessionInput ( Chan (Either SessionInputCommand InputFrame) )
sendFrametoSession :: SessionInput  -> InputFrame -> IO ()
sendFrametoSession (SessionInput chan) frame = writeChan chan $ Right frame


-- From outside, one can only read from this one 
newtype SessionOutput = SessionOutput ( Chan (Either SessionOutputCommand OutputFrame) )
getFrameFromSession :: SessionOutput -> IO (Either SessionOutputCommand OutputFrame) 
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


-- NH2.Frame != Frame
data SessionData = SessionData {
    _sessionInput                :: Chan (Either SessionInputCommand InputFrame)

    -- We need to lock this channel occassionally so that we can order multiple 
    -- header frames properly.... 
    ,_sessionOutput              :: MVar (Chan (Either SessionOutputCommand OutputFrame))

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


--                                v- {headers table size comes here!!}
http2Session :: CoherentWorker -> SessionStartData -> IO Session
http2Session coherent_worker _ =   do 
    session_input   <- newChan
    session_output  <- newChan
    session_output_mvar <- newMVar session_output


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
        ,_sessionOutput              = session_output_mvar
        ,_toDecodeHeaders            = decode_headers_table_mvar
        ,_toEncodeHeaders            = encode_headers_table_mvar
        ,_stream2HeaderBlockFragment = stream_request_headers
        ,_forWorkerThread            = for_worker_thread
        ,_coherentWorker             = coherent_worker
        ,_streamsCancelled           = cancelled_streams_mvar
        }

    -- Create an input thread that decodes frames...
    forkIO $ runReaderT sessionInputThread session_data
 
    -- Create a thread that captures headers and sends them down the tube 
    forkIO $ runReaderT (headersOutputThread headers_output session_output_mvar) session_data

    -- Create a thread that captures data and sends it down the tube
    forkIO $ dataOutputThread data_output session_output_mvar

    -- The two previous thread fill the session_output argument below (they write to it)
    -- the session machinery in the other end is in charge of sending that data through the 
    -- socket.
    

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
                let for_worker_thread     = set streamId stream_id for_worker_thread_uns 
                headers_bytes             <- getHeaderBytes stream_id
                dyn_table                 <- liftIO $ takeMVar decode_headers_table_mvar
                (new_table, header_list ) <- liftIO $ HP.decodeHeader dyn_table headers_bytes
                -- Good moment to remove the headers from the table.... we don't want a space
                -- leak here 
                liftIO $ H.delete stream_request_headers stream_id
                liftIO $ putMVar decode_headers_table_mvar new_table

                -- I'm clear to start the worker, in its own thread
                -- !!
                liftIO . forkIO $ runReaderT 
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


frameIsHeaderOfStream :: InputFrame -> Maybe (GlobalStreamId, B.ByteString)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.HeadersFrame _ block_fragment   ) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )
    = Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream _                                       
    = Nothing 


frameEndsHeaders  :: InputFrame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags


streamIdFromFrame :: InputFrame -> GlobalStreamId
streamIdFromFrame (NH2.Frame (NH2.FrameHeader _ _ stream_id) _) = NH2.fromStreamIdentifier stream_id


-- TODO: Have different size for the headers..... just now going with a default size of 16 k...
-- TODO: Find a way to kill this thread....
headersOutputThread :: Chan (GlobalStreamId, Headers)
                       -> MVar (Chan (Either SessionOutputCommand OutputFrame)) 
                       -> ReaderT SessionData IO ()
headersOutputThread input_chan session_output_mvar = forever $ do 
    (stream_id, headers) <- liftIO $ readChan input_chan

    -- First encode the headers using the table
    encode_dyn_table_mvar <- view toEncodeHeaders

    encode_dyn_table <- liftIO $ takeMVar encode_dyn_table_mvar
    (new_dyn_table, data_to_send ) <- liftIO $ HP.encodeHeader HP.defaultEncodeStrategy encode_dyn_table headers
    liftIO $ putMVar encode_dyn_table_mvar new_dyn_table

    -- Now split the bytestring in chunks of the needed size.... 
    let bs_chunks = bytestringChunk useChunkLength data_to_send

    -- And send the chunks through while locking the output place....
    session_output <- liftIO $ takeMVar session_output_mvar

    -- First frame is just a headers frame....
    if (length bs_chunks) == 1 
      then
        do 
            let flags = NH2.setEndHeader NH2.defaultFlags

            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.HeadersFrame Nothing (head bs_chunks)
                )
      else 
        do 
            let flags = NH2.defaultFlags
            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.HeadersFrame Nothing (head bs_chunks)
                )
            -- And write the other frames
            let 
                writeContinuations :: [B.ByteString] -> ReaderT SessionData IO ()
                writeContinuations (last_fragment:[]) = liftIO $
                    writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.setEndHeader NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.ContinuationFrame last_fragment
                        )
                writeContinuations (fragment:xs) = do 
                    liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.ContinuationFrame fragment
                        )
                    writeContinuations xs


            writeContinuations (tail bs_chunks)
    -- Restore output capability, so that other pieces waiting can send...
    liftIO $ putMVar session_output_mvar session_output
  

bytestringChunk :: Int -> B.ByteString -> [B.ByteString]
bytestringChunk len s | (B.length s) < len = [ s ]
bytestringChunk len s = h:(bytestringChunk len xs)
  where 
    (h, xs) = B.splitAt len s 


-- TODO: find a clean way to finish this thread (maybe with negative stream ids?)
-- TODO: This function does non-optimal chunking for the case where responses are
--       actually streamed.... in those cases we need to keep state for frames in 
--       some other format.... 
dataOutputThread :: Chan (GlobalStreamId, B.ByteString)
                    -> MVar (Chan (Either SessionOutputCommand OutputFrame)) 
                    -> IO ()
dataOutputThread input_chan session_output_mvar = forever $ do 
    (stream_id, contents) <- readChan input_chan

    -- And now just simply output it...
    let bs_chunks = bytestringChunk useChunkLength contents

    -- And send the chunks through while locking the output place....
    session_output <- liftIO $ takeMVar session_output_mvar

    -- First frame is just a headers frame....
    if (length bs_chunks) == 1 
      then
        do 
            let flags = NH2.setEndStream NH2.defaultFlags

            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.DataFrame (head bs_chunks)
                )
      else 
        do 
            let flags = NH2.defaultFlags
            -- Write the first frame 
            liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                NH2.encodeFlags     = flags
                ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                ,NH2.encodePadding  = Nothing }, 

                NH2.DataFrame (head bs_chunks)
                )
            -- And write the other frames
            let 
                writeContinuations :: [B.ByteString] -> IO ()
                writeContinuations (last_fragment:[]) = liftIO $
                    writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.setEndStream NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.DataFrame last_fragment
                        )
                writeContinuations (fragment:xs) = do 
                    liftIO $ writeChan session_output $ Right ( NH2.EncodeInfo {
                        NH2.encodeFlags     = NH2.defaultFlags 
                        ,NH2.encodeStreamId = NH2.toStreamIdentifier stream_id 
                        ,NH2.encodePadding  = Nothing }, 

                        NH2.DataFrame fragment
                        )
                    writeContinuations xs
            writeContinuations (tail bs_chunks)
    -- Restore output capability, so that other pieces waiting can send...
    liftIO $ putMVar session_output_mvar session_output                    
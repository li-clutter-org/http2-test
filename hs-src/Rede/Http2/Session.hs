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


import           Control.Monad                           (forever)
import           Control.Lens
import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Reader
import           Control.Exception(throwIO)

import           Data.Conduit
-- import           Data.IORef
import qualified Data.Streaming.Zlib                     as Z

-- import           Data.Conduit.Lift                       (distribute)
-- import qualified Data.Conduit.List                       as CL
import qualified Data.ByteString                         as B
import qualified Data.ByteString.Lazy                    as LB
import           Data.ByteString.Char8                   (pack)
import           Data.Default                            (def)
import           Control.Concurrent.MVar
import qualified Data.Map                                as MA
import           Data.Binary.Put                         (runPut)
import qualified Data.Binary                             as Bi
import           Data.Binary.Get                         (runGet)
import qualified Data.HashTable.IO          as H
import qualified Data.Dequeue               as D


import           Rede.MainLoop.CoherentWorker 
import           Rede.MainLoop.Tokens


type Frame = NH2.Frame


-- Very basic thing that a session is, with ic and oc 
-- the side information with channel events... (like connection 
-- closed, or destroy the thing)
type Session ic oc = (SessionInput ic, SessionOutput oc)


-- From outside, one can only write to this one 
newtype SessionInput ic = SessionInput (Chan (Either ic Frame))
sendFrametoSession :: SessionInput ic -> Frame -> IO ()
sendFrametoSession (SessionInput chan) frame = writeChan chan $ Right frame


-- From outside, one can only read from this one 
newtype SessionOutput oc = SessionOutput (Chan (Either oc Frame))
getFrameFromSession :: SessionOutput oc -> IO (Either oc Frame) 
getFrameFromSession (SessionOutput chan) = readChan chan


-- Here is how we make a session 
type SessionMaker a ic oc = a -> IO (Session ic oc)


-- Here is how we make a session wrapping a CoherentWorker
type CoherentSession a ic oc = CoherentWorker -> SessionMaker a ic oc 


type HashTable k v = H.CuckooHashTable k v


-- Blaze builder could be more proper here... 
type Stream2HeaderBlockFragment = HashTable GlobalStreamId B.ByteString



data SessionData = SessionData {
    -- To be kept around while the stream is being initialized....
    _streamRequestHeaders :: Stream2HeaderBlockFragment

    -- Use to encode 
    ,_toEncodeHeaders :: MVar HP.DynamicTable
    -- And used to decode
    ,_toDecodeHeaders :: MVar HP.DynamicTable
    }


makeLenses ''SessionData


flippedRunReader :: r -> ReaderT r m a -> m a
flippedRunReader  = (flip runReaderT)


--                                v- {headers table size comes here!!}
http2Session :: CoherentWorker -> () -> IO (Session ic oc)
http2Session coherent_worker _ =   do 
    session_input   <- newChan
    session_output  <- newChan
    to_decode_headers <- view toDecodeHeaders
    stream_request_headers <- H.new
    decode_headers_table <- HP.newDynamicTableForDecoding

    -- Create an input thread that decodes frames...
    input_thread <- liftIO $ forkIO $ flippedRunReader  (to_decode_headers, stream_request_headers ) $ do 
        input <- liftIO $ readChan session_input 
        case input of 

            Left _ -> do 
                error "Received control token but don't know what to do with it..."

            Right frame | Just (stream_id, bytes) <- frameIsHeaderOfStream frame -> do 
                -- Just append the frames to streamRequestHeaders
                appendHeaderFragmentBlock stream_id bytes

                if frameEndsHeaders frame then 
                  do
                    -- Let's decode the headers
                    -- I'm clear to start the worker, in its own thread


    return ( (SessionInput session_input),
             (SessionOutput session_output) )


newSessionData :: SessionData
newSessionData = error "Not implemented"


appendHeaderFragmentBlock :: GlobalStreamId -> B.ByteString -> ReaderT (HP.DynamicTable,  Stream2HeaderBlockFragment) IO ()
appendHeaderFragmentBlock global_stream_id bytes = do 
    ht <- view _2 
    maybe_old_block <- liftIO $ H.lookup ht global_stream_id
    new_block <- return $ case maybe_old_block of 

        Nothing -> bytes

        Just something -> something `B.append` bytes 

    liftIO $ H.insert ht global_stream_id new_block


frameIsHeaderOfStream :: Frame -> Maybe (GlobalStreamId, B.ByteString)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.HeadersFrame _ block_fragment   ) )= Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream (NH2.Frame (NH2.FrameHeader _ _ stream_id) ( NH2.ContinuationFrame block_fragment) )= Just (NH2.fromStreamIdentifier stream_id, block_fragment)
frameIsHeaderOfStream _                                        = Nothing 


frameEndsHeaders  :: NH2.Frame -> Bool 
frameEndsHeaders (NH2.Frame (NH2.FrameHeader _ flags _) _) = NH2.testEndHeader flags
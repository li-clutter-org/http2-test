
module Rede.SpdyProtocol.Session(
    trivialSession
    ) where


import           Control.Monad.IO.Class                  (liftIO)
import           Control.Concurrent                      (forkIO)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Reader 
import           Data.Conduit
import           Data.Conduit.Lift                       (distribute)
import qualified Data.Conduit.List                       as CL
import           Data.Default                            (def)
-- Going for the simple choice now!!
import qualified Data.Map                                as MA
import           Data.IORef
import           Control.Concurrent.MVar

import           Rede.SpdyProtocol.Framing.Frame
import           Rede.SpdyProtocol.Framing.AnyFrame     
import qualified Rede.SpdyProtocol.Framing.GoAway        as GoA
import           Rede.SpdyProtocol.Framing.KeyValueBlock
import qualified Rede.SpdyProtocol.Framing.Settings      as SeF
import qualified Rede.SpdyProtocol.Framing.SynStream     as SyS
import           Rede.SpdyProtocol.Streams.State         (StreamStateT,
                                                          initStreamState,
                                                          unpackRecvHeaders)
import           Rede.SpdyProtocol.TrivialTestWorker     (trivialWorker)
import           Rede.MainLoop.StreamPlug


initialSettings :: SeF.SettingsFrame
initialSettings = SeF.SettingsFrame {
     SeF.prologue = def 
    ,SeF.persistSettings = [
        (SeF.InitialWindowSize_S, 65536, SeF.None_PS)
    ]
}


goAwayMsg :: GoA.GoAwayFrame
goAwayMsg = GoA.GoAwayFrame {
     GoA.prologue = def 
    ,GoA.statusCode = GoA.OK_GAR
    ,GoA.lastGoodStream = 0
}



data SimpleSessionStateRecord = SimpleSessionStateRecord {
    streamInputs :: IORef (MA.Map Int (MVar  AnyFrame))  
    }


type SessionM = ReaderT SimpleSessionStateRecord 
 
-- Super-simple session manager without flow control and such.... 
-- but using StreamWorkers already....
-- TODO: without proper flow control, we are in troubles....
superSimpleSessionWithState :: IO ( (Sink AnyFrame IO () ), (Source IO AnyFrame ) )
superSimpleSessionWithState = do
    input_mvar <- (newEmptyMVar :: IO (MVar AnyFrame))
    packet_sink <- return $ createTrivialSink input_mvar
    output_mvar <- (newEmptyMVar :: IO (MVar (Maybe AnyFrame)))
    packet_source <- return $ createTrivialSource output_mvar
    

    stream_inputs     <- newIORef $ MA.empty
    session_record    <- return $ SimpleSessionStateRecord {
        streamInputs = stream_inputs
        }

    forkIO $ processInput  session_record input_mvar 
    forkIO $ processOutput session_record output_mvar 

    return (packet_sink, packet_source)
    
                         
  where

    -- asyncWork :: (MVar AnyFrame) -> (MVar (Maybe AnyFrame)) -> SimpleSessionStateRecord -> IO ()
    asyncWork input_mvar output_mvar session_state_record = do

    startConduit = yield $  wrapCF initialSettings

    -- streamConduit :: MVar AnyFrame -> MVar AnyFrame -> streamMonad ()
    streamConduit input_mvar output_mvar = ( (takesInput input_mvar)  $= inputPlug 
            =$= (transPipe liftIO trivialWorker) 
            =$= (outputPlug :: Conduit StreamOutputAction (StreamStateT IO) AnyFrame)
            $$  ( CL.mapM_ $ \ any_frame -> liftIO $ putMVar output_mvar any_frame ) 
        ) :: StreamStateT IO ()

    -- iDropThisFrame :: AnyControlFrame -> Bool
    iDropThisFrame  (SettingsFrame_ACF _ ) = True 
    iDropThisFrame  _                      = False
    -- IMPLEMENT: iDropThisFrame  (Ping_CFT) 

    -- processInput :: Session
    processInput session_record input_mvar = do 
        anyframe <- takeMVar input_mvar
        maybe_anyframe <- await 
        case maybe_anyframe of 

            -- Some of the frames won't be handled.
            Just (AnyControl_AF control_frame)  | iDropThisFrame control_frame -> return () 

            -- -- We started here 
            -- Just (AnyControl_AF (PingFrame_ACF ping_frame)) -> _handle_ping_frame

            Just (AnyControl_AF (SynStream_ACF syn_stream)) ->  let 
                    stream_id = streamIdFromFrame syn_stream
                    inputs = streamInputs session_record
                in do 
                    stream_inputs <- liftIO $ readIORef $ inputs
                    input_place   <- liftIO $ newEmptyMVar
                    liftIO $ writeIORef inputs $ MA.insert stream_id input_place stream_inputs
                    liftIO $ forkIO $ initStreamState $ streamConduit input_place output_place
                    return ()
        processInput session_record output_place




showHeadersIfPresent ::  AnyFrame -> StreamStateT IO ()
showHeadersIfPresent ( AnyControl_AF ( SynStream_ACF syn_stream)) = let 
    CompressedKeyValueBlock hb = SyS.compressedKeyValueBlock syn_stream
  in do 
    uncompressed <- unpackRecvHeaders hb
    liftIO $ putStrLn $ show uncompressed 
showHeadersIfPresent _ = return ()



-- Just for testing
trivialSession  :: IO ( (Sink AnyFrame IO () ), (Source IO AnyFrame ) )
trivialSession  = do 
    input_mvar <- (newEmptyMVar :: IO (MVar AnyFrame))
    packet_sink <- return $ createTrivialSink input_mvar
    output_mvar <- (newEmptyMVar :: IO (MVar (Maybe AnyFrame)))
    packet_source <- return $ createTrivialSource output_mvar
    forkIO $ trivialAsync input_mvar output_mvar
    return (packet_sink, packet_source)

  where 

    -- IO ()
    trivialAsync input_mvar output_mvar = do 
        pck1 <- takeMVar input_mvar
        putStrLn $ show pck1
        pck2 <- takeMVar input_mvar
        putStrLn $ show pck2
        pck3 <- takeMVar input_mvar
        putStrLn $ show pck3

createTrivialSink :: MVar AnyFrame -> Sink AnyFrame IO
createTrivialSink input_mvar = do 
    anyframe_maybe <- await 
    case anyframe_maybe of 
        Just anyframe -> do 
            liftIO $ putMVar input_mvar anyframe
            createTrivialSink input_mvar
        Nothing -> return () 

createTrivialSource ::  MVar AnyFrame -> Source (Maybe AnyFrame) IO
createTrivialSource output_mvar = do 
    anyframe_maybe <- liftIO $ readMVar output_mvar
    case anyframe_maybe of 
        Just anyframe -> do
            yield anyframe
            createTrivialSource output_mvar
        Nothing       -> do
            return () -- (done)

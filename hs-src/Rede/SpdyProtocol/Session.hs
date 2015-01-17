
module Rede.SpdyProtocol.Session(
	trivialSession
	) where


import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Trans.Class               (lift)
import           Data.Conduit
import           Data.Conduit.Lift                       (distribute)
import           Data.Default                            (def)


import qualified Rede.SpdyProtocol.Framing.AnyFrame      as AnFr
import qualified Rede.SpdyProtocol.Framing.GoAway        as GoA
import           Rede.SpdyProtocol.Framing.KeyValueBlock
import qualified Rede.SpdyProtocol.Framing.Settings      as SeF
import qualified Rede.SpdyProtocol.Framing.SynStream     as SyS
import           Rede.SpdyProtocol.Streams.State         (StreamStateT,
                                                          initStreamState,
                                                          unpackRecvHeaders)


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


-- Just for testing
trivialSessionWithState  :: Conduit AnFr.AnyFrame (StreamStateT IO) AnFr.AnyFrame
trivialSessionWithState  = do 
	yield $ AnFr.wrapCF initialSettings
	Just pck1 <- await 
	liftIO $ putStrLn $ show pck1
	Just pck2 <- await
	liftIO $ putStrLn $ show pck2
	Just pck3 <- await 
	liftIO $ putStrLn $ show pck3 
	lift $ showHeadersIfPresent pck3
	yield $ AnFr.wrapCF goAwayMsg
	-- We start by informing the browser of the settings


showHeadersIfPresent :: AnFr.AnyFrame -> StreamStateT IO ()
showHeadersIfPresent (AnFr.AnyControl_AF (AnFr.SynStream_ACF syn_stream)) = let 
    CompressedKeyValueBlock hb = SyS.compressedKeyValueBlock syn_stream
  in do 
  	uncompressed <- unpackRecvHeaders hb
  	liftIO $ putStrLn $ show uncompressed 
showHeadersIfPresent _ = return ()


-- Need to refactor....
trivialSession ::  Conduit AnFr.AnyFrame IO AnFr.AnyFrame   
trivialSession = 
	initStreamState $ distribute $ trivialSessionWithState  
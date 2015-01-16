
module Rede.SpdyProtocol.Session(
	trivialSession
	) where


import Data.Default
import Data.Conduit
import Data.Conduit.Lift( distribute )
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class


import Rede.SpdyProtocol.Framing.AnyFrame
import qualified  Rede.SpdyProtocol.Framing.Settings as SeF 
import qualified  Rede.SpdyProtocol.Framing.GoAway   as GoA
import qualified  Rede.SpdyProtocol.Framing.SynStream as SyS
import Rede.SpdyProtocol.Streams.State
import Rede.SpdyProtocol.Framing.KeyValueBlock

-- import qualified  Rede.SpdyProtocol.Framing.WindowUpdate as WuF



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
trivialSessionWithState  :: Conduit AnyFrame (StreamStateT IO) AnyFrame
trivialSessionWithState  = do 
	yield $ wrapCF initialSettings
	Just pck1 <- await 
	liftIO $ putStrLn $ show pck1
	Just pck2 <- await
	liftIO $ putStrLn $ show pck2
	Just pck3 <- await 
	liftIO $ putStrLn $ show pck3 
	lift $ showHeadersIfPresent pck3
	yield $ wrapCF goAwayMsg
	-- We start by informing the browser of the settings


showHeadersIfPresent :: AnyFrame -> StreamStateT IO ()
showHeadersIfPresent (AnyControl_AF (SynStream_ACF syn_stream)) = let 
    CompressedKeyValueBlock hb = SyS.compressedKeyValueBlock syn_stream
  in do 
  	uncompressed <- unpackRecvHeaders hb
  	liftIO $ putStrLn $ show uncompressed 
showHeadersIfPresent _ = return ()


-- Need to refactor....
trivialSession ::  Conduit AnyFrame IO AnyFrame   
trivialSession = 
	initStreamState $ distribute $ trivialSessionWithState  
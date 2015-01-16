
module Rede.SpdyProtocol.Session(
	trivialSession
	) where


import Data.Default
import Data.Conduit


import Rede.SpdyProtocol.Framing.AnyFrame
import qualified  Rede.SpdyProtocol.Framing.Settings as SeF 
import qualified  Rede.SpdyProtocol.Framing.GoAway   as GoA

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
trivialSession  :: Monad m => Conduit AnyFrame m AnyFrame
trivialSession  = do 
	yield $ wrapCF initialSettings
	yield $ wrapCF goAwayMsg
	-- We start by informing the browser of the settings 
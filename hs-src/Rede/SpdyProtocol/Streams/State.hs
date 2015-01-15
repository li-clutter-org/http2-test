
module Rede.SpdyProtocol.Streams.State(
	) where 


import qualified Data.Streaming.Zlib as Z


data StreamStage =
	 Open_StS
	|CanOnlySend_StS
	|CanOnlyReceive_Sts
	|Closed 
	;


-- Due to the use of Z here, this struct only
-- makes sense inside the IO monad (or equivalent)
data StreamState = StreamState {
	  stage      :: StreamStage
	, bytesSent  :: Int 
	, sendWindow :: Int
	, sendZlib   :: Z.Deflate
	, recvZlib   :: Z.Inflate 
	}
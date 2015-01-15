
module Rede.MainLoop.PushPullType (
	PushAction
	,PullAction
	,Attendant
	) where 


import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB


type PushAction  = LB.ByteString -> IO ()

type PullAction  = IO  B.ByteString

type Attendant = PushAction -> PullAction -> IO () 
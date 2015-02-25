
module Rede.Http2.MakeAttendant (
    http2Attendant
    ) where


import Rede.MainLoop.CoherentWorker
import           Rede.MainLoop.PushPullType   (PullAction,
                                               PushAction)
import Rede.Http2.Framer  (wrapSession)


http2Attendant :: CoherentWorker -> PushAction -> PullAction ->  IO ()
http2Attendant coherent_worker push_action pull_action  = do 
    let 
        attendant = wrapSession coherent_worker
    attendant push_action pull_action        
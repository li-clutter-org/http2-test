{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- Contains adapter helpers to stream workers
module Rede.MainLoop.StreamPlug(
    UnpackedNameValueList
    ,StreamInputToken
    ,StreamOutputAction
    ,StreamId
    ,StreamPlug (..)
    ,IdStream(..)
    ) where 

-- import qualified Data.ByteString                as B
import Data.Conduit (Conduit)
import Control.Monad.IO.Class


import Rede.MainLoop.Tokens

type StreamId = Int


class IdStream frame where 
    idStream :: frame -> Maybe StreamId 


-- This interface says: m is a Monad that you can easily 
-- materialize out of thin IO-air, nothing is required 
-- from the embedded session (is this in general possible? 
-- I would answer yes for SPDY... so far). That's way 
-- "initStreamPlugs" can drop the monad in the IO space.
class MonadIO m => StreamPlug m frame | m -> frame where
    inputPlug        :: Conduit frame m StreamInputToken
    outputPlug       :: Conduit StreamOutputAction m frame
    initStreamPlugs  :: m a -> IO a


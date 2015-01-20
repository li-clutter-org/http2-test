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


-- This interface says: m is a Monad that you need to materialize
-- from the embedding session somehow, everything else can be 
-- well-encapsulated in this computation...
class MonadIO m => StreamPlug m frame | m -> frame where
    inputPlug        :: Conduit frame m StreamInputToken
    outputPlug       :: Conduit StreamOutputAction m frame



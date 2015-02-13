{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- | A CoherentWorker is one that doesn't need to compute everything at once...
--   This one is simpler than the SPDY one, because it enforces certain order....



module Rede.MainLoop.CoherentWorker(
    Headers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclussion
    ) where 


import Data.Conduit
import qualified Data.ByteString as B


type Headers = [(B.ByteString, B.ByteString)]

type FinalizationHeaders = Headers

-- Thanks to Luis Cobian for the name... 
type Footers = FinalizationHeaders

type CoherentWorker = Headers -> IO PrincipalStream

type PrincipalStream = (Headers, PushedStreams, DataAndConclussion)

type DataAndConclussion = ConduitM () B.ByteString IO Headers

type PushedStreams = [ IO PushedStream ]

type PushedStream = (Headers, DataAndConclussion)
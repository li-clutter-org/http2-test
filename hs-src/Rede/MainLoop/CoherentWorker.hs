{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- | A CoherentWorker is one that doesn't need to compute everything at once...
--   This one is simpler than the SPDY one, because it enforces certain order....



module Rede.MainLoop.CoherentWorker(
    Headers
    , Footers
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

-- We can use this type in the future...
-- type DataAndConclussion = ConduitM () B.ByteString IO Footers

-- This type could possibly return footers but ... 
type DataAndConclussion = Source IO B.ByteString

type PushedStreams = [ IO PushedStream ]

type PushedStream = (Headers, DataAndConclussion)
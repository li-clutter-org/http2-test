{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances  #-} 
-- | A CoherentWorker is one that doesn't need to return everything at once...


module Rede.MainLoop.CoherentWorker(
    getHeaderFromFlatList
    , waitRequestBody

    , Headers
    , Request
    , Footers
    , CoherentWorker
    , PrincipalStream
    , PushedStreams
    , PushedStream
    , DataAndConclussion
    , InputDataStream
    ) where 


import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as Bu
import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit
import           Data.Foldable           (find)
import qualified Data.Monoid             as M



type Headers = [(B.ByteString, B.ByteString)]

type InputDataStream = Source IO B.ByteString

-- A request is a set of headers and a request body....
-- which will normally be empty
type Request = (Headers, Maybe InputDataStream)

type FinalizationHeaders = Headers

-- Thanks to Luis Cobian for the name... 
type Footers = FinalizationHeaders

type CoherentWorker = Request -> IO PrincipalStream

type PrincipalStream = (Headers, PushedStreams, DataAndConclussion)

-- We can use this type in the future...
-- type DataAndConclussion = ConduitM () B.ByteString IO Footers

-- This type could possibly return footers but ... 
-- Update: there is functionality now to support this....
type DataAndConclussion = Source IO B.ByteString

type PushedStreams = [ IO PushedStream ]

type PushedStream = (Headers, DataAndConclussion)


getHeaderFromFlatList :: Headers -> B.ByteString -> Maybe B.ByteString
getHeaderFromFlatList unvl bs = 
    case find (\ (x,_) -> x==bs ) unvl of
        Just (_, found_value)  -> Just found_value 

        Nothing                -> Nothing  


-- | Consumes the request body and returns it.... this can be 
--   happily done in the threadlets of Haskell without any further
--   brain-burning.....
waitRequestBody :: InputDataStream -> IO B.ByteString
waitRequestBody source = 
  let
    consumer  b = do 
        maybe_bytes <- await 
        case maybe_bytes of 

            Just bytes -> do
                -- liftIO $ putStrLn $ "Got bytes " ++ (show $ B.length bytes)
                consumer $ b `M.mappend` (Bu.byteString bytes)

            Nothing -> do
                -- liftIO $ putStrLn "Finishing"
                return b
  in do 
    full_builder <- (source $$ consumer "") :: IO Bu.Builder
    return $ (LB.toStrict . Bu.toLazyByteString) full_builder
    





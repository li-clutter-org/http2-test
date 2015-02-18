module Rede.MainLoop.Framer(
    readNextChunk
    ,readLength

	,Framer
    ,LengthCallback
	) where


import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import           Data.Conduit

import           Data.Monoid               (mappend, mempty)


type Framer m =        LB.ByteString                        -- Input left overs
                       -> m B.ByteString                    -- Generator
                       -> Maybe Int                         -- Length to read, if we know now
                       -> m (LB.ByteString, LB.ByteString)  -- To yield, left-overs...


-- * Doing it by parts

type LengthCallback = B.ByteString -> Maybe Int


readNextChunk :: Monad m =>
    LengthCallback                         -- ^ How to know if we can split somewhere
    -> B.ByteString                        -- ^ Input left-overs
    -> m B.ByteString                      -- ^ Generator action
    -> Source m B.ByteString               -- ^ Packet and leftovers, if we could get them 
readNextChunk length_callback input_leftovers gen = do 
    new_fragment <- lift gen 
    let 
        new_leftovers = mappend input_leftovers new_fragment
        maybe_length = length_callback new_leftovers
        readUpTo lo the_length | (B.length lo) >= the_length = 
            return $ B.splitAt the_length lo
        readUpTo lo the_length = do 
            frag <- lift gen 
            readUpTo (lo `mappend` frag) the_length

    case maybe_length of 
        Just the_length -> do 
            -- Just need to read the rest .... 
            (package_bytes, newnewleftovers) <- readUpTo new_leftovers the_length 
            yield package_bytes 
            readNextChunk length_callback newnewleftovers gen 

        Nothing -> do 
            -- Read a bit more 
            readNextChunk length_callback new_leftovers gen


-- Some protocols, e.g., http/2, have the client transmit a fixed-length
-- prefix. This function reads both that prefix and returns whatever get's
-- trapped up there.... 
readLength :: MonadIO m => Int -> m B.ByteString -> m (B.ByteString, B.ByteString)
readLength the_length gen = 
    readUpTo mempty 
  where 
    readUpTo lo  
      | (B.length lo) >= the_length  = do
            liftIO $ putStrLn "Full read"
            return $ B.splitAt the_length lo
      | otherwise = do 
            liftIO $ putStrLn "fragment read"
            frag <- gen 
            readUpTo (lo `mappend` frag)

  

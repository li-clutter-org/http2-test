{-# LANGUAGE OverloadedStrings #-}
module SpdyPing.MainLoop () where 

import qualified Pipes.Core as PC 
import Pipes
import qualified Pipes.Prelude as P 
import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy as LB
import           Data.Binary.Get        (runGet)
import Control.Monad(unless)


import SpdyPing.Framing.Frame


chunkProducer :: IO B.ByteString    -- Generator
      -> LB.ByteString              -- Left-overs
      -> PC.Producer LB.ByteString IO ()
chunkProducer gen leftovers = do 
	(product, new_leftovers) <- lift $ chunkProducerHelper leftovers gen Nothing
	yield product
	chunkProducer gen new_leftovers


chunkProducerHelper :: LB.ByteString 
                       -> IO B.ByteString 
                       -> Maybe Int -- Length to read
                       -> IO (LB.ByteString, LB.ByteString)  -- To yield, left-overs...
chunkProducerHelper pieces gen Nothing = 
	let 
		lazy = pieces 
		length_lazy = LB.length lazy
		the_control_Frame = runGet getControlFrame lazy
		cf_length = cfLength the_control_Frame
	in if length_lazy >= 8 then
		  chunkProducerHelper pieces gen (Just cf_length)
	   else do 
	   	  new_piece <- gen 
	   	  new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
	   	  chunkProducerHelper new_lazy gen Nothing 
chunkProducerHelper lazy gen j@(Just length_to_read) =
	let 
		length_lazy = LB.length lazy
		l64 = fromIntegral length_to_read
	in if length_lazy >= l64 then
		  	return $ LB.splitAt l64 lazy
	    else do 
	   	    new_piece <- gen 
	   	    new_lazy <- return $ LB.append lazy (LB.fromChunks [new_piece]) 
	   	    chunkProducerHelper new_lazy gen j


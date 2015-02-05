module Rede.MainLoop.Framer(
	Framer
	) where


import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as LB


type Framer m =        LB.ByteString 
                       -> m B.ByteString                    -- Generator
                       -> Maybe Int                         -- Length to read
                       -> m (LB.ByteString, LB.ByteString)  -- To yield, left-overs...



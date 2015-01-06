{-# LANGUAGE OverloadedStrings #-}

module SpdyPing.Constants(
	whichSPDY,
	whichHTTP
)  where

import           Data.ByteString(ByteString)

whichSPDY :: ByteString
whichSPDY = "spdy/3.1"

whichHTTP :: ByteString 
whichHTTP = "http/1.1"
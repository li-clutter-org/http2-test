module SpdyPing.Utils (strToInt) where 

strToInt::String -> Int 
strToInt = fromIntegral . toInteger . read
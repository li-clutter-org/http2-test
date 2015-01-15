module Rede.StreamToken( StreamToken ) where


import qualified Data.ByteString as B


data StreamToken =  Headers_STk  [(B.ByteString, B.ByteString)]
                  | Data_Stk     B.ByteString
                  | Finish_Stk
                  deriving Show

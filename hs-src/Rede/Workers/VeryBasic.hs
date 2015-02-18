{-# LANGUAGE OverloadedStrings #-}

module Rede.Workers.VeryBasic(
    veryBasic
    ) where 


import qualified Data.ByteString as B
import           Data.Conduit


import Rede.MainLoop.CoherentWorker


trivialHeaders :: [(B.ByteString, B.ByteString)] 
trivialHeaders = [
    (":status", "200"),
    ("server",  "reh0m")
    ]


veryBasic :: CoherentWorker
veryBasic headers = do 
    let data_and_conclussion = yield "Hello world!"

    print "Got these headers: "
    print headers 

    return (trivialHeaders, [], data_and_conclussion)
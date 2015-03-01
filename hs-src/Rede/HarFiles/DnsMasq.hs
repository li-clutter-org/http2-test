{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Rede.HarFiles.DnsMasq (
    dnsMasqFileContents
    ) where 


import qualified Data.ByteString as B 

dnsMasqFileContents :: [B.ByteString] -> B.ByteString
dnsMasqFileContents all_seen_hosts = B.intercalate "\n" $ map 
    (\ hostname -> B.append "127.0.0.1      " hostname) 
    all_seen_hosts
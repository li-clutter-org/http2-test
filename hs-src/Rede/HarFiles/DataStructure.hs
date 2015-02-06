{-# LANGUAGE OverloadedStrings #-}
module Rede.HarFiles.DataStructure where 


data Har_Outer = Har_Outer {
    log:: Har_Log
    }


data Har_Log = Har_Log {
    entries           :: [Har_Entry]
    ,pages            :: [Har_Page]
    ,browser          :: Har_VersionPair
    ,version          :: String 
    ,creator          :: Har_VersionPair
   }


data Har_Page = Har_Page {
    startedDateTime   :: String 
    ,pageTimings      :: Har_PageTimings
    ,pageId           :: String
    ,title            :: String
}



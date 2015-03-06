
{-# LANGUAGE OverloadedStrings #-}


-- System grade imports
-- import qualified Data.ByteString            as B
-- import qualified Data.ByteString.Builder    as Bu
-- import           Data.ByteString.Char8      (pack)
-- import qualified Data.ByteString.Lazy       as BL
-- import           Data.Foldable              (foldMap)
import           Data.Monoid
import           Text.Printf                (printf)

-- import           System.Directory
-- import           System.FilePath
import           System.IO
-- import           System.Process

import           Options.Applicative

-- Logging utilities
import           System.Log.Logger
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Formatter      (simpleLogFormatter)

-- Imports from other parts of the program

import           Rede.MainLoop.ConfigHelp   (mimicDataDir)
import           Rede.Research.Main         (research)


-- What is the program going to do?
data ProgramAction = 
    ResearchUrl_PA -- Wait for a POST request from the browser


actionStrToAction :: String -> ProgramAction
actionStrToAction "research"     = ResearchUrl_PA
actionStrToAction _              = error "Action doesn't exist"


data Program  = Program {
    action   :: ProgramAction
    -- ,harFileName :: String
    }


programParser :: Parser Program
programParser = Program <$> (
    actionStrToAction <$>
        strOption
             ( long "action"
                <> metavar "ACTION"
                <> help    "What's the program going to do: output-hosts, serve or research url" )
    ) 


main :: IO ()
main = do
    configureLogging
    mimic_dir <- mimicDataDir
    infoM "RehMimic" $ printf "Mimic dir: \"%s\"" mimic_dir
    prg   <-  execParser opts_metadata
    case Main.action prg of 

        ResearchUrl_PA       -> do 
            research mimic_dir


  where 
    opts_metadata = info 
        ( helper <*> programParser )
        ( fullDesc 
            <>  progDesc "Mimics web servers from .har file"
            <>  header   "reh-mimic"
            )


configureLogging :: IO ()
configureLogging = do 
    s <- streamHandler stderr DEBUG  >>= 
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "HTTP2.Session" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel ERROR  
        )
    updateGlobalLogger "ResearchWorker" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel DEBUG  
        )

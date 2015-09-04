
{-# LANGUAGE OverloadedStrings #-}


-- System imports
import           Data.Foldable             (find)
import           Data.Monoid
import           Text.Printf               (printf)

import           Control.Exception         (catch)
--
--
import           System.Environment        (getEnvironment)
import           System.IO

import           Options.Applicative

-- Logging utilities
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter, LogHandler)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog (Facility (..), Option (..), openlog)
import           System.Log.Logger

import           SecondTransfer
import           SecondTransfer.Exception  (IOProblem(..))


-- Imports from other parts of the program

import           Rede.MainLoop.ConfigHelp  (mimicDataDir)
import           Rede.Research.Main        (research)




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
                <> help    "What's the program going to do: only \"research\" is supported now." )
        )


main :: IO ()
main = do
    env_vars <- getEnvironment
    case find (\ (name, _) -> name == "MIMIC_CONSOLE_LOGGER") env_vars of

        Nothing                                         -> configureLoggingToSyslog

        Just (_, x) | not $ (length x) == 0 || x == "0" -> configureLoggingToConsole

                    | otherwise                         -> configureLoggingToSyslog


    mimic_dir <- mimicDataDir
    infoM "RehMimic" $ printf "Mimic dir: \"%s\"" mimic_dir
    prg   <-  execParser opts_metadata

    catch
        (case Main.action prg of

            ResearchUrl_PA       -> do
                research mimic_dir
        )
        (\ (IOProblem msg) -> errorM "HTTP2.Session" ("ConnectionIOError: " ++ show msg)
        )


  where
    opts_metadata = info
        ( helper <*> programParser )
        ( fullDesc
            <>  progDesc
                ( "Mimics web servers. Use environmnet variables MIMIC_DATA_DIR to point to where the scratch directory" ++
                  "of the server is. Use the variable MIMIC_CONSOLE_LOGGER with value 1 to log messages to console; otherwise " ++
                  "they are going to syslog. " )

            <>  header   "reh-mimic"
            )


configureLoggingToConsole :: IO ()
configureLoggingToConsole = do
    s <- streamHandler stderr DEBUG  >>=
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    setLoggerLevels s


configureLoggingToSyslog :: IO ()
configureLoggingToSyslog = do
    s <- openlog "RehMimic" [PID] DAEMON INFO >>=
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    setLoggerLevels s


setLoggerLevels :: (LogHandler s) => s -> IO ()
setLoggerLevels s = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "HTTP2.Session" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel INFO
        )
    updateGlobalLogger "OpenSSL" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel INFO
        )
    updateGlobalLogger "HarWorker" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel DEBUG
        )
    updateGlobalLogger "ResearchWorker" (
        setHandlers [s] .  -- Remember that composition works in reverse...
        setLevel DEBUG
        )
    updateGlobalLogger "HTTP2.Framer" (
        setHandlers [s] .
        setLevel INFO
        )

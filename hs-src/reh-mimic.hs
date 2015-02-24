
{-# LANGUAGE OverloadedStrings #-}

import Rede.MainLoop.OpenSSL_TLS(
    tlsServeWithALPN
    )


import qualified Data.ByteString                 as B
import qualified Data.ByteString.Builder         as Bu
import           Data.ByteString.Char8           (pack)
import qualified Data.ByteString.Lazy            as BL
import           Data.Foldable                   (foldMap)
import           Data.Monoid

import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import           Options.Applicative

import           Rede.SimpleHTTP1Response        (exampleHTTP11Response)

import           Rede.HarFiles.ServedEntry       (createResolveCenterFromFilePath,
                                                  hostsFromHarFile)
import           Rede.MainLoop.CoherentWorker    (CoherentWorker)
import           Rede.MainLoop.ConfigHelp        (configDir, getInterfaceName,
                                                  getMimicPort, mimicDataDir,
                                                  getMimicPostPort, getMimicPostInterface,
                                                  getPrivkeyFilename,
                                                  getCertFilename
                                                  )
import           Rede.MainLoop.PushPullType
import           Rede.Workers.AcceptCoherentPost (acceptCoherentPost)
import           Rede.Workers.HarWorker          (harCoherentWorker)
-- We import this one for testing sake
import           Rede.Http2.Framer               (wrapSession)
import           Rede.Research.Main              (research)
import           Rede.Http2.MakeAttendant        (http2Attendant)





-- What is the program going to do?
data ProgramAction = 
    OutputHosts_PA
    |ResearchUrl_PA -- Wait for a POST request from the browser
    |ServeHar_PA



-- data ImproperlyConfigured = ImproperlyConfigured B.ByteString
--     deriving (Show, Typeable)

-- instance Exception ImproperlyConfigured


actionStrToAction :: String -> ProgramAction
actionStrToAction "output-hosts" = OutputHosts_PA 
actionStrToAction "serve"        = ServeHar_PA
actionStrToAction "research"     = ResearchUrl_PA
actionStrToAction _              = error "Action doesn't exist"


data Program  = Program {
    action   :: ProgramAction
    ,harFileName :: String
    }


programParser :: Parser Program
programParser = Program <$> (
    actionStrToAction <$>
        strOption
             ( long "action"
                <> metavar "ACTION"
                <> help    "What's the program going to do: output-hosts, serve or research url" )
    ) <*> (
        strOption
            ( long "har-file"
               <>  metavar "HARFILE"
               <>  help    "Har file to process, if action is output-hosts or serve. The actual url if action is \"research\""
            )
    )


main :: IO ()
main = do
    mimic_dir <- mimicDataDir
    let mimic_config_dir = configDir mimic_dir
    prg   <-  execParser opts_metadata
    let har_filename = harFileName prg
    case Main.action prg of 
        OutputHosts_PA        -> do
            all_seen_hosts <- hostsFromHarFile har_filename
            outputHosts all_seen_hosts
            -- And since I'm on that, let's create a big fluffy certificate to 
            -- use in this case..
            getComprehensiveCertificate mimic_dir har_filename all_seen_hosts


        ResearchUrl_PA       -> do 
            let url_to_research = har_filename
            research mimic_config_dir url_to_research har_filename


        ServeHar_PA           -> do
            port  <-  getMimicPort
            putStrLn $  "Mimic port: " ++ (show port)
            iface <-  getInterfaceName mimic_config_dir
            putStrLn $ "Using interface: " ++ (show iface)

            let 
                priv_key_filename = privKeyFilename mimic_dir har_filename
                cert_filename  = certificateFilename mimic_dir har_filename
            putStrLn $ "Chosen cert. at file: " ++ (show cert_filename)
            putStrLn $ "... with private key: " ++ (show priv_key_filename)
            resolve_center <- createResolveCenterFromFilePath $ pack har_filename 
            let 
                http2worker = harCoherentWorker resolve_center
            tlsServeWithALPN  cert_filename priv_key_filename iface [ 
                 -- ("h2-14", wrapSession veryBasic)
                 ("h2-14", http2Attendant http2worker)
                --,("spdy/3.1" ,spdyAttendant har_filename)
                ,("http/1.1",httpAttendant) 
                ] port
  where 
    opts_metadata = info 
        ( helper <*> programParser )
        ( fullDesc 
            <>  progDesc "Mimics web servers from .har file"
            <>  header   "reh-mimic"
            )


-- The "PushAction" is a callback that can pull bytes from 
-- some medium (a network socket, for example), while the 
-- PullAction is the opposite. Here I'm saying this: give me two 
-- callbacks for getting and sending data, and I will take care of 
-- the rest.
httpAttendant :: PushAction -> PullAction -> IO ()
httpAttendant push _ = 
    push $ BL.fromChunks [exampleHTTP11Response]


-- spdyAttendant :: String -> PushAction -> PullAction -> IO () 
-- spdyAttendant har_filename push pull = do 
--     har_worker_service_pocket <- initService $ HarWorkerParams har_filename
--     activateSessionManager  
--         id 
--         (basicSession har_worker_service_pocket) 
--         push 
--         pull
--         chunkProducerHelper



certificateFilename :: FilePath -> FilePath -> FilePath 
certificateFilename mimic_dir harfilename = let
    basename = takeFileName harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in 
    namesprefix </> "server.pem" 


privKeyFilename :: FilePath -> FilePath -> FilePath 
privKeyFilename mimic_dir harfilename = let
    basename = takeFileName harfilename
    namesprefix = mimic_dir </> "fakecerts" </> basename
  in 
    namesprefix </> "privkey.pem" 


getComprehensiveCertificate :: FilePath -> FilePath -> [B.ByteString] -> IO ()
getComprehensiveCertificate mimic_dir harfilename all_seen_hosts = do 

    let 
        basename = takeFileName harfilename
        mimic_config_dir = mimic_dir </> "config"
        namesprefix = mimic_dir </> "fakecerts" </> basename

    createDirectoryIfMissing True namesprefix
    let 
        ca_location       = mimic_config_dir </> "ca"
        cnf_filename      = ca_location      </> "openssl-noprompt.cnf"
        db_filename       = ca_location      </> "certindex.txt"
        ca_cert           = ca_location      </> "cacert.pem"
        csr_filename      = namesprefix      </> "cert.csr"
        template_cnf      = namesprefix      </> "openssl.conf" 
        priv_key_filename = privKeyFilename  mimic_dir harfilename
        cert_filename     = namesprefix      </> "server.pem"

    -- Create a food file 
    templatefile <- B.readFile cnf_filename

    let 
        builder0 = Bu.byteString templatefile
        builderM1 = Bu.byteString $ "dir =" `mappend` (pack ca_location) `mappend` "\n\n\n"
        segment_builder = foldMap ( \ (hostname,i) -> 
                    Bu.byteString $ B.concat ["DNS.", (pack $ show i), "=", hostname, "\n"]
                ) all_seen_hosts_and_indices
        all_seen_hosts_and_indices = zip all_seen_hosts [2 .. ] :: [(B.ByteString,Int)]
        complete_builder = builderM1 `mappend` builder0 `mappend` segment_builder

    -- Save it 
    withFile template_cnf WriteMode $ \ handle -> 
        Bu.hPutBuilder handle complete_builder

    -- Invoke openssl to create a signing request
    (_, _, _, h ) <- createProcess $ proc "openssl" 
        ["req", 
            "-outform" , "PEM", 
            "-batch"   ,
            "-new"     , 
            "-newkey"  , "rsa:2048",
            "-nodes"   ,
            "-keyout"  ,  priv_key_filename,
            "-out"     , csr_filename,
            "-config"  , template_cnf
        ]
    waitForProcess h 

    -- Restart the database....
    B.writeFile db_filename ""

    -- Invoke to sign the certificate...
    (_, _, _, h2 ) <- createProcess $ proc "openssl"
        [ 
              "ca"
            , "-config", template_cnf
            , "-in"    , csr_filename
            , "-out"   , cert_filename
            , "-batch"
            , "-cert"  , ca_cert
        ]
    waitForProcess h2

    return ()


outputHosts :: [B.ByteString] -> IO ()
outputHosts all_seen_hosts = do 
    -- all_seen_hosts <- hostsFromHarFile har_filename

    mimic_data_dir <- mimicDataDir
    hosts_filename <- return $ mimic_data_dir </> "har_hosts.txt"
    B.writeFile hosts_filename $ B.intercalate "\n" $ map 
        (\ hostname -> B.append "127.0.0.1      " hostname) 
        all_seen_hosts
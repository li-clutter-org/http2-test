-- TLS is small hell to setup, let's do it here ...

module Rede.MainLoop.Tls(readyTCPSocket
    , tcpServe
    , enchantSocket
    , buildContextParams
    ) where 



-- import           Control.Concurrent
import qualified Crypto.PubKey.DH     as DH
-- import           Data.ASN1.Types      (ASN1 (OctetString), fromASN1)
import qualified Data.ByteString      as B
-- import qualified Data.ByteString.Lazy as LB
import           Data.Default         (def)
import           Data.X509            (decodeSignedCertificate)
import qualified Network.TLS          as T
import           System.FilePath      ((</>))
import           Crypto.Random
import           Network.TLS.Extra.Cipher (ciphersuite_strong)
-- import           Control.Exception
-- import qualified System.FilePath      as FP
-- import           System.IO



import Rede.MainLoop.ConfigHelp(configDir)
-- import qualified Control.Exception as E


import Network.Socket 


-- type DHParams   = DH.Params


readyTCPSocket :: String -> Int ->  IO Socket 
readyTCPSocket hostname portnumber = do 
    the_socket <- socket AF_INET Stream defaultProtocol 
    addr_info0 : _ <- getAddrInfo Nothing (Just hostname) Nothing
    addr_info1  <- return $ addr_info0 {
        addrFamily = AF_INET
        ,addrSocketType = Stream
        ,addrAddress = (\ (SockAddrInet _ a) -> SockAddrInet (fromIntegral portnumber) a) 
                       (addrAddress addr_info0) 
        }
    host_address <- return $ addrAddress addr_info1
    bindSocket the_socket host_address
    return the_socket


tcpServe :: Socket -> ( Socket -> IO () ) -> IO ()
tcpServe  to_bind_socket action =  
    do 
        listen to_bind_socket 10000
        accept_loop to_bind_socket 
  where 
    accept_loop b = do 
        (new_socket, _ ) <- accept b
        action new_socket
        accept_loop b 


buildContextParams :: IO T.ServerParams 
buildContextParams = do 
    config_dir <- configDir

    -- CA certificates
    cacert_fs_path <- return $ config_dir </> "ca/cacert.der" 
    cacert_str <- B.readFile cacert_fs_path  
    (Right casigned_certificate) <- return $ decodeSignedCertificate cacert_str 

    -- My credential
    (Right credential) <- T.credentialLoadX509 
        (config_dir </> "servercert.pem")
        (config_dir </> "privkey.pem")

    return $ def {
        T.serverCACertificates = [ casigned_certificate ]
        ,T.serverWantClientCert = False 
        ,T.serverDHEParams = Just dhParams 
        ,T.serverShared = def {
            T.sharedCredentials = T.Credentials [ credential]
        }
        --,T.serverHooks = _sh
        ,T.serverSupported = serverSupported
        }


serverSupported :: T.Supported
serverSupported = def {
    T.supportedVersions = [T.TLS11]
    ,T.supportedCiphers =  ciphersuite_strong

    }


-- Wired DH params....
--- TODO: Fix here 
dhParams :: DH.Params
dhParams = DH.Params{ 
    DH.params_p = 160870573327511515755694812153035117423465767994224171293799401008595077500999512573342274297754048601232162928745909141418010838816767337684899532156625070107178016199211264183981876937379840682650214449906599372700826256634113979846962538158218926053405148278109540762210692241689283056010768548464169065243
    ,DH.params_g = 2 
    }
   

enchantSocket :: Socket -> T.ServerParams -> IO T.Context
enchantSocket s server_params = do  
    rng <- rngProduce
    ctx <- T.contextNew s server_params rng
    T.handshake ctx
    return ctx


rngProduce :: IO SystemRNG
rngProduce = do 
    rng_pool <- createEntropyPool 
    return $ cprgCreate rng_pool


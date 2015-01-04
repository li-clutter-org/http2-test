module SpdyPing.TLSConnect (
    makeTLSParamsForSpdy
    ) where


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Options.Applicative

import Data.Default
import Data.Default.Class
import Data.Data
import Data.X509.CertificateStore( CertificateStore  )
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Network as N
import qualified Network.Connection as N


-- | Create a final TLS 'ClientParams' according to the destination and the
-- TLSSettings.
makeTLSParamsForSpdy :: N.ConnectionContext -> (String,Int) -> CertificateStore ->  TLS.ClientParams
makeTLSParamsForSpdy cg cid scs  =
    (TLS.defaultParamsClient (fst cid) portString)
        { TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_all }
        , TLS.clientShared    = def
            { TLS.sharedCAStore         = scs
            , TLS.sharedValidationCache = def
            -- , TLS.sharedSessionManager  = connectionSessionManager
            }
        }
  where  portString = BC.pack $ show $ snd cid

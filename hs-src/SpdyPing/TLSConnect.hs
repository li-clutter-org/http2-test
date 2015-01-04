{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpdyPing.TLSConnect (
    makeTLSParamsForSpdy
    ) where


import qualified Data.ByteString.Char8      as BC
import           Data.Default
import           Data.X509.CertificateStore (CertificateStore)

import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLS



-- | Create a final TLS 'ClientParams' according to the destination and the
-- TLSSettings.
makeTLSParamsForSpdy :: (String,Int) -> CertificateStore ->  TLS.ClientParams
makeTLSParamsForSpdy cid scs  =
    (TLS.defaultParamsClient (fst cid) portString)
        { TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_all }
        , TLS.clientShared    = def
            { TLS.sharedCAStore         = scs
            , TLS.sharedValidationCache = def
            }
        }
  where  portString = BC.pack $ show $ snd cid
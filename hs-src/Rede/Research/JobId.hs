{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Rede.Research.JobId(
    jobIdFromUrl
    ,hashidFromJobId

    ,UrlJobId

    -- Use the implementation detail with care
    ,HashId(..)

    ,lnRndDistinctor
    ,lnOriginalUrl
    ,lnReqTimeStamp
    ) where 





import           Control.Lens.Type         (Lens')
import qualified Crypto.Hash.MD5           as MD5
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import qualified Data.ByteString.Builder   as Bu
import qualified Data.Monoid               as Mo
import qualified Data.ByteString.Base16    as B16
import           Data.ByteString.Char8     (pack, 
                                            {-unpack-} )
import qualified Data.Time.Clock           as Cl
import           Data.Hashable             (Hashable(..))
-- import qualified Network.URI               as U
import qualified System.Random             as SR


-- Job id description
data UrlJobId = UrlJobId { 
      originalUrl :: B.ByteString 
    , reqTimestamp :: Cl.UTCTime
    , rndDistinctor :: B.ByteString
  } deriving (Eq, Show)


lnOriginalUrl :: Lens' UrlJobId B.ByteString
lnOriginalUrl f (UrlJobId n e r) = fmap (\a' -> UrlJobId a' e r) (f n)

lnReqTimeStamp :: Lens' UrlJobId Cl.UTCTime
lnReqTimeStamp f (UrlJobId n e r) = fmap (\e' -> UrlJobId n e' r) (f e)

lnRndDistinctor :: Lens' UrlJobId B.ByteString
lnRndDistinctor f (UrlJobId n e r) = fmap (\r' -> UrlJobId n e r') (f r)


instance Hashable UrlJobId where 
    hashWithSalt salt u = hashWithSalt salt $ urlJobIdToByteStringRepr u


instance Hashable HashId where 
    hashWithSalt salt u = hashWithSalt salt $ unHashId u

-- Length of a HashId
idLength :: Int
idLength = 14


-- neutralizeUrl :: B.ByteString -> B.ByteString
-- neutralizeUrl url = let 
--     Just (U.URI {- scheme -} _ authority u_path u_query u_frag) = U.parseURI $ unpack url
--     Just (U.URIAuth _ use_host _) = authority
--     complete_url  = U.URI {
--         U.uriScheme     = "snu:"
--         ,U.uriAuthority = Just $ U.URIAuth {
--             U.uriUserInfo = ""
--             ,U.uriRegName = use_host 
--             ,U.uriPort    = ""
--             }
--         ,U.uriPath      = u_path
--         ,U.uriQuery     = u_query 
--         ,U.uriFragment  = u_frag 
--       }
--   in 
--     pack $ show complete_url


newtype HashId = HashId { unHashId::B.ByteString }
    deriving (Eq, Show)


hashidFromJobId :: UrlJobId -> HashId
hashidFromJobId url = 
    HashId . B.take idLength . B16.encode . MD5.finalize $ foldl MD5.update MD5.init $  
      [
        urlJobIdToByteStringRepr url
      ]


jobIdFromUrl :: B.ByteString -> IO UrlJobId 
jobIdFromUrl url = do 
    utc_time <- Cl.getCurrentTime
    -- My high order thinking is flawed right now
    a <- SR.randomIO 
    b <- SR.randomIO
    c <- SR.randomIO 
    d <- SR.randomIO
    e <- SR.randomIO
    return $ UrlJobId {
      originalUrl = url
      ,reqTimestamp = utc_time
      ,rndDistinctor = pack [a,b,c,d,e]
      }


urlJobIdToByteStringRepr :: UrlJobId -> B.ByteString
urlJobIdToByteStringRepr (UrlJobId n e r) = LB.toStrict $ Bu.toLazyByteString $ Mo.mconcat [
    Bu.byteString n,
    (Bu.byteString . pack . show  $ e),
    Bu.byteString r
    ]
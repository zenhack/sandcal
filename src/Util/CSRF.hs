{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Helper module for CSRF protection.
module Util.CSRF
    ( Key
    , Mac
    , PostCap(..)
    , genKey
    , makeCsrfToken
    , verifyPostRoute
    ) where

import Zhp

import qualified Route
import qualified Sandstorm


import Crypto.Hash               (SHA3_512, digestFromByteString)
import GHC.Generics              (Generic)
import Network.HTTP.Types.Status (status401)

import qualified Crypto.MAC.HMAC         as HMAC
import qualified Crypto.Random           as Rand
import qualified Data.Aeson              as Aeson
import qualified Data.ByteArray          as ByteArray
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as Base64
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text.Lazy.Encoding as TE
import qualified Web.Scotty              as Ws

newtype Key = Key BS.ByteString

newtype Mac
    = Mac (HMAC.HMAC SHA3_512)
    deriving(Eq)

instance Ws.Parsable Mac where
    parseParam txt = do
        case b64ToMac (LBS.toStrict $ TE.encodeUtf8 txt) of
            Nothing  -> Left "base64 decoding failed"
            Just mac -> Right mac

data PostCap = PostCap
    { route  :: Route.PostRoute
    , userId :: Maybe Sandstorm.UserId
    }
    deriving(Generic)
instance Aeson.ToJSON PostCap
instance Aeson.FromJSON PostCap

genKey :: IO Key
genKey = do
    drg <- Rand.getSystemDRG
    let (bytes, _) = Rand.randomBytesGenerate (512 `div` 8) drg
    pure $ Key bytes

makeCsrfToken :: Key -> PostCap -> String
makeCsrfToken key cap =
    BS8.unpack $ macToB64 $ macPostCap key cap

verifyPostRoute :: Key -> Route.PostRoute -> Ws.ActionM ()
verifyPostRoute key route = do
    mac <- Ws.param "csrfToken"
    userId <- Sandstorm.maybeGetUserId
    verifyPostCap key PostCap{ route, userId } mac

macPostCap :: Key -> PostCap -> Mac
macPostCap (Key key) cap =
    Mac $ HMAC.hmac key $ LBS.toStrict $ Aeson.encode cap

validPostCapMac :: Key -> PostCap -> Mac -> Bool
validPostCapMac key cap mac =
    -- Cryptonite docs say that HMac's Eq instance is constant
    -- time, so suitable to verify the mac:
    macPostCap key cap == mac

verifyPostCap :: Key -> PostCap -> Mac -> Ws.ActionM ()
verifyPostCap key cap mac =
    unless (validPostCapMac key cap mac) $
        Ws.raiseStatus status401 "Invalid CSRF token"

b64ToMac :: BS.ByteString -> Maybe Mac
b64ToMac b64 =
    case Base64.decode b64 of
        Left _ -> Nothing
        Right bytes ->
            Mac . HMAC.HMAC <$> digestFromByteString bytes

macToB64 :: Mac -> BS.ByteString
macToB64 (Mac m) =
    Base64.encode $ BS.pack $ ByteArray.unpack $ HMAC.hmacGetDigest m

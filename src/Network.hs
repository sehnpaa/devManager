{-# LANGUAGE OverloadedStrings #-}

module Network where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(EitherT))
import Data.Aeson
       (Value(String), (.=), eitherDecode', encode, object)
import Data.Aeson.Lens (AsValue, _Number, _String, key, nth)
import qualified Data.ByteString.Char8 as S8 (append, concat, pack)
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Data.Scientific (Scientific, coefficient)
import Data.Text as T (Text, pack)
import Lens.Micro ((^?))
import Network.HTTP.Client
       (Request, RequestBody(RequestBodyLBS), Response, httpLbs,
        newManager, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit
       (defaultRequest, host, method, path, requestBody, requestHeaders,
        secure)
import Network.HTTP.Types.Header
       (Header, hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (getArgs)

import Token (getToken, tokenSanityCheck)
import Types
       (DropletId(DropletId), Error(..), SnapshotId(SnapshotId),
        Success(..), Token, getSecret, unDropletId, unSnapshotId)

snapshotsRequest :: Token -> Request
snapshotsRequest token =
  defaultRequest
  { host = "api.digitalocean.com"
  , path = "/v2/snapshots"
  , requestHeaders = authHeaders token
  , secure = False
  }

startDropletRequest :: Token -> RequestBody -> Request
startDropletRequest token body =
  defaultRequest
  { host = "api.digitalocean.com"
  , method = "POST"
  , path = "/v2/droplets"
  , requestBody = body
  , requestHeaders = authHeaders token
  , secure = False
  }

destroyDropletRequest :: Token -> DropletId -> Request
destroyDropletRequest token dropletId =
  defaultRequest
  { host = "api.digitalocean.com"
  , method = "DELETE"
  , path =
      S8.concat
        ["/v2/droplets/", S8.pack . show . coefficient $ unDropletId dropletId]
  , requestHeaders = authHeaders token
  , secure = False
  }

authHeaders :: Token -> [Header]
authHeaders token =
  [ (hAuthorization, S8.append "Bearer " (S8.pack . getSecret $ token))
  , (hContentType, "application/json")
  ]

requestObject :: SnapshotId -> Value
requestObject id =
  object
    [ ("name" :: Text) .= ("haskellbox" :: Value)
    , ("image" :: Text) .= (String $ unSnapshotId id :: Value)
    , ("region" :: Text) .= ("ams3" :: Value)
    , ("size" :: Text) .= ("c-2" :: Value)
    ]

mapError :: (a -> c) -> Either a b -> Either c b
mapError f (Left x) = Left $ f x
mapError _ (Right x) = Right x

decodeBody :: Response L8.ByteString -> Either Error Value
decodeBody = mapError toParseResponseError . eitherDecode' . responseBody

toParseResponseError :: String -> Error
toParseResponseError = ParseResponse . T.pack

getSnapshotId :: AsValue s => s -> Maybe T.Text
getSnapshotId x = x ^? key "snapshots" . nth 0 . key "id" . _String

getDropletId :: AsValue s => s -> Maybe Scientific
getDropletId x = x ^? key "droplet" . key "id" . _Number

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

snapshotRequest :: Token -> SnapshotId -> Request
snapshotRequest token =
  startDropletRequest token . RequestBodyLBS . encode . requestObject

parseDropletId :: Response ByteString -> Either Error Success
parseDropletId =
  fmap (DropletCreated . DropletId) . maybeToEither ParseDropletId .
  getDropletId .
  responseBody

parseSnapshotId :: Response ByteString -> Either Error SnapshotId
parseSnapshotId =
  fmap SnapshotId . maybeToEither ParseSnapshotId . getSnapshotId . responseBody

startSnapshotIO :: Token -> SnapshotId -> IO (Either Error Success)
startSnapshotIO token id = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotRequest token id) manager
  return . parseDropletId $ response

getSnapshotIO :: Token -> IO (Either Error SnapshotId)
getSnapshotIO token = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotsRequest token) manager
  return . parseSnapshotId $ response

destroyDropletIO :: Token -> DropletId -> IO (Either Error Success)
destroyDropletIO token id = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (destroyDropletRequest token id) manager
  case statusCode (responseStatus response) of
    204 -> return $ Right $ DropletRemoved id
    n -> return $ mapError DropletIdNotFound $ Left n

getTokenIO :: IO (Either Error Token)
getTokenIO = do
  args <- getArgs
  return $ getToken args >>= tokenSanityCheck

startDropletFromSnapshot :: EitherT Error IO Success
startDropletFromSnapshot = do
  token <- EitherT getTokenIO
  liftIO $ putStrLn "Token received"
  snapshotId <- EitherT $ getSnapshotIO token
  EitherT $ startSnapshotIO token snapshotId

destroyDroplet :: DropletId -> EitherT Error IO Success
destroyDroplet id = do
  token <- EitherT getTokenIO
  EitherT $ destroyDropletIO token id

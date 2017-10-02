{-# LANGUAGE OverloadedStrings #-}

module Network where

import Control.Monad.Trans.Either (EitherT(EitherT))
import qualified Data.ByteString.Char8 as S8 (append, concat, pack)
import Data.ByteString.Lazy.Char8 as L8 (unpack)
import Data.Scientific (coefficient)
import Data.Text as T (pack)
import Network.HTTP.Client (Request, RequestBody(RequestBodyLBS))
import Network.HTTP.Conduit
       (defaultRequest, host, method, path, requestBody, requestHeaders,
        secure)
import Network.HTTP.Types.Header
       (Header, hAuthorization, hContentType)
import Network.HTTP.Types.Method (methodDelete, methodPost)
import Network.HTTP.Types.Status (statusCode)
import Prelude hiding (id)

import Boundaries (MonadArgs, MonadDisplay, MonadHttpRequest, httpRequest)
import Parse (encodeRequestObject, getDropletId, getSnapshotId)
import Token (getTokenIO)
import Types
       (CResponse(CResponse), DropletId(DropletId), Error(..),
        SnapshotId(SnapshotId), Success(..), Token, getSecret, unDropletId)

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
  , method = methodPost
  , path = "/v2/droplets"
  , requestBody = body
  , requestHeaders = authHeaders token
  , secure = False
  }

destroyDropletRequest :: Token -> DropletId -> Request
destroyDropletRequest token dropletId =
  defaultRequest
  { host = "api.digitalocean.com"
  , method = methodDelete
  , path =
      S8.concat
        [ "/v2/droplets/"
        , S8.pack . show . coefficient . unDropletId $ dropletId
        ]
  , requestHeaders = authHeaders token
  , secure = False
  }

authHeaders :: Token -> [Header]
authHeaders token =
  [ (hAuthorization, S8.append "Bearer " (S8.pack . getSecret $ token))
  , (hContentType, "application/json")
  ]

toParseResponseError :: String -> Error
toParseResponseError = ParseResponse . T.pack

mapError :: (a -> c) -> Either a b -> Either c b
mapError f (Left x) = Left $ f x
mapError _ (Right x) = Right x

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

snapshotRequest :: Token -> SnapshotId -> Request
snapshotRequest token =
  startDropletRequest token . RequestBodyLBS . encodeRequestObject

parseDropletId :: CResponse -> Either Error Success
parseDropletId (CResponse _ body) =
  fmap (DropletCreated . DropletId) .
  maybeToEither ParseDropletId . getDropletId $
  body

parseSnapshotId :: CResponse -> Either Error SnapshotId
parseSnapshotId (CResponse _ body) =
  fmap SnapshotId .
  maybeToEither (ParseSnapshotId $ T.pack . L8.unpack $ body) . getSnapshotId $
  body

startSnapshotIO ::
     (MonadHttpRequest m) => Token -> SnapshotId -> m (Either Error Success)
startSnapshotIO token id =
  httpRequest (snapshotRequest token id) >>= return . parseDropletId

getSnapshotIO :: (MonadHttpRequest m) => Token -> m (Either Error SnapshotId)
getSnapshotIO token = do
  httpRequest (snapshotsRequest token) >>= return . parseSnapshotId

destroyDropletIO ::
     (MonadHttpRequest m) => Token -> DropletId -> m (Either Error Success)
destroyDropletIO token id = do
  (CResponse s _) <- httpRequest (destroyDropletRequest token id)
  case statusCode s of
    204 -> return $ Right $ DropletRemoved id
    n -> return $ mapError DropletIdNotFound $ Left n

startDropletFromSnapshot ::
     (MonadDisplay m, MonadArgs m, MonadHttpRequest m)
  => EitherT Error m Success
startDropletFromSnapshot = do
  token <- EitherT getTokenIO
  snapshotId <- EitherT $ getSnapshotIO token
  EitherT $ startSnapshotIO token snapshotId

destroyDroplet ::
     (MonadArgs m, MonadHttpRequest m) => DropletId -> EitherT Error m Success
destroyDroplet id = do
  token <- EitherT getTokenIO
  EitherT $ destroyDropletIO token id

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT (EitherT), runEitherT)
import           Data.Aeson ((.=), Value (Number), eitherDecode', encode, object)
import           Data.Aeson.Lens (AsValue, key, nth, _Number, _String)
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Scientific (Scientific, coefficient, scientific)
import           Data.Text as T (Text, concat, pack, unpack)
import           Lens.Micro ((^?))
import           Network.HTTP.Client (httpLbs, newManager, responseBody, responseStatus, Response, Request, RequestBody (RequestBodyLBS))
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Conduit (defaultRequest, host, method, path, requestBody, requestHeaders, secure)
import           Network.HTTP.Types.Header (Header, hAuthorization, hContentType)
import           Network.HTTP.Types.Status (statusCode)
import           Prelude hiding (id)
import           System.Environment (getArgs)
import           System.Exit (ExitCode (ExitFailure), exitWith)
import           Text.Read (reads)

import           Network (authHeaders, destroyDropletRequest, snapshotsRequest, startDropletRequest)
import           Token (getToken, tokenSanityCheck)
import           Types (DropletId (DropletId), Error (..), SnapshotId (SnapshotId), Token, getSecret, unDropletId, unSnapshotId)

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

parseText :: Text -> Either Error Scientific
parseText text = case (reads . T.unpack $ text :: [(Integer, String)]) of
                  [(x, [])] -> Right (scientific x 0)
                  _ -> mapError ParseResponse err
                  where
                    err = Left $ T.concat ["Could not cast Text '", text, "' to Scientific"]

parseSnapshotId :: Response ByteString -> Either Error Text
parseSnapshotId = maybeToEither ParseSnapshotId . getSnapshotId . responseBody

parseDropletId :: Response ByteString -> Either Error DropletId
parseDropletId =
  fmap DropletId . maybeToEither ParseDropletId . getDropletId . responseBody

requestObject :: SnapshotId -> Value
requestObject id = object
  [ ("name" :: Text) .= ("haskellbox" :: Value)
  , ("image" :: Text).= (Number $ unSnapshotId id :: Value)
  , ("region" :: Text).= ("ams3" :: Value)
  , ("size" :: Text).= ("c-2" :: Value) ]

snapshotRequest :: Token -> SnapshotId -> Request
snapshotRequest token = startDropletRequest token . RequestBodyLBS . encode . requestObject

startSnapshotIO :: Token -> SnapshotId -> IO (Either Error DropletId)
startSnapshotIO token id = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotRequest token id) manager
  return . fmap DropletId . maybeToEither ParseDropletId $ getDropletId $
    responseBody response

getSnapshotIO :: Token -> IO (Either Error SnapshotId)
getSnapshotIO token = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotsRequest token) manager
  return $ fmap SnapshotId (parseSnapshotId response >>= parseText)

destroyDropletIO :: Token -> DropletId -> IO (Either Error DropletId)
destroyDropletIO token id = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (destroyDropletRequest token id) manager
  case statusCode (responseStatus response) of
    204 -> return $ Right id
    n ->
      return $ mapError (DropletIdNotFound) $ Left $
      T.concat ["Response status code: ", T.pack . show $ n]

getTokenIO :: IO (Either Error Token)
getTokenIO = do
  args <- getArgs
  return $ mapError (const NoToken) $ (getToken args >>= tokenSanityCheck)

startDropletFromSnapshot :: EitherT Error IO DropletId
startDropletFromSnapshot = do
  token <- EitherT getTokenIO
  liftIO $ putStrLn "Token received"
  snapshotId <- EitherT $ getSnapshotIO token
  EitherT $ startSnapshotIO token snapshotId

destroyDroplet :: DropletId -> EitherT Error IO DropletId
destroyDroplet id = do
  token <- EitherT getTokenIO
  EitherT $ destroyDropletIO token id

wait :: DropletId -> EitherT Error IO DropletId
wait id = liftIO (threadDelay (200*1000*1000)) >> return id

main :: IO ()
main = do
  dropletId <- runEitherT (startDropletFromSnapshot >>= wait >>= destroyDroplet)
  case dropletId of
    (Right id) -> putStrLn . (++) "Success: ". show . coefficient . unDropletId $ id
    (Left err) -> putStrLn . (++) "Error: " $ show err
  return ()

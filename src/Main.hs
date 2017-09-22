{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import Data.Aeson
       (Value(Number, String), (.=), eitherDecode', encode, object)
import Data.Aeson.Lens (AsValue, _Number, _String, key, nth)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Data.Scientific (Scientific, coefficient, scientific)
import Data.Text as T (Text, concat, pack, unpack)
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
import Prelude hiding (id)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.Read (reads)

import Network
       (authHeaders, destroyDropletRequest, snapshotsRequest,
        startDropletRequest)
import Token (getToken, tokenSanityCheck)
import Types
       (DropletId(DropletId), Error(..), SnapshotId(SnapshotId), Token,
        getSecret, unDropletId, unSnapshotId)

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
parseText text =
  case (reads . T.unpack $ text :: [(Integer, String)]) of
    [(x, [])] -> Right (scientific x 0)
    _ -> mapError ParseResponse err
  where
    err = Left $ T.concat ["Could not cast Text '", text, "' to Scientific"]

parseSnapshotId :: Response ByteString -> Either Error SnapshotId
parseSnapshotId = fmap SnapshotId . maybeToEither ParseSnapshotId . getSnapshotId . responseBody

parseDropletId :: Response ByteString -> Either Error DropletId
parseDropletId =
  fmap DropletId . maybeToEither ParseDropletId . getDropletId . responseBody

requestObject :: SnapshotId -> Value
requestObject id =
  object
    [ ("name" :: Text) .= ("haskellbox" :: Value)
    , ("image" :: Text) .= (String $ unSnapshotId id :: Value)
    , ("region" :: Text) .= ("ams3" :: Value)
    , ("size" :: Text) .= ("c-2" :: Value)
    ]

snapshotRequest :: Token -> SnapshotId -> Request
snapshotRequest token =
  startDropletRequest token . RequestBodyLBS . encode . requestObject

startSnapshotIO :: Token -> SnapshotId -> IO (Either Error DropletId)
startSnapshotIO token id = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotRequest token id) manager
  return . parseDropletId $ response

getSnapshotIO :: Token -> IO (Either Error SnapshotId)
getSnapshotIO token = do
  manager <- newManager tlsManagerSettings
  response <- httpLbs (snapshotsRequest token) manager
  -- return $ fmap SnapshotId (parseSnapshotId response >>= parseText)
  return . parseSnapshotId $ response

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
wait id = liftIO (threadDelay (200 * 1000 * 1000)) >> return id

data Command
  = CreateCommand
  | RemoveCommand
  | QuitCommand
  | UnknownCommand

parseInput :: String -> Command
parseInput "create" = CreateCommand
parseInput "remove" = RemoveCommand
parseInput "quit" = QuitCommand
parseInput _ = UnknownCommand

data Env =
  Env (Maybe DropletId)
      (Maybe SnapshotId)
  deriving (Show)

emptyEnv = Env Nothing Nothing

updateEnvDropletId (Env _ a) newId = Env (Just newId) a

clearEnvDropletId (Env _ a) = Env Nothing a

updateEnvSnapshotIdId (Env a _) newId = Env a (Just newId)

main :: IO ()
main = runInputT defaultSettings $ loop emptyEnv
  where
    loop :: Env -> InputT IO ()
    loop env@(Env mayDropletId maySnapshotId) = do
      outputStrLn "--------------"
      outputStrLn $ "Env: " ++ show env
      outputStrLn "--------------"
      minput <- getInputLine "devManager> "
      case minput of
        Nothing -> outputStrLn "No input"
        Just n ->
          case parseInput n of
            CreateCommand -> do
              outputStrLn "Creating a new droplet..."
              dropletId <- liftIO . runEitherT $ startDropletFromSnapshot
              case dropletId of
                (Right id) -> do
                  outputStrLn . (++) "Success: " . show . coefficient .
                    unDropletId $
                    id
                  loop $ updateEnvDropletId env id
                (Left err) -> do
                  outputStrLn . (++) "Error: " $ show err
                  loop env
            RemoveCommand -> do
              outputStrLn "Removing droplet..."
              case mayDropletId of
                Nothing -> outputStrLn "No droplet id in env" >> loop env
                (Just id) -> do
                  dropletId <- liftIO . runEitherT $ destroyDroplet id
                  outputStrLn $ "Removed droplet with id " ++ show dropletId
                  loop $ clearEnvDropletId env
              return ()
            QuitCommand -> do
              outputStrLn "Quitting..."
              return ()
            _ -> do
              outputStrLn "Unrecognized command."
              loop env

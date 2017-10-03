{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Boundaries where

import Control.Monad.State
import Data.ByteString.Lazy.Char8 as L8 (pack)
import Network.HTTP.Client
       (Request(), RequestBody(RequestBodyLBS), httpLbs, method, newManager, responseBody,
        responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Network.HTTP.Types.Method
       (StdMethod(DELETE), parseMethod)
import Network.HTTP.Types.Status
       (status200, status204, status400, status500)
import Prelude hiding (id)
import System.Environment (getArgs)

import Parse
import Types (CResponse(CResponse), Op(..), MockEnv(..), SnapshotId, testToken)

class Monad m =>
      MonadArgs m where
  getArguments :: m [String]

instance MonadArgs IO where
  getArguments = getArgs

instance MonadArgs (State MockEnv) where
  getArguments = do
    get >>= (\_ -> return [testToken])

class Monad m =>
      MonadDisplay m where
  output :: String -> m ()

instance MonadDisplay IO where
  output = putStrLn

instance MonadDisplay (State MockEnv) where
  output _ = undefined -- put s >> return ()

class Monad m =>
      MonadHttpRequest m where
   httpRequest :: Request -> Op -> m CResponse

instance MonadHttpRequest IO where
  httpRequest req _ = do
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    return (CResponse (responseStatus response) (responseBody response))

f :: SnapshotId -> RequestBody
f = RequestBodyLBS . encodeRequestObject

instance MonadHttpRequest (State MockEnv) where
  httpRequest req (NewDroplet _) = return $ CResponse status body
    where
      body =
        L8.pack "{\"snapshots\": [{\"id\": \"123\"}], \"droplet\": {\"id\": 1}}"
      -- body = L8.pack "{\"snapshots\": [{\"id\": \"123\"}]}"
      -- body = L8.pack "{\"droplet\": {\"id\": 123}}"
      status =
        case (parseMethod $ method req) of
          (Right DELETE) -> status200
          (Left _) -> status400
          _ -> status500
  httpRequest _ (RemoveDroplet _ _) = do
    _ <- get
    return $ CResponse status body
      where
        body = L8.pack "{\"droplet\": {\"id\": 999}}"
        status = status204
  httpRequest _ _ = undefined

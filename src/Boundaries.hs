{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Boundaries where

import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, unpack)
import Network.HTTP.Client
       (Request, Response, responseBody, responseStatus, httpLbs, method, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (StdMethod(DELETE), methodDelete, methodPost, parseMethod)
import Network.HTTP.Types.Status (Status, statusCode, status200, status204, status400)
import System.Environment (getArgs)

import Types

class Monad m => MonadArgs m where
  getArguments :: m [String]

instance MonadArgs IO where
  getArguments = getArgs

instance MonadArgs (State String) where
  getArguments = get >>= (\n -> return [n])


class Monad m => MonadDisplay m where
  output :: String -> m ()

instance MonadDisplay IO where
  output = putStrLn

instance MonadDisplay (State String) where
  output s = put s >> return ()


class Monad m => MonadHttpRequest m where
  httpRequest :: Request -> m CResponse

instance MonadHttpRequest IO where
  httpRequest req = do
    manager <- newManager tlsManagerSettings
    response <- httpLbs req manager
    return (CResponse (responseStatus response) (responseBody response))


instance MonadHttpRequest (State String) where
  httpRequest req = return $ CResponse status body
    where
      -- status = status204
      body = L8.pack "{\"snapshots\": [{\"id\": \"123\"}], \"droplet\": {\"id\": 3}}"
      -- body = L8.pack "{\"snapshots\": [{\"id\": \"123\"}]}"
      -- body = L8.pack "{\"droplet\": {\"id\": 123}}"
      status = case (parseMethod $ method req) of
                 (Right DELETE) -> status200
                 (Left _) -> status400

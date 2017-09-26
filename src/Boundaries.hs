module Boundaries where

import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Network.HTTP.Client
       (Manager, ManagerSettings, Request, Response, httpLbs, newManager)
import System.Environment (getArgs)

class Monad m => MonadArgs m where
  getArguments :: m [String]

instance MonadArgs IO where
  getArguments = getArgs


class Monad m => MonadDisplay m where
  output :: String -> m ()

instance MonadDisplay IO where
  output = putStrLn


class Monad m => MonadT m where
  newManager_ :: ManagerSettings -> m Manager

instance MonadT IO where
  newManager_ = newManager


class Monad m => MonadT2 m where
  mkHttp :: Request -> Manager -> m (Response ByteString)

instance MonadT2 IO where
  mkHttp = httpLbs

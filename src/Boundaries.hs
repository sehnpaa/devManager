{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Boundaries where

import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Network.HTTP.Client
       (Request, Response, httpLbs, newManager)

import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getArgs)

import Types

class Monad m => MonadArgs m where
  getArguments :: m [String]

instance MonadArgs IO where
  getArguments = getArgs

instance MonadArgs (Reader [String]) where
  getArguments = ask >>= return


class Monad m => MonadDisplay m where
  output :: String -> m ()

instance MonadDisplay IO where
  output = putStrLn

instance MonadDisplay (State String) where
  output s = put s >> return ()

class Monad m => MonadHttpRequest m where
  httpRequest :: Token -> (Token -> Request) -> m (Response ByteString)

instance MonadHttpRequest IO where
  httpRequest token g = do
    manager <- newManager tlsManagerSettings
    response <- httpLbs (g token) manager
    return response

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Prelude hiding (id)
import System.Console.Haskeline
       (InputT, MonadException, defaultSettings, getInputLine, outputStrLn, runInputT)

import Boundaries
import Network (destroyDroplet, startDropletFromSnapshot)
import Types
       (Command(..), DropletId, Env(..), Error(..), SnapshotId,
        Success(..))

wait :: DropletId -> EitherT Error IO DropletId
wait id = liftIO (threadDelay (200 * 1000 * 1000)) >> return id

parseInput :: String -> Command
parseInput "create" = CreateCommand
parseInput "remove" = RemoveCommand
parseInput "quit" = QuitCommand
parseInput _ = UnknownCommand

emptyEnv :: Env
emptyEnv = Env Nothing Nothing

updateEnvDropletId :: Env -> DropletId -> Env
updateEnvDropletId (Env _ a) newId = Env (Just newId) a

clearEnvDropletId :: Env -> Env
clearEnvDropletId (Env _ a) = Env Nothing a

updateEnvSnapshotIdId :: Env -> SnapshotId -> Env
updateEnvSnapshotIdId (Env a _) newId = Env a (Just newId)

run :: (MonadArgs m, MonadDisplay m, MonadHttpRequest m) => Env -> Command -> m (Either Error Success)
run _ CreateCommand = runEitherT startDropletFromSnapshot
run (Env mayDropletId _) RemoveCommand =
  case mayDropletId of
    Nothing -> return $ Left MissingDropletIdInEnv
    (Just n) -> runEitherT $ destroyDroplet n
run _ UnknownCommand = return $ Left NotACommand
run _ QuitCommand = return $ Left Quit

updateEnv :: Success -> Env -> Env
updateEnv (DropletCreated id) env = updateEnvDropletId env id
updateEnv (DropletRemoved _) env = clearEnvDropletId env

newEnvBasedOn :: Either Error Success -> Env -> Env
newEnvBasedOn (Right success) env = updateEnv success env
newEnvBasedOn (Left _) env = env

loop :: (MonadException m, MonadIO m) => Env -> InputT m ()
loop env = do
  outputStrLn "--------------"
  outputStrLn $ "Env: " ++ show env
  outputStrLn "--------------"
  minput <- getInputLine "devManager> "
  case minput of
    Nothing -> outputStrLn "No input"
    Just n -> do
      res <- liftIO . run env . parseInput $ n
      case res of
        (Left Quit) -> return ()
        _ -> do
          outputStrLn $ show res
          loop $ newEnvBasedOn res env

main :: IO ()
main = runInputT defaultSettings $ loop emptyEnv

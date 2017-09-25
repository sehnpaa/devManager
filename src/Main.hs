{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import System.Console.Haskeline
       (InputT, defaultSettings, getInputLine,
        outputStrLn, runInputT)

import Network (destroyDroplet, startDropletFromSnapshot)
import Types
       (Command(..), DropletId(DropletId), Env(..), Error(..), SnapshotId,
        Success(..),
        unDropletId, unSnapshotId)

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

run :: String -> Env -> IO (Either Error Success)
run s (Env mayDropletId _) =
  case parseInput s of
    CreateCommand -> runEitherT startDropletFromSnapshot
    RemoveCommand ->
      case mayDropletId of
        Nothing -> return $ Left MissingDropletIdInEnv
        (Just n) -> runEitherT $ destroyDroplet n
    UnknownCommand -> return $ Left NotACommand

updateEnv :: Success -> Env -> Env
updateEnv (DropletCreated id) env = updateEnvDropletId env id
updateEnv (DropletRemoved _) env = clearEnvDropletId env

newEnvBasedOn :: Either Error Success -> Env -> Env
newEnvBasedOn (Right success) env = updateEnv success env
newEnvBasedOn (Left _) env = env

main :: IO ()
main = runInputT defaultSettings $ loop emptyEnv
  where
    loop :: Env -> InputT IO ()
    loop env = do
      outputStrLn "--------------"
      outputStrLn $ "Env: " ++ show env
      outputStrLn "--------------"
      minput <- getInputLine "devManager> "
      case minput of
        Nothing -> outputStrLn "No input"
        Just n -> do
          res <- liftIO $ run n env
          outputStrLn $ show res
          loop $ newEnvBasedOn res env

module Types where

import Data.ByteString.Lazy.Char8 as L8 (ByteString)
import Data.Scientific (Scientific, coefficient)
import Data.Text (Text, unpack)
import Network.HTTP.Types.Status (Status)

type Body = String

data CResponse = CResponse Status ByteString

newtype Token = Token
  { getSecret :: String
  }

data Env =
  Env (Maybe DropletId)
      (Maybe SnapshotId)
  deriving (Show)

data Command
  = CreateCommand
  | RemoveCommand
  | QuitCommand
  | UnknownCommand

data Success
  = DropletCreated DropletId
  | DropletRemoved DropletId
  deriving (Show)

type HTTPError = Int

data Error
  = TokenLength Int
  | NoToken
  | ParseResponse Text
  | ParseSnapshotId Text
  | ParseDropletId
  | DropletIdNotFound HTTPError
  | TextToScientific Text
  | NotACommand
  | MissingDropletIdInEnv
  | Quit

instance Show Error where
  show (TokenLength n) = "Token length is " ++ show n ++ "."
  show NoToken = "You need to supply at least one argument."
  show (ParseResponse s) =
    "Could not parse Text '" ++ Data.Text.unpack s ++ "' to Scientific"
  show (ParseSnapshotId s) = "Could not parse snapshot id from body: " ++ unpack s
  show ParseDropletId = "Could not parse droplet id."
  show (DropletIdNotFound n) =
    "Droplet id not found. Response status code: " ++ show n
  show (TextToScientific s) =
    "Could not parse Text '" ++ unpack s ++ "' to Scientific"
  show NotACommand = "That is not a recognized command."
  show MissingDropletIdInEnv = "Missing Droplet id in env"
  show Quit = "Quitting..."

newtype SnapshotId = SnapshotId
  { unSnapshotId :: Text
  }

instance Show SnapshotId where
  show = Data.Text.unpack . unSnapshotId

newtype DropletId = DropletId
  { unDropletId :: Scientific
  }

instance Show DropletId where
  show = show . coefficient . unDropletId

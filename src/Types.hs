module Types where

import Data.Scientific (Scientific, coefficient)
import Data.Text (Text, unpack)

newtype Token = Token
  { getSecret :: String
  }

data Error
  = TokenLength Int
  | NoToken
  | ParseResponse Text
  | ParseSnapshotId
  | ParseDropletId
  | DropletIdNotFound Text
  | TextToScientific Text

instance Show Error where
  show (TokenLength n) = "Token length is " ++ show n ++ "."
  show NoToken = "You need to supply at least one argument."
  show (ParseResponse s) =
    "Could not parse Text '" ++ Data.Text.unpack s ++ "' to Scientific"
  show ParseSnapshotId = "Could not parse snapshot id."
  show ParseDropletId = "Could not parse droplet id."
  show (DropletIdNotFound n) = "Response status code: " ++ show n
  show (TextToScientific s) =
    "Could not parse Text '" ++ Data.Text.unpack s ++ "' to Scientific"

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

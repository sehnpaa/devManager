module Types where

import           Data.Text (Text, unpack)
import           Data.Scientific (Scientific, coefficient, scientific)

newtype Token = Token { getSecret :: String }

data Error = TokenLength Int | NoToken | CastTextToScientific Text | ParseSnapshotId | ParseDropletId | DropletIdNotFound Int

instance Show Error where
  show (TokenLength n) = "Token length is " ++ show n ++ "."
  show NoToken = "You need to supply at least one argument."
  show (CastTextToScientific s) =
    "Could not cast Text '" ++ Data.Text.unpack s ++ "' to Scientific"
  show ParseSnapshotId = "Could not parse snapshot id."
  show ParseDropletId = "Could not parse droplet id."
  show (DropletIdNotFound n) = "Response status code: " ++ show n

newtype SnapshotId = SnapshotId { unSnapshotId :: Scientific }
newtype DropletId = DropletId { unDropletId :: Scientific }


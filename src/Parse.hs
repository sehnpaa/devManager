{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
       (Value(String), (.=), eitherDecode', encode, object)
import Data.Aeson.Lens (AsValue, _Number, _String, key, nth)
import Data.Scientific (Scientific, coefficient)
import Data.Text (Text)
import Lens.Micro ((^?))

import Types

requestObject :: SnapshotId -> Value
requestObject id =
  object
    [ ("name" :: Text) .= ("haskellbox" :: Value)
    , ("image" :: Text) .= (String $ unSnapshotId id :: Value)
    , ("region" :: Text) .= ("ams3" :: Value)
    , ("size" :: Text) .= ("c-2" :: Value)
    ]

getSnapshotId :: AsValue s => s -> Maybe Text
getSnapshotId x = x ^? key "snapshots" . nth 0 . key "id" . _String

getDropletId :: AsValue s => s -> Maybe Scientific
getDropletId x = x ^? key "droplet" . key "id" . _Number

encodeRequestObject = encode . requestObject

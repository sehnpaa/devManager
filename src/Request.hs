{-# LANGUAGE OverloadedStrings #-}

module Request where

import qualified Data.ByteString.Char8 as S8 (append, concat, pack)
import Data.Scientific (coefficient)
import Network.HTTP.Client (Request, RequestBody())
import Network.HTTP.Conduit
       (defaultRequest, host, method, path, requestBody, requestHeaders,
        secure)
import Network.HTTP.Types.Header
       (Header, hAuthorization, hContentType)
import Network.HTTP.Types.Method (methodDelete, methodPost)
import Prelude hiding (id)

import Types (DropletId, Token, getSecret, unDropletId)

snapshotsRequest :: Token -> Request
snapshotsRequest token =
  defaultRequest
  { host = "api.digitalocean.com"
  , path = "/v2/snapshots"
  , requestHeaders = authHeaders token
  , secure = False
  }

startDropletRequest :: Token -> RequestBody -> Request
startDropletRequest token body =
  defaultRequest
  { host = "api.digitalocean.com"
  , method = methodPost
  , path = "/v2/droplets"
  , requestBody = body
  , requestHeaders = authHeaders token
  , secure = False
  }

destroyDropletRequest :: Token -> DropletId -> Request
destroyDropletRequest token dropletId =
  defaultRequest
  { host = "api.digitalocean.com"
  , method = methodDelete
  , path =
      S8.concat
        [ "/v2/droplets/"
        , S8.pack . show . coefficient . unDropletId $ dropletId
        ]
  , requestHeaders = authHeaders token
  , secure = False
  }

authHeaders :: Token -> [Header]
authHeaders token =
  [ (hAuthorization, S8.append "Bearer " (S8.pack . getSecret $ token))
  , (hContentType, "application/json")
  ]

module Token where

import Types (Error(..), Token(Token), getSecret)

tokenSanityCheck :: Token -> Either Error Token
tokenSanityCheck token =
  case length (getSecret token) of
    64 -> Right token
    n -> Left $ TokenLength n

getToken :: [String] -> Either Error Token
getToken xss =
  case xss of
    [] -> Left NoToken
    (x:_) -> Right $ Token x

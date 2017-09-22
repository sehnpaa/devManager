module Token where

import Types (Token(Token), getSecret)

tokenSanityCheck :: Token -> Either String Token
tokenSanityCheck token =
  case length (getSecret token) of
    64 -> Right token
    n -> Left $ "Token length is " ++ show n ++ "."

getToken :: [String] -> Either String Token
getToken xss =
  case xss of
    [] -> Left "You need to supply at least one argument."
    (x:_) -> Right $ Token x

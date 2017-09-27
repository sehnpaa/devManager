module Token where

import Boundaries
import Types (Error(..), Token(Token), getSecret)

getTokenIO :: (MonadArgs m) => m (Either Error Token)
getTokenIO = do
  args <- getArguments
  return $ getToken args >>= tokenSanityCheck

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

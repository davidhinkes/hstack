module Main where

import Network.Hstack
import Example

main = do
  result <- createClient d (Endpoint "hink.es" 8000) (AddRequest 1 2)
  case result of
    Ok o -> print o
    ClientError e -> print e
    ServerError e -> print e
    ConnectionError e -> print e

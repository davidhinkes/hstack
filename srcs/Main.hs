module Main where

import Data.Serialize
import Network.Hstack
import Snap.Http.Server.Config
import Snap.Http.Server
import Network.Hstack.Example

main = do
  
  result <- createClient d (Endpoint "hink.es" 8000) (AddRequest 1 2)
  case result of
    Ok o -> print o
    ClientError e -> print e
    ServerError e -> print e

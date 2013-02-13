module Main where

import Data.Serialize
import Network.Hstack
import Snap.Http.Server.Config
import Snap.Http.Server
import Network.Hstack.Example (d, h)

main = do
  httpServe defaultConfig (createSnap d defaultParameters h)

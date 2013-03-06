module Main where

import Network.Hstack
import Network.Hstack.Example (d, h)

main = do
  run (registerHandler d defaultParameters h)

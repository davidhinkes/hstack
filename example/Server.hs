module Main where

import Network.Hstack
import Example

h :: Handler IO AddRequest AddResponse
h = do
  i <- getInput
  return $ AddResponse (a i + b i)

main = do
  run (registerHandler d defaultParameters h)

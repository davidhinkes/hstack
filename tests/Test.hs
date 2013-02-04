module Main (main) where

import Network.Hstack

import Data.Maybe
import Test.Framework.Providers.QuickCheck2
import Test.Framework (defaultMain)

test_1 = let
  a :: OutcomeT Maybe Int
  a = do
    c <- return 5
    return (c*2)
  in case (runOutcomeT a) of
    Just (Ok 10) -> True
    _ -> False

tests = [ testProperty "CombineT" test_1]
main = defaultMain tests

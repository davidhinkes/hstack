module Main (main) where

import Network.Hstack
import Network.Hstack.Internal
import Control.Monad.Identity
import Control.Monad.State.Lazy
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

test_2 = let
  a :: Handler Identity Int Int
  a = do
    i <- getInput
    return (i*2)
  in case runIdentity . evalHandler a defaultParameters $ 5 of
    Ok 10 -> True
    _ -> False

parameterTest = let
  a :: Handler Identity Int Int
  a = do
    i <- getInput
    setParameters $ Parameters 69
    return 0
  (action, parameters) = (runState . runHandler $ a) defaultParameters
  in (bodySize parameters) == 69

tests = [ testProperty "CombineT" test_1,
          testProperty "Handler" test_2,
          testProperty "parameterTest" parameterTest ]

main = defaultMain tests

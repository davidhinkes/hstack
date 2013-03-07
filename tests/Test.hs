module Main (main) where

import Network.Hstack
import Network.Hstack.Internal
import Control.Monad.Identity
import Control.Monad.State.Lazy
import qualified Data.ByteString as BS
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
  in case runIdentity . evalAction a $ (Context 5 BS.empty) of
    Ok 10 -> True
    _ -> False
tests = [ testProperty "CombineT" test_1,
          testProperty "Handler" test_2]

main = defaultMain tests

module Example where

import Data.Serialize
import Network.Hstack

data AddRequest = AddRequest {
  a :: Int,
  b :: Int
} deriving Show

instance Serialize AddRequest where
  put = f where
    f r = do
      put $ (a r)
      put $ (b r)
  get = do
    a' <- get
    b' <- get
    return $ AddRequest a' b'

data AddResponse = AddResponse {
  result :: Int
} deriving Show

instance Serialize AddResponse where
  put = f where
    f r = put $ (result r)
  get = do
    result' <- get
    return $ AddResponse result'

d :: ServiceDescriptor AddRequest AddResponse
d = ServiceDescriptor "add"


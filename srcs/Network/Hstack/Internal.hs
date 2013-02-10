module Network.Hstack.Internal where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Int
import Data.Serialize
import Data.String
import qualified Network.HTTP.Base as N
import qualified Network.HTTP as N
import qualified Network.URI as N
import qualified Network.Stream as N
import qualified Snap.Core as S

data (Serialize i, Serialize o) => ServiceDescriptor i o = ServiceDescriptor {
  path :: String
}

data Endpoint = Endpoint {
  host :: String,
  port :: Integer
}

data Context i = Context {
  input :: i
}

data Outcome o = Ok o | ServerError String | ClientError String

instance Monad Outcome where
  a >>= f = case a of
    Ok b -> f b
    ServerError s -> ServerError s
    ClientError s -> ClientError s
  return = Ok

newtype Handler m i o = Handler {
  runHandler :: State Parameters (Action m i o)
}

instance (Monad m) => Monad (Handler m i) where
  -- f :: o -> Handler m i p
  a >>= f = Handler $ do
    r <- runHandler a
    s <- Control.Monad.State.Lazy.get
    -- s :: Parameters
    -- r :: Action m i o
    -- f' :: o -> Action m i p
    let f' o = evalState (runHandler . f $ o) s
    return (r >>= f')
  return x = Handler . return . return $ x

instance (MonadIO m) => MonadIO (Handler m i) where
  liftIO a = Handler $ do
    return . liftIO $ a



newtype Parameters = Parameters {
  bodySize :: Int64
}

instance MonadTrans OutcomeT where
  lift x = OutcomeT $ do
    v <- x
    return . Ok $ v

newtype OutcomeT m a = OutcomeT {
  runOutcomeT :: m (Outcome a)
}

instance Monad m => Monad (OutcomeT m) where
  a >>= f = OutcomeT $ do
    r <- runOutcomeT a
    case r of
      Ok e -> runOutcomeT . f $ e
      ServerError s -> return . ServerError $ s
      ClientError s -> return . ClientError $ s
  return a = lift . return $ a

instance (MonadIO m) => MonadIO (OutcomeT m) where
  liftIO a = OutcomeT $ do
    a' <- liftIO a
    return . Ok $ a'

defaultParameters :: Parameters
defaultParameters = Parameters (1024*1024)

newtype Action m i o = Action {
  runAction :: Reader (Context i) (OutcomeT m o)
}

instance (Monad m) => Monad (Action m i) where
  a >>= f = Action $ do
    r <- runAction a
    c <- ask
    let f' o = (runReader . runAction . f $ o) c
    return $ r >>= f'
  return x = Action . return . return $ x

instance (MonadIO m) => MonadIO (Action m i) where
  liftIO a = Action $ do
    return . liftIO $ a

evalHandler :: Handler m i o -> Parameters -> i -> m (Outcome o)
evalHandler h p i = let
  action = evalState (runHandler h) p
  ot = runReader (runAction action) (Context i)
  in runOutcomeT ot

evalAction :: Action m i o -> i -> m (Outcome o)
evalAction a i = let
  ot = runReader (runAction a) (Context i)
  in runOutcomeT ot

httpResultToOutcome :: N.Result o -> Outcome o
httpResultToOutcome r = case r of
  Left _ -> ClientError "Could not make connection."
  Right a -> Ok a

httpResponseBody :: Show a => N.Response a -> Outcome a
httpResponseBody r = case r of
  N.Response (2,_,_) _ _ bdy -> Ok bdy
  N.Response _ _ _ bdy ->  ClientError $ "Non 200 HTTP response code :" ++ (show bdy)

decode' :: Serialize a => BS.ByteString -> Outcome a
decode' msg = case (decode msg) of
  Left msg -> ClientError $ msg
  Right o -> Ok o

writeOutcome :: (S.MonadSnap m, Serialize o) => Outcome o -> m ()
writeOutcome outcome = case outcome of
  Ok o -> do
    S.modifyResponse (S.setResponseCode 200)
    S.writeBS (encode o)
    S.modifyResponse (S.setContentType . fromString $ "text/base64")
  ClientError msg -> do
    S.modifyResponse (S.setResponseCode 400)
    S.writeBS (fromString msg)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
  ServerError msg -> do
    S.modifyResponse (S.setResponseCode 500)
    S.writeBS (fromString msg)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")

module Network.Hstack.Internal where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Map
import Data.Int
import Data.Serialize
import Data.String
import qualified Network.HTTP.Base as N
import qualified Network.HTTP as N
import qualified Network.URI as N
import qualified Network.Stream as N
import qualified Text.JSON as J
import qualified Snap.Core as S

data Outcome o = Ok o | ServerError String | ClientError String

newtype OutcomeT m a = OutcomeT {
  runOutcomeT :: m (Outcome a)
}

data Context i = Context {
  input :: i
}

newtype Action m i o = Action {
  runAction :: ReaderT (Context i) (OutcomeT m) o
}

type Handler m i o = Action m i o

data (Serialize i, Serialize o) => ServiceDescriptor i o = ServiceDescriptor {
  path :: String
}

data Endpoint = Endpoint {
  host :: String,
  port :: Integer
}

instance Monad Outcome where
  a >>= f = case a of
    Ok b -> f b
    ServerError s -> ServerError s
    ClientError s -> ClientError s
  return = Ok


newtype Parameters = Parameters {
  bodySize :: Int64
}

instance MonadTrans OutcomeT where
  lift x = OutcomeT $ do
    v <- x
    return . Ok $ v

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

instance (Monad m) => Monad (Action m i) where
  a >>= f = Action $ do
    runAction a >>= (runAction . f)
  return x = Action . return $ x

instance (MonadIO m) => MonadIO (Action m i) where
  liftIO a = Action $ do
    liftIO $ a

evalAction :: Action m i o -> i -> m (Outcome o)
evalAction a i = let
  ot = runReaderT (runAction a) (Context i)
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

writeOutcome :: (S.MonadSnap m, Serialize o) => TVar Variables -> Outcome o -> m ()
writeOutcome v outcome = case outcome of
  Ok o -> do
    S.modifyResponse (S.setResponseCode 200)
    S.writeBS (encode o)
    S.modifyResponse (S.setContentType . fromString $ "text/base64")
    liftIO $ atomically $ emitVariable v "_/return-code/200" 1
  ClientError msg -> do
    S.modifyResponse (S.setResponseCode 400)
    S.writeBS (fromString msg)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
    liftIO $ atomically $ emitVariable v "_/return-code/400" 1
  ServerError msg -> do
    S.modifyResponse (S.setResponseCode 500)
    S.writeBS (fromString msg)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
    liftIO $ atomically $ emitVariable v "_/return-code/500" 1

type Variables = Map String Int

data Registry m = Registry {
  runRegistry :: TVar Variables -> m ()
}

emitVariable :: TVar Variables -> String -> Int -> STM ()
emitVariable varsTVar s i = do
  vars <- readTVar varsTVar
  let alterFunction v = case v of
                          Just j -> Just (j + i)
                          Nothing -> Just i
  let vars' = alter alterFunction s vars
  writeTVar varsTVar vars'

variablesHandler :: (S.MonadSnap m) => Registry m
variablesHandler = Registry f where
  f vars = S.path (fromString "_/variables") $ do
    v <- liftIO $ atomically $ readTVar vars
    S.modifyResponse (S.setResponseCode 200)
    S.writeBS (fromString . J.encode $ v)
    S.modifyResponse (S.setContentType . fromString $ "text/json")
     

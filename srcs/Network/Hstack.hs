module Network.Hstack (
  Handler,
  OutcomeT(..),
  Outcome(..),
  defaultParameters,
  getInput,
  evalHandler
) where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Serialize
import Data.String
import qualified Snap.Core as S

data Context i  = Context {
  input :: i
}

data Outcome o = Ok o | ServerError String | ClientError String

newtype OutcomeT m a = OutcomeT {
  runOutcomeT :: m (Outcome a)
}

instance MonadTrans OutcomeT where
  lift x = OutcomeT $ do
    v <- x
    return . Ok $ v

instance Monad m => Monad (OutcomeT m) where
  a >>= f = OutcomeT $ final where
    final = do
      -- r :: Outcome a
      r <- runOutcomeT a
      case r of
        Ok b -> runOutcomeT . f $ b
        ServerError s -> return $ ServerError s
        ClientError s -> return $ ClientError s
  return a = lift . return $ a

instance (MonadIO m) => MonadIO (OutcomeT m) where
  liftIO a = OutcomeT $ do
    a' <- liftIO a
    return . Ok $ a'

newtype Parameters = Parameters {
  bodySize :: Int
}

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

newtype Handler m i o = Handler {
  runHandler :: State Parameters (Action m i o)
}

evalHandler :: Handler m i o -> Parameters -> i -> m (Outcome o)
evalHandler h p i = let
  action = evalState (runHandler h) p
  ot = runReader (runAction action) (Context i)
  in runOutcomeT ot

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

-- Util functions for doing useful things with the Handler Monad.
getInput :: Monad m => Handler m i i
getInput = Handler $ do
  return $ Action (ask >>= (return . return . input))

data (Serialize i, Serialize o) => ServiceDescriptor i o = ServiceDescriptor {
  path :: String
}

data Endpoint = Endpoint {
  host :: String,
  port :: Integer
}

createClient :: ServiceDescriptor i o -> Endpoint -> i -> IO (Outcome o)
createClient sd channel i = return $ ClientError "Not implemented"

decodeInput :: (S.MonadSnap m, Serialize a) => LBS.ByteString -> m a
decodeInput bs = case (decodeLazy bs) of
    Left _ -> S.pass
    Right a -> return a

createSnap :: (S.MonadSnap m, Serialize i, Serialize o) =>
    ServiceDescriptor i o -> Handler IO i o -> m ()
createSnap d h =
  let pathAsBS = fromString . path $ d
      -- Exact match on PUT the path.
      filter = S.method S.PUT . S.path pathAsBS
  in filter $ do
    req <- S.readRequestBody (1024*1024)
    input <- decodeInput req
    let c = Context input
    -- Run the handler.
    outcome <- liftIO $ evalHandler h defaultParameters input
    S.pass

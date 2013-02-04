module Network.Hstack (
  Outcome(..),
  OutcomeT(..),
  Context(..) 
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
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

newtype Handler m i o = Handler {
  runHandler :: Reader (Context i) (OutcomeT m o)
}

instance (Monad m) => Monad (Handler m i) where
  a >>= f = Handler $ do
    -- r :: OutcomeT m o
    r <- runHandler a
    -- c :: Context c
    c <- ask
    return $ r >>= (\o -> (runReader . runHandler . f $ o) c)
  return x = Handler . return . return $ x

instance (MonadIO m) => MonadIO (Handler m i) where
  liftIO ioAction = Handler $ do
    return $ OutcomeT (liftIO ioAction >>= return . Ok)

-- Util functions for doing useful things with the Handler Monad.
getInput :: Monad m => Handler m i i
getInput = Handler $ do
  c <- ask
  return . return . input $ c

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
    outcome <- liftIO $ runOutcomeT (runReader (runHandler h) c)
    S.pass

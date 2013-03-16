module Network.Hstack.Internal where

import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Exception.Base
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Map
import Data.Monoid 
import Data.Int
import Data.Serialize
import Data.String
import qualified Network.HTTP.Base as N
import qualified Network.HTTP as N
import qualified Network.URI as N
import qualified Network.Stream as N
import qualified Text.JSON as J
import qualified Snap.Core as S

data Outcome o = Ok o | ServerError String | ClientError String | ConnectionError String

newtype OutcomeT m a = OutcomeT {
  runOutcomeT :: m (Outcome a)
}

data Context i = Context {
  input :: i,
  remoteAddr :: BS.ByteString
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
} deriving (Eq, Ord)

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
      ConnectionError s -> return . ConnectionError $ s
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

evalAction :: Action m i o -> Context i -> m (Outcome o)
evalAction a c = let
  ot = runReaderT (runAction a) c
  in runOutcomeT ot

httpResultToOutcome :: N.Result o -> OutcomeT Identity o
httpResultToOutcome r = case r of
  Left _ -> OutcomeT . Identity $ ClientError "Could not make connection."
  Right a -> return a

httpResponseBody :: Show a => N.Response a -> OutcomeT Identity a
httpResponseBody r = case r of
  N.Response (2,_,_) _ _ bdy -> return bdy
  N.Response (5,_,_) _ _ bdy -> OutcomeT . Identity $ ClientError $ "5XX HTTP response code :" ++ (show bdy)
  N.Response _ _ _ bdy -> OutcomeT . Identity $ ClientError $ "Non 200 HTTP response code :" ++ (show bdy)

decode' :: Serialize a => BS.ByteString -> OutcomeT Identity a
decode' m = case (decode m) of
  Left msg -> OutcomeT . Identity . ClientError $ msg
  Right o -> return o

writeOutcome :: (S.MonadSnap m, Serialize o) => TVar Variables -> Outcome o -> m ()
writeOutcome v outcome = case outcome of
  Ok o -> do
    S.modifyResponse (S.setResponseCode 200)
    S.writeBS (encode o)
    S.modifyResponse (S.setContentType . fromString $ "text/base64")
    liftIO $ atomically $ emitVariable v "_/return-code/200" 1
  ClientError m -> do
    S.modifyResponse (S.setResponseCode 400)
    S.writeBS (fromString m)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
    liftIO $ atomically $ emitVariable v "_/return-code/400" 1
  ServerError m -> do
    S.modifyResponse (S.setResponseCode 500)
    S.writeBS (fromString m)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
    liftIO $ atomically $ emitVariable v "_/return-code/500" 1
  -- ConnectionError should really not happen, but we include it for
  -- completeness.
  ConnectionError m -> do
    S.modifyResponse (S.setResponseCode 500)
    S.writeBS (fromString m)
    S.modifyResponse (S.setContentType . fromString $ "text/plain")
    liftIO $ atomically $ emitVariable v "_/return-code/500" 1

type Variables = Map String Int

data Registry m = Registry {
  runRegistry :: TVar Variables -> m ()
}

instance S.MonadSnap m => Monoid (Registry m) where
  mempty = Registry (\_ -> S.pass)
  mappend a b = Registry (\v -> let a' = runRegistry a v
                                    b' = runRegistry b v
                                in a' <|> b')

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

-- request' lacks IO, hence is testable.
request' ::
    (Serialize i, Serialize o, Monad m)
    => (N.Request BS.ByteString -> m (N.Result (N.Response BS.ByteString)))
    -> (m (Outcome o) -> (IOException -> m (Outcome o)) -> m (Outcome o))
    -> ServiceDescriptor i o
    -> Endpoint
    -> i
    -> m (Outcome o)
request' simpleHTTP catchIO sd channel i =
  let auth = N.URIAuth "" (host channel) (":" ++ (show . port $ channel))
      uri = N.URI "http:" (Just auth) ("/" ++ path sd) "" ""
      payload = encode i
      contentLength = N.mkHeader N.HdrContentLength (show . BS.length $ payload)
      req = N.Request uri N.PUT [contentLength] payload
      handleIOError _ = return $ ConnectionError "Could not make connection."
      run = do
        res <- simpleHTTP req
        return . runIdentity . runOutcomeT $ do
          resp <- httpResultToOutcome res
          body <- httpResponseBody resp
          decode' body
  in catchIO run handleIOError

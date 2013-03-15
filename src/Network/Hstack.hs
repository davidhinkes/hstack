module Network.Hstack (
  Endpoint(..),
  Handler,
  Outcome(..),
  ServiceDescriptor(..),
  request,
  defaultParameters,
  getInput,
  getRemoteAddr,
  registerHandler,
  registerHandlerWithVariables,
  run
) where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import qualified Data.ByteString as BS
import Data.Map
import Data.Serialize
import Data.String
import Network.Hstack.Internal
import qualified Network.HTTP.Base as N
import qualified Network.HTTP as N
import qualified Snap.Core as S
import qualified Snap.Http.Server as S

-- Util functions for doing useful things with the Handler Monad.
getInput :: Monad m => Handler m i i
getInput = Action $ do
  c <- ask
  return $ input c

getRemoteAddr :: Monad m => Handler m i BS.ByteString
getRemoteAddr = Action $ do
  c <- ask
  return $ remoteAddr c

request ::
    (Serialize i, Serialize o)
    => ServiceDescriptor i o -> Endpoint -> i -> IO (Outcome o)
request = request' N.simpleHTTP N.catchIO

run :: Registry S.Snap -> IO ()
run r = do
  variables <- newTVarIO Data.Map.empty
  let handlers = runRegistry (variablesHandler <> r) variables
  S.httpServe S.defaultConfig handlers

registerHandler ::
    (S.MonadSnap m, Serialize i, Serialize o)
    => ServiceDescriptor i o -> Parameters -> Handler IO i o -> Registry m
registerHandler sd p h = Registry f where
    f v = createSnap' v sd p h

registerHandlerWithVariables ::
    (S.MonadSnap m, Serialize i, Serialize o)
    => ServiceDescriptor i o
    -> Parameters
    -> (TVar Variables -> Handler IO i o)
    -> Registry m
registerHandlerWithVariables sd p f = Registry f' where
  f' v = createSnap' v sd p (f v)

createSnap' ::
    (S.MonadSnap m, Serialize i, Serialize o)
    => TVar Variables
    -> ServiceDescriptor i o
    -> Parameters
    -> Handler IO i o
    -> m ()
createSnap' v d params h =
  let pathAsBS = fromString . path $ d
      -- Exact match on PUT the path.
      blockInvalid = S.method S.PUT . S.path pathAsBS
      action = h
  in blockInvalid $ do
    req <- S.getRequest
    body <- S.readRequestBody . bodySize $ params
    let ip = S.rqRemoteAddr req
    outcome <- case (decodeLazy body) of
      Left _ -> return . ClientError $ "Server could not decode payload"
      Right i -> liftIO $ evalAction action (Context i ip)
    writeOutcome v outcome

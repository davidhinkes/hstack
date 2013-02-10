module Network.Hstack (
  Endpoint(..),
  Handler,
  Outcome(..),
  ServiceDescriptor(..),
  createClient,
  createSnap,
  defaultParameters,
  getInput,
  getParameters,
  setParameters
) where

import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Int
import Data.Serialize
import Data.String
import Network.Hstack.Internal
import qualified Network.HTTP.Base as N
import qualified Network.HTTP as N
import qualified Network.URI as N
import qualified Network.Stream as N
import qualified Snap.Core as S

-- Util functions for doing useful things with the Handler Monad.
getInput :: Monad m => Handler m i i
getInput = Handler $ do
  return $ Action (ask >>= (return . return . input))

getParameters :: Monad m => Handler m i Parameters
getParameters = Handler $ do
  p <- Control.Monad.State.Lazy.get
  return . return $ p

setParameters :: Monad m => Parameters -> Handler m i ()
setParameters p = Handler $ do
  Control.Monad.State.Lazy.put p
  return . return $ ()

createClient :: (Serialize i, Serialize o) =>
  ServiceDescriptor i o -> Endpoint -> i -> IO (Outcome o)
createClient sd channel i =
  let auth = N.URIAuth "" (host channel) (":" ++ (show . port $ channel))
      uri = N.URI "http:" (Just auth) ("/" ++ path sd) "" ""
      payload = encode i
      contentLength = N.mkHeader N.HdrContentLength (show . BS.length $ payload)
      req = N.Request uri N.PUT [contentLength] payload
  in do
    res <-  N.simpleHTTP req
    return $ do
      resp <- httpResultToOutcome res
      body <- httpResponseBody resp
      decode' body

createSnap :: (S.MonadSnap m, Serialize i, Serialize o) =>
    ServiceDescriptor i o -> Handler IO i o -> m ()
createSnap d h =
  let pathAsBS = fromString . path $ d
      -- Exact match on PUT the path.
      blockInvalid = S.method S.PUT . S.path pathAsBS
      (action, params) = runState (runHandler h) defaultParameters
  in blockInvalid $ do
    req <- S.readRequestBody (bodySize params)
    outcome <- case (decodeLazy req) of
      Left _ -> return . ClientError $ "Server could not decode payload"
      Right i -> liftIO $ evalAction action i
    writeOutcome outcome

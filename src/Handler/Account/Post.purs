module Handler.Account.Post (handlePost) where

import Prelude

import App.Env (class Has, Env)
import App.Err (Err, badRequest)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account)
import Db.Account (insertAccount)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index as F.I
import Handler (class Input, runJsonHandler)
import Server (Request, Response)
import SQLite3 (DBConnection)

newtype Params = Params { username :: String }

instance inputParams :: Input Params where
  fromRequest = toParams

toParams :: Request -> Except Err Params
toParams req = do
  username <- readUsername req.body
  pure $ Params { username }

readUsername :: Foreign -> Except Err String
readUsername = withExcept toReadErr <<< read
  where read = F.readString <=< F.I.readProp "username"
        toReadErr = const $ badRequest "Missing required field `username` in the request body."

handlePost :: Env -> Request -> Aff Response
handlePost env = runJsonHandler env 201 handler

handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Params
  -> m Account
handler (Params params) = insertAccount { username: params.username }

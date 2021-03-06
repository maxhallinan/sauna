module Handler.Account.Get (handleGet) where

import Prelude

import App.Env (class Has, Env)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account)
import Data.Newtype (class Newtype, unwrap)
import Db.Account (getAccountByUsername)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index as F.I
import Handler (class Input, runJsonHandler)
import SQLite3 (DBConnection)
import Server (Request, Response)

newtype Params = Params { username :: String }
derive instance newtypeParams :: Newtype Params _

instance inputParams :: Input Params where
  fromRequest = toParams

toParams :: Request -> Except Err Params
toParams req = do
  username <- readUsername req.params
  pure $ Params { username }

readUsername :: Foreign -> Except Err String
readUsername = withExcept toInvalidErr <<< read
  where read = F.readString <=< F.I.readProp "username"
        toInvalidErr = const $ Err.badRequest "Missing `username` path parameter."

handleGet :: Env -> Request -> Aff Response
handleGet env = runJsonHandler env 200 handler

handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Params
  -> m Account
handler =
  unwrap
  >>> _.username
  >>> getAccountByUsername

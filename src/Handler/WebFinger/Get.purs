module Handler.WebFinger.Get (handleGet) where

import Prelude

import App.Env (Env, class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.WebFinger (WebFinger(..))
import Db.Account (getAccountByUsername)
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
  resource <- readResource req.query
  pure $ Params { username: resource }

readResource :: Foreign -> Except Err String
readResource = withExcept toInvalidErr <<< read
  where read = F.readString <=< F.I.readProp "resource"
        toInvalidErr = const $ Err.badRequest "Missing `resource` query parameter."

handleGet :: Env -> Request -> Aff Response
handleGet env = runJsonHandler env 200 handler

{-
  200 OK
  Access-Control-Allow-Origin: *
  Content-Type: application/jrd+json; charset=utf-8
  {
    "subject": "acct:foo@host.com",
    "aliases": [
      "host.com/users/foo"
    ]
    "links": [
      {
        "rel": "self",
        "type": "application/activity+json",
        "href": "foo.com/users/foo"
      }
    ],
  }
-}
handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Params
  -> m WebFinger
handler (Params { username }) = do
  Account account <- getAccountByUsername username
  pure $ WebFinger { subject: account.username
                   , links: []
                   , aliases: []
                   }

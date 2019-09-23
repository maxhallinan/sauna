module Handler.WebFinger.Get (handleGet) where

import Prelude

import App (runApp)
import App.Env (Env, class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.WebFinger (Jrd(..), Link(..), LinkType(..), Uri, parseUri)
import Data.Argonaut (encodeJson)
import Data.Argonaut as A
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Db.Account (getAccountByUsername)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index as F.I
import Handler (toErrResponse)
import Server (Request, Response)
import SQLite3 (DBConnection)

handleGet :: Env -> Request -> Aff Response
handleGet env =
  handler
  >>> runApp env
  >=> either toErrResponse toSuccessResponse
  >>> pure

toSuccessResponse :: Jrd Account -> Response
toSuccessResponse jrd =
  { body: A.stringify $ encodeJson jrd
  , headers: [ Tuple "Access-Control-Allow-Origin" "*"
             , Tuple "Content-Type" "application/jrd+json; charset=utf-8"
             ]
  , status: 200
  }

handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Request
  -> m (Jrd Account)
handler req = do
  { rawUri } <- readParams req
  uri@{ host, name, scheme } <- parseResource rawUri
  if host == req.hostname
    then getAccountJrd uri
    else throwNotFound rawUri

type Params = { rawUri :: String }

readParams
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Request
  -> m Params
readParams req = do
  resourceParam <- readResource req.query
  pure { rawUri: resourceParam }

readResource
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Foreign
  -> m String
readResource =
  read
  >>> runExcept
  >>> either (const err) pure
  where read = F.readString <=< F.I.readProp "resource"
        err = throwBadRequest "Missing `resource` query parameter."

parseResource
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m Uri
parseResource rawUri = 
  either (const $ throwNotFound rawUri) pure (parseUri rawUri)

getAccountJrd
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Uri
  -> m (Jrd Account)
getAccountJrd uri = do
  Account account <- catchError (getAccountByUsername uri.name)
                                (const $ throwNotFound uri.raw)
  pure $ makeAccountJrd uri

makeAccountJrd :: forall a. Uri -> Jrd a
makeAccountJrd { name, host } =
  Jrd { subject: "acct:" <> name <> "@" <> host
      , links: [ Link { href: "https://" <> host <> "/users/" <> name
                      , rel: "self"
                      , type_: ActivityJson
                      }
               ]
      , aliases: [ "https://" <> host <> "/@" <> name ]
      }

throwNotFound
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m a
throwNotFound rawUri = throwError $ Err.notFound msg
  where msg = "Unknown resource " <> rawUri

throwBadRequest
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m a
throwBadRequest = throwError <<< Err.badRequest

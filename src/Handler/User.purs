module Handler.User (handleGet) where

import Prelude

import App (runApp)
import App.Env (Env, class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (runExcept, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.ActivityPub (Person(..))
import Crypto (unPublicKey)
import Data.Argonaut as A
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Db.Account (getAccountByUsername)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign (F, Foreign)
import Foreign as F
import Foreign.Index as F.I
import Handler (toErrResponse)
import Server (Request, Response)
import SQLite3 (DBConnection)

handleGet :: Env -> Request -> Aff Response
handleGet env =
  handler
  >>> runApp env
  >=> either toErrResponse identity
  >>> pure

handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Request
  -> m Response
handler req = do
  { username } <- readParams req
  case contentType of
    Just ActivityJson ->
      handleGetActor req.hostname username
    Just LdJson ->
      handleGetActor req.hostname username
    Just Html ->
      handleGetUser username
    Just Wildcard ->
      handleGetUser username
    Nothing ->
      throwNotFound username
  where contentType = accepts [ ActivityJson, Html, LdJson, Wildcard ]
        accepts = toContentType <=< req.accepts <<< map fromContentType

type Params = { username :: String }

readParams
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Request
  -> m Params
readParams { params } =
  map { username: _ } (readUsername params)
  # runExcept
  # either throwBadRequest pure
  where throwBadRequest _ = throwError (Err.badRequest "")

readUsername :: Foreign -> F String
readUsername = errorsAt "username" <<< F.readString <=< F.I.readProp "username"

errorsAt :: forall a. String -> F a -> F a
errorsAt prop = withExcept $ map (F.I.errorAt prop)

data ContentType = ActivityJson | Html | LdJson | Wildcard

toContentType :: String -> Maybe ContentType
toContentType = case _ of
  "*/*" ->
    Just Wildcard
  "application/activity+json" ->
    Just ActivityJson
  "application/ld+json" ->
    Just LdJson
  "text/html" ->
    Just Html
  _ ->
    Nothing

fromContentType :: ContentType -> String
fromContentType = case _ of
  ActivityJson ->
    "application/activity+json"
  LdJson ->
    "application/ld+json"
  Html ->
    "text/html"
  Wildcard ->
    "*/*"

handleGetUser
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => String
  -> m Response
handleGetUser username = do
  account <- catchError (getAccountByUsername username)
                        (const $ throwNotFound username)
  pure $ makeHtmlResponse account

makeHtmlResponse :: Account -> Response
makeHtmlResponse (Account { username }) =
  { body: "<h1>user:" <> username <> "</h1>"
  , headers: [ Tuple "Content-Type" "text/html" ]
  , status: 200
  }

handleGetActor
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => String
  -> String
  -> m Response
handleGetActor hostname username = do
  account <- getAccountByUsername username
  pure $ makeActorResponse hostname ActivityJson account

makeActorResponse :: String -> ContentType -> Account -> Response
makeActorResponse hostname contentType account@(Account { username }) =
  { body: toActorJson account
  , headers: [ Tuple "Content-Type" (fromContentType contentType) ]
  , status: 200
  }
  where toActorJson = A.stringify <<< A.encodeJson <<< actorFromAccount hostname

actorFromAccount :: String -> Account -> Person
actorFromAccount hostname (Account { pubKey, username }) = Person
  { context: [ "https://www.w3.org/ns/activitystreams", "https://w3id.org/security/v1" ]
  , id: actorId
  , inbox: ""
  , name: username
  , outbox: ""
  , publicKey: { id: actorId <> "#main-key"
               , owner: actorId
               , publicKeyPem: unPublicKey pubKey
               }
  }
  where actorId = "https://" <> hostname <> "/users/" <> username

throwNotFound
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m a
throwNotFound username = throwError $ Err.notFound msg
  where msg = "Unknown user " <> username

module Handler.User.Inbox (handlePost) where

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
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
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

handlePost :: Env -> Request -> Aff Response
handlePost env =
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
  { contentType, username } <- readParams req
  case contentType of
    Just ActivityJson ->
      handleActivityPost username
    Just LdJson ->
      handleActivityPost username
    Nothing ->
      throwUnsupportedMedia

type Params = { contentType :: Maybe ContentType, username :: String }

readParams
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Request
  -> m Params
readParams { headers, params } =
  { contentType: _, username: _ }
  <$> (readContentType headers)
  <*> (readUsername params)
  # runExcept
  # either throwBadRequest pure
  where throwBadRequest _ = throwError (Err.badRequest "")

data ContentType = ActivityJson | LdJson

toContentType :: String -> Maybe ContentType
toContentType "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"" = Just LdJson
toContentType s = 
  split (Pattern ";") s
  # Array.head 
  >>= case _ of
        "application/activity+json" -> 
          Just ActivityJson
        _ -> 
          Nothing

readContentType :: Foreign -> F (Maybe ContentType)
readContentType = errorsAt "content-type" <<< map toContentType <<< F.readString <=< F.I.readProp "content-type"

readUsername :: Foreign -> F String
readUsername = errorsAt "username" <<< F.readString <=< F.I.readProp "username"

errorsAt :: forall a. String -> F a -> F a
errorsAt prop = withExcept $ map (F.I.errorAt prop)

handleActivityPost
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => String
  -> m Response
handleActivityPost _ =
  pure { body: ""
       , headers: []
       , status: 201
       }

throwUnsupportedMedia
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => m a
throwUnsupportedMedia = throwError $ Err.unsupportedMedia ""

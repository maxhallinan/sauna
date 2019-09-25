module Handler.User.Inbox (handlePost) where

import Prelude

import App (runApp)
import App.Env (Env, class Has)
import App.Err (Err)
import App.Err as Err
import ContentType as ContentType
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.ActivityPub (Activity(..), ActivityType, toActivityType)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Db.Account (getAccountByUsername)
import Db.Activity (insertAccountActivity, insertActivity)
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
  { contentType, msg, username } <- readParams req
  case contentType of
    Just ActivityJson ->
      handleActivityPost username msg
    Just LdJson ->
      handleActivityPost username msg
    Nothing ->
      throwUnsupportedMedia

type Params =
  { contentType :: Maybe ContentType
  , msg :: Msg
  , username :: String
  }

type Msg =
  { activityId :: String
  , activityType :: ActivityType
  }

readParams
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Request
  -> m Params
readParams { body, headers, params } =
  { contentType: _, msg:_, username: _ }
  <$> (readContentType headers)
  <*> (readMsg body)
  <*> (readUsername params)
  # runExcept
  # either throwBadRequest pure
  where throwBadRequest errs = throwError (Err.badRequest $ show (map show errs))

data ContentType = ActivityJson | LdJson

toContentType :: String -> Maybe ContentType
toContentType =
  ContentType.parse
  >>> either (const Nothing) toSupportedType
  where
    toSupportedType { mimeType, parameters } =
      case mimeType of
        "application/activity+json" ->
          Just ActivityJson
        "application/ld+json" ->
          -- Mime-type of `application/ld+json` must have the Activity Streams profile.
          either (const Nothing)
                 ldJsonFromProfile
                 (readProfile parameters)
        _ ->
          Nothing
    readProfile = runExcept <<< (F.readString <=< F.I.readProp "profile")
    ldJsonFromProfile "https://www.w3.org/ns/activitystreams" = Just LdJson
    ldJsonFromProfile _ = Nothing

readMsg :: Foreign -> F Msg
readMsg f =
  { activityId: _, activityType:_ }
  <$> readActivityId f
  <*> readActivityType f

readActivityId :: Foreign -> F String
readActivityId =
  errorsAt "id"
  <<< F.readString
  <=< F.I.readProp "id"

readActivityType :: Foreign -> F ActivityType
readActivityType =
  errorsAt "type"
  <<< map toActivityType
  <<< F.readString
  <=< F.I.readProp "type"

readContentType :: Foreign -> F (Maybe ContentType)
readContentType =
  errorsAt "content-type"
  <<< map toContentType
  <<< F.readString
  <=< F.I.readProp "content-type"

readUsername :: Foreign -> F String
readUsername =
  errorsAt "username"
  <<< F.readString
  <=< F.I.readProp "username"

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
  -> Msg
  -> m Response
handleActivityPost username msg = do
  (Account account) <- getAccountByUsername username
  (Activity activity) <- insertActivity { activityId: msg.activityId
                                        , activityType: msg.activityType
                                        }
  insertAccountActivity { accountId: account.id
                        , activityId: activity.id
                        }
  pure $ makeResponse

makeResponse :: Response
makeResponse =
  { body: ""
  , headers: []
  , status: 201
  }

throwUnsupportedMedia
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => m a
throwUnsupportedMedia = throwError $ Err.unsupportedMedia ""
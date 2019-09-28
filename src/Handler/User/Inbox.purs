module Handler.User.Inbox (formatDateHeader, handlePost) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestHeader as RequestHeader
import App (runApp)
import App.Env (Env, class Has)
import App.Err (Err)
import App.Err as Err
import ContentType as ContentType
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.ActivityPub (Activity(..), ActivityType(..), toActivityType)
import Crypto (PublicKey)
import Crypto as Crypto
import Data.Argonaut (class DecodeJson, Json)
import Data.Argonaut as J
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.Nullable as Nullable
import Data.Options (Options, (:=))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Db.Account (getAccountByUsername, getAccountPrivKey)
import Db.Activity (insertAccountActivity, insertActivity)
import Db.Actor (getActorByUri, insertActor)
import Db.Following (insertFollowing)
import Db.Follower (deleteFollower, insertFollower)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Foreign (F, Foreign)
import Foreign as F
import Foreign.Index as F.I
import Foreign.Object (Object)
import Foreign.Object as O
import Global.Unsafe (unsafeStringify)
import Handler (toErrResponse)
import HttpSignature (SignatureParams)
import HttpSignature as HS
import Node.HTTP.Client as HTTP
import Node.URL as URL
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
  checkAuthorization req
  { contentType, msg, username } <- readParams req
  case contentType of
    Just ActivityJson ->
      handleActivityPost { msg, request: req, username }
    Just LdJson ->
      handleActivityPost { msg, request: req, username }
    Nothing ->
      throwUnsupportedMedia

checkAuthorization
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Request
  -> m Unit
checkAuthorization req = do
  authHeader <- readAuthHeader req.headers
  params <- parseHttpSignature authHeader
  comparisonString <- makeComparisonString params.headers req
  publicKey <- fetchPublicKey params.keyId
  verifySignature { comparisonString, publicKey, signature: params.signature }

verifySignature
  :: forall m
   . MonadThrow Err m
  => { comparisonString :: String, publicKey :: PublicKey, signature :: String }
  -> m Unit
verifySignature { comparisonString, publicKey, signature } =
  if Crypto.rsaVerify publicKey signature comparisonString
  then pure unit
  else throwError $ Err.unauthorized "Not authorized."

makeComparisonString
  :: forall m
   . MonadThrow Err m
  => Maybe (Array String)
  -> Request
  -> m String
makeComparisonString headersInSignature req =
  case headersInSignature of
    Nothing ->
      let
        dateHeader = map pure $ readReqHeader req.headers "date"
      in
      either dateErr makeString (runExcept dateHeader)
    Just inSignature ->
      let
        pairs = traverse (readReqHeader req.headers) inSignature
      in
      either headersErr makeString (runExcept pairs)
  where dateErr _ = throwError $ Err.unauthorized "The request is missing a Date header."
        headersErr _ = throwError $ Err.unauthorized "The request is missing headers specified in the signature's header list."
        reqPieces = { reqMethod: req.method, reqUrl: req.originalUrl }
        makeString = pure <<< HS.makeStringToSign reqPieces

readReqHeader :: Foreign -> String -> F { k :: String, v :: String }
readReqHeader f k = map makePair headerVal
  where headerVal = errorsAt k $ F.I.readProp k f >>= F.readString
        makePair v = { k, v }

fetchPublicKey
  :: forall m
   . MonadThrow Err m
  => MonadAff m
  => String
  -> m PublicKey
fetchPublicKey url = do
  resBody <- getActorJson
  case resBody of
    Left _ ->
      authErr
    Right json ->
      either (const authErr) (pure <<< Crypto.makePublicKey) (decodePubKeyPem json)
  where getActorJson = liftAff $ map _.body $ AX.request requestConfig
        requestConfig = AX.defaultRequest { headers = Array.cons acceptHeader AX.defaultRequest.headers
                                          , responseFormat = ResponseFormat.json
                                          , url = url
                                          }
        acceptHeader = RequestHeader.Accept (MediaType "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
        authErr = throwError $ Err.unauthorized $ "Unable to retrieve key from keyId: " <> url
        decodePubKeyPem = J.decodeJson >=> getField "publicKey" >=> getField "publicKeyPem" >=> J.decodeJson

getField :: forall a. DecodeJson a => String -> Object Json -> Either String a
getField = flip J.getField

parseHttpSignature
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m SignatureParams
parseHttpSignature =
  HS.parseSignatureParams
  >>> either authErr pure
  where authErr err = throwUnauthorized "Could not parse Authorization header."

readAuthHeader
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => Foreign
  -> m String
readAuthHeader =
  readHeader
  >>> runExcept
  >>> either (const authErr) pure
  where readHeader = errorsAt "authorization" <<< F.readString <=< F.I.readProp "authorization"
        authErr = throwUnauthorized "Missing Authorization header."

throwUnauthorized
  :: forall m a
   . MonadError Err m
  => MonadThrow Err m
  => String
  -> m a
throwUnauthorized = throwError <<< Err.unauthorized

type Params =
  { contentType :: Maybe ContentType
  , msg :: Msg
  , username :: String
  }

type Msg =
  { activityBlob :: String
  , activityId :: String
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
  { activityBlob:_, activityId:_, activityType:_ }
  <$> readActivityBlob f
  <*> readActivityId f
  <*> readActivityType f

readActivityBlob :: Foreign -> F String
readActivityBlob = pure <<< unsafeStringify

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
  => { msg :: Msg, request :: Request, username :: String }
  -> m Response
handleActivityPost { msg, request, username } = do
  (Account account) <- getAccountByUsername username
  (Activity activity) <- insertActivity { activityBlob: msg.activityBlob
                                        , activityId: msg.activityId
                                        , activityType: msg.activityType
                                        }
  insertAccountActivity { accountId: account.id
                        , activityId: activity.id
                        }
  handleActivityType request (Account account) (Activity activity)
  pure $ makeResponse

handleActivityType
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Request
  -> Account
  -> Activity
  -> m Unit
handleActivityType req account activity@(Activity { activityType }) =
  case activityType of
    Accept ->
      handleAccept account activity
    Follow ->
      handleFollow req account activity
    Remove ->
      handleRemove account activity
    _ ->
      pure unit

handleAccept
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Account
  -> Activity
  -> m Unit
handleAccept (Account account) (Activity activity) =
  case decodeActorUri activity.activityBlob of
    Left _ ->
      pure unit
    Right uri -> do
      actor <- insertActor { uri }
      insertFollowing { accountId: account.id, actorId: actor.id }

handleFollow
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Request
  -> Account
  -> Activity
  -> m Unit
handleFollow req (Account account) (Activity activity) =
  case decodeActorUri activity.activityBlob of
    Left _ ->
      pure unit
    Right uri -> do
      actor <- insertActor { uri }
      insertFollower { accountId: account.id, actorId: actor.id }
      sendAcceptFollow req (Account account) actor (Activity activity)

sendAcceptFollow
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Request
  -> Account
  -> { id :: Int, uri :: String }
  -> Activity
  -> m Unit
sendAcceptFollow req (Account account) actor follow = do
  privateKey <- getAccountPrivKey { id: account.id }
  dateHeader <- liftEffect $ map formatDateHeader nowDateTime
  let stringToSign = HS.makeStringToSign { reqMethod: req.method
                                         , reqUrl: req.originalUrl
                                         } 
                                         [ {k: "(request-target)", v: requestTarget}
                                         , {k: "Date", v: dateHeader}
                                         ]
  let signature = Crypto.rsaSign privateKey stringToSign
  let authHeader = "Signature keyId=\"" <> accountKeyUrl <> "\",algorithm=\"rsa-sha256\",headers=\"(request-target) date\",signature=\"" <> signature <> "\""
  let headers = makeRequestHeaders authHeader dateHeader
  let requestOptions = (makeRequestOptions headers) <$> (Nullable.toMaybe parsedActorUri.hostname) <*> (Nullable.toMaybe parsedActorUri.path)
  case requestOptions of
    Just opts -> do
      _ <- liftAff $ sendRequest opts
      pure unit
    Nothing ->
      pure unit
  where acceptFollow = makeAcceptFollow req.hostname (Account account) follow
        parsedActorUri = URL.parse actor.uri
        getActorPath = maybe "/" identity <<< Nullable.toMaybe <<< _.path
        actorUriPath = getActorPath parsedActorUri
        requestTarget = "post " <> actorUriPath
        accountKeyUrl = "https://" <> req.hostname <> "/users/" <> account.username <> "#main-key"
        makeRequestHeaders authHeader dateHeader = HTTP.RequestHeaders $ O.fromFoldable [ Tuple "Authorization" authHeader
                                                                                        , Tuple "Content-Type" "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""
                                                                                        , Tuple "Date"  dateHeader
                                                                                        ]
        makeRequestOptions headers hostname path =
          HTTP.method := "POST"
          <> HTTP.protocol := "https:"
          <> HTTP.hostname := hostname
          <> HTTP.path := path
          <> HTTP.headers := headers

sendRequest :: Options HTTP.RequestOptions -> Aff HTTP.Response
sendRequest options = makeAff \cb -> do
  _ <- HTTP.request options (void <<< cb <<< Right)
  pure mempty

formatDateHeader :: DateTime -> String
formatDateHeader = Formatter.format $ List.fromFoldable
  [ Formatter.DayOfWeekNameShort
  , Formatter.Placeholder ", "
  , Formatter.DayOfMonthTwoDigits
  , Formatter.Placeholder " "
  , Formatter.MonthShort
  , Formatter.Placeholder " "
  , Formatter.YearFull
  , Formatter.Placeholder " "
  , Formatter.Hours24
  , Formatter.Placeholder ":"
  , Formatter.MinutesTwoDigits
  , Formatter.Placeholder ":"
  , Formatter.SecondsTwoDigits
  , Formatter.Placeholder " GMT"
  ]

makeAcceptFollow :: String -> Account -> Activity -> Json
makeAcceptFollow hostname (Account account) (Activity activity) = J.fromObject $ O.fromFoldable
  [ Tuple "@context" $ J.fromString "https://www.w3.org/ns/activitystreams"
  , Tuple "actor" $ J.fromString $ "https://" <> hostname <> "/users/" <> account.username
  , Tuple "type" $ J.fromString "Accept"
  , Tuple "object" $ J.fromString activity.activityBlob
  ]

handleRemove
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadError Err m
  => MonadThrow Err m
  => MonadAff m
  => Account
  -> Activity
  -> m Unit
handleRemove (Account account) (Activity activity) =
  case decodeActorUri activity.activityBlob of
    Left _ ->
      pure unit
    Right uri -> do
      actor <- getActorByUri uri
      deleteFollower { accountId: account.id, actorId: actor.id }

decodeActorUri :: String -> Either String String
decodeActorUri = J.jsonParser >=> J.decodeJson >=> getField "actor" >=> decodeActorField
  where decodeActorField :: Json -> Either String String
        decodeActorField =
          J.caseJson
            toLeft -- unit
            toLeft -- boolean
            toLeft -- number
            Right  -- string
            toLeft -- array
            (getField "id" >=> J.decodeJson) -- object

        toLeft :: forall a. a -> Either String String
        toLeft _ = Left "Actor field was not a String or an Object."

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

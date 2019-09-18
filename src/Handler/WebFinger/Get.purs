module Handler.WebFinger.Get (handler) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import SQLite3 (DBConnection)
import App (Env, badRequest)
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Core.WebFinger (WebFinger(..), toJsonString)
import Data.Either (Either, either)
import Db.Account (getAccountByUsername)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign as F
import Foreign.Index as F.I
import Handler (class Input, class Output, runHandler)
import Server (Request, Response)

newtype In = In (Either F.MultipleErrors Params)

type Params = { username :: String }

instance inputIn :: Input In where
  fromRequest = requestToIn

requestToIn :: Request -> In
requestToIn = 
  _.query
  >>> decodeResource
  >>> map toParams
  >>> In
  where decodeResource = runExcept <<< (F.readString <=< F.I.readProp "resource")
        toParams resource = { username: resource }

newtype Out = Out WebFinger

instance outputOut :: Output Out where
  toResponse = outToResponse

outToResponse :: Out -> Response
outToResponse (Out webFinger) =
  { body: toJsonString webFinger
  , headers: []
  , status: 200
  }

handler :: Env -> Request -> Aff Response
handler env = runHandler env $
  unwrapIn
  >>> either handleBadReq handleGoodReq
  >>> wrapOut
  where unwrapIn (In i) = i
        wrapOut = map Out

handleBadReq 
  :: forall m
   . MonadError Err m
  => MonadThrow Err m
  => F.MultipleErrors 
  -> m WebFinger
handleBadReq _ = throwError (badRequest "Missing `resource` query parameter.")

handleGoodReq
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Params
  -> m WebFinger
handleGoodReq { username } = do 
  Account account <- getAccountByUsername username
  pure $ WebFinger { subject: account.username }

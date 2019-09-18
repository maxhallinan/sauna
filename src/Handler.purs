module Handler
  ( class Input
  , Handler
  , fromRequest
  , runJsonHandler
  ) where

import Prelude

import App (App, Env, Err(..), ErrName(..), runApp)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Server (Request, Response)

type Handler = forall i o. Input i => EncodeJson o => i -> App o

class Input input where
  fromRequest :: Request -> Except Err input

runJsonHandler
  :: forall i o
   . Input i
  => EncodeJson o
  => Env
  -> Int
  -> (i -> App o)
  -> (Request -> Aff Response)
runJsonHandler env successStatus handleReq =
  fromRequest
  >>> runExcept
  >>> either throwError handleReq
  >>> runApp env
  >=> either toErrResponse (toJsonResponse successStatus)
  >>> pure

toErrResponse :: Err -> Response
toErrResponse (Err { msg, name }) =
  { body: maybe "" identity msg
  , headers: []
  , status: toErrStatus name
  }

toErrStatus :: ErrName -> Int
toErrStatus BadRequest = 400
toErrStatus NotFound = 404
toErrStatus Conflict = 409
toErrStatus Unknown = 500
toErrStatus DbErr = 500

toJsonResponse :: forall a. EncodeJson a => Int -> a -> Response
toJsonResponse status output =
  { body : A.stringify $ encodeJson output
  , headers: [Tuple "Content-Type" "application/json"]
  , status
  }

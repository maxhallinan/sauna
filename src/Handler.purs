module Handler
  ( class Input
  , Handler
  , fromRequest
  , runJsonHandler
  , toErrResponse
  ) where

import Prelude

import App (App, runApp)
import App.Env (Env)
import App.Err (Err(..), ErrCode(..))
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Either (either)
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
toErrResponse err =
  { body: A.stringify $ encodeJson err
  , headers: [corsHeader, jsonHeader]
  , status: toErrStatus err
  }

toErrStatus :: Err -> Int
toErrStatus (Err { code }) =
  case code of
       BadRequest -> 400
       NotFound -> 404
       Conflict -> 409
       UnsupportedMedia -> 415
       (InvalidData _) -> 422
       DbErr -> 500
       Unknown -> 500

toJsonResponse :: forall a. EncodeJson a => Int -> a -> Response
toJsonResponse status output =
  { body : A.stringify $ encodeJson output
  , headers: [corsHeader, jsonHeader]
  , status
  }

jsonHeader :: Tuple String String
jsonHeader = Tuple "Content-Type" "application/json"

corsHeader :: Tuple String String
corsHeader = Tuple "Access-Control-Allow-Origin" "*"

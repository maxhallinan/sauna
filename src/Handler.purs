module Handler 
  ( class Input
  , class Output
  , Handler
  , fromRequest
  , runHandler
  , toResponse
  ) where

import Prelude

import App (App, Env, Err(..), ErrName(..), runApp)
import Data.Either (either)
import Data.Maybe (maybe)
import Effect.Aff (Aff)
import Server (Request, Response)

type Handler = forall i o. Input i => Output o => i -> App o

class Input input where
  fromRequest :: Request -> input

class Output output where
  toResponse :: output -> Response

runHandler 
  :: forall i o
   . Input i 
  => Output o 
  => Env 
  -> (i -> App o) 
  -> (Request -> Aff Response)
runHandler env handler = 
  fromRequest 
  >>> handler 
  >>> runApp env 
  >=> either toErrResponse toResponse 
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
toErrStatus DbErr = 500

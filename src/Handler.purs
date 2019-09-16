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
import Data.Either (Either(..))
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
runHandler env handler req = do
  r <- runApp env app
  case r of
    Left err -> 
      pure $ toErrResponse err
    Right output -> 
      pure $ toResponse output
  where input = fromRequest req
        app = handler input

toErrResponse :: Err -> Response
toErrResponse (Err { msg, name }) =
  { body: maybe "" identity msg
  , headers: []
  , status: toErrStatus name
  }

toErrStatus :: ErrName -> Int
toErrStatus BadRequest = 400
toErrStatus NotFound = 404

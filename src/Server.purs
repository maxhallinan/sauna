module Server
  ( module Server.Router
  , module Server.Core
  , makeJsonMiddleware
  , runServer
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Makkori (listen, makeApp)
import Makkori as M
import Makkori.Extra (useRouter)
import Makkori.Extra as M.E
import Server.Core (Handler, Method(..), Middleware, Path, Port(..), Request, Response, Router, path, port)
import Server.Router (makeRouter, registerRoute, useSubRouter)

makeJsonMiddleware :: Array String -> Effect Middleware
makeJsonMiddleware = M.E.makeJsonMiddleware

runServer :: Port -> Router -> Aff Unit
runServer (Port port) router = liftEffect $ do
  app <- makeApp
  _ <- useRouter (path "/") router app
  void $ listen (M.Port port) (pure unit) app

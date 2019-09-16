module Server 
  ( module Server.Router 
  , module Server.Core
  , runServer
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Makkori (listen, makeApp, makeJSONMiddleware, use)
import Makkori.Extra (useRouter)
import Server.Core (Handler, Method(..), Path, Port, Request, Response, Router, path, port)
import Server.Router (makeRouter, registerRoute, useSubRouter)

runServer :: Port -> Router -> Aff Unit
runServer port router = liftEffect $ do
  app <- makeApp
  json <- makeJSONMiddleware {}
  _ <- use (path "/") json app
  _ <- useRouter (path "/") router app
  void $ listen port (pure unit) app

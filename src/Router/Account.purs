module Router.Account (makeRouter) where

import Prelude

import App.Env (Env)
import Effect (Effect)
import Handler.Account (handleGet, handlePost)
import Server (Method(..), Router, path, registerRoute)
import Server as S
import Server.Router (useRouterMiddleware)

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  json <- S.makeJsonMiddleware [ "application/json" ]
  useRouterMiddleware (path "/") json router
  _ <- registerRoute Post (path "/") (handlePost env) router
  _ <- registerRoute Get (path "/:username") (handleGet env) router
  pure router

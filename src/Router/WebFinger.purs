module Router.WebFinger (makeRouter) where

import Prelude

import App.Env (Env)
import Effect (Effect)
import Handler.WebFinger (handleGet)
import Server (Method(..), Router, path, registerRoute)
import Server as S

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  _ <- registerRoute Get (path "/") (handleGet env) router
  pure router

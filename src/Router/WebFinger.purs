module Router.WebFinger (makeRouter) where

import Prelude

import App (Env)
import Effect (Effect)
import Handler.WebFinger as WebFinger
import Server (Method(..), Router, path, registerRoute)
import Server as S

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  _ <- registerRoute Get (path "/") (WebFinger.handleGet env) router
  pure router

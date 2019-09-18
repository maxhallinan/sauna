module Router.Account (makeRouter) where

import Prelude

import App (Env)
import Effect (Effect)
import Handler.Account (handleGet, handlePost)
import Server (Method(..), Router, path, registerRoute)
import Server as S

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  _ <- registerRoute Post (path "/") (handlePost env) router
  _ <- registerRoute Get (path "/:username") (handleGet env) router
  pure router

module Router (makeRouter) where

import Prelude

import App (Env)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Server (Router, path, useSubRouter)
import Server as S
import Router.WebFinger as WebFinger

makeRouter :: Env -> Aff Router
makeRouter env = liftEffect do
  rootRouter <- S.makeRouter
  webFingerRouter <- WebFinger.makeRouter env
  useSubRouter (path "/.well-known/webfinger") rootRouter webFingerRouter
  pure rootRouter

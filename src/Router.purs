module Router (makeRouter) where

import Prelude

import App (Env)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Server (Router)
import Server as S
import Router.HelloWorld as HelloWorld

makeRouter :: Env -> Aff Router
makeRouter env = liftEffect do
  router <- S.makeRouter
  helloWorldRouter <- HelloWorld.makeRouter env
  S.useSubRouter "/hello-world" helloWorldRouter router
  pure router

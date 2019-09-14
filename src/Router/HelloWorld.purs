module Router.HelloWorld (makeRouter) where

import Prelude

import App (Env)
import Effect (Effect)
import Handler.HelloWorld as HelloWorld
import Server (Method(..), Router)
import Server as S

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  _ <- S.registerRoute Get "/" (HelloWorld.getHelloWorld env) router
  pure router

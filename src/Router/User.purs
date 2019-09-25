module Router.User (makeRouter) where

import Prelude

import App.Env (Env)
import Effect (Effect)
import Handler.User.Get (handleGet)
import Handler.User.Inbox as Inbox
import Server (Method(..), Router, path, registerRoute)
import Server as S

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  _ <- registerRoute Post (path "/:username/inbox") (Inbox.handlePost env) router
  _ <- registerRoute Get (path "/:username") (handleGet env) router
  pure router

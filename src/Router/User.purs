module Router.User (makeRouter) where

import Prelude

import App.Env (Env)
import Effect (Effect)
import Handler.User.Get (handleGet)
import Handler.User.Inbox as Inbox
import Server (Method(..), Router, path, registerRoute)
import Server as S
import Server.Router (useRouterMiddleware)

makeRouter :: Env -> Effect Router
makeRouter env = do
  router <- S.makeRouter
  activityJson <- S.makeJsonMiddleware [ "application/ld+json", "application/activity+json" ]
  useRouterMiddleware (path "/") activityJson router
  _ <- registerRoute Post (path "/:username/inbox") (Inbox.handlePost env) router
  _ <- registerRoute Get (path "/:username") (handleGet env) router
  pure router

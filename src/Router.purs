module Router (makeRouter) where

import Prelude

import App.Env (Env)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Server (Router, path, useSubRouter)
import Server as S
import Router.Account as Account
import Router.User as User
import Router.WebFinger as WebFinger

makeRouter :: Env -> Aff Router
makeRouter env = liftEffect do
  rootRouter <- S.makeRouter
  accountRouter <- Account.makeRouter env
  userRouter <- User.makeRouter env
  webFingerRouter <- WebFinger.makeRouter env
  useSubRouter (path "/api/v1/accounts") rootRouter accountRouter
  useSubRouter (path "/.well-known/webfinger") rootRouter webFingerRouter
  useSubRouter (path "/users") rootRouter userRouter
  pure rootRouter

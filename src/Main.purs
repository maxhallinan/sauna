module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Makkori (App, Middleware, Request, Response)
import Makkori as M
import Node.HTTP (Server)
import Route.Account as Route.Account

main :: Effect Unit
main = do
  app <- M.makeApp
  server <- startServer 3000 app
  json <- jsonMiddleware
  M.use (M.Path "/") json app
  post "/api/v1/accounts" Route.Account.handlePost app
  get "/api/v1/accounts/:id" Route.Account.handleGet app

startServer :: Int -> App -> Effect Server
startServer port app = M.listen (M.Port port) cb app
  where cb = log $ "Listening on 0.0.0.0:" <> show port

jsonMiddleware :: Effect Middleware
jsonMiddleware = M.makeJSONMiddleware {}

get :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
get path handler app = M.get (M.Path path) (M.makeHandler handler) app

post :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
post path handler app = M.post (M.Path path) (M.makeHandler handler) app

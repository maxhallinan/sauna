module Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, catchError)
import Effect.Console (log)
import Effect.Exception (catchException)
import Foreign as F
import Makkori (App, Middleware, Request, Response)
import Makkori as M
import Node.HTTP (Server)
import Route.Account as Route.Account
import Route.WebFinger as Route.WebFinger
import SQLite3 (newDB, queryDB)

dropTable = """
DROP TABLE accounts;
"""

createTable = """
CREATE TABLE IF NOT EXISTS accounts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE
);
"""

insertAccount = """
INSERT INTO accounts (username) VALUES (?)
"""

main :: Effect Unit
main = launchAff_ (catchError foo (\e -> do
  _ <- liftEffect (log $ show e)
  pure unit))

foo :: Aff Unit
foo = do
  db <- newDB "./data"
  _ <- queryDB db createTable []
  liftEffect $ do
    app <- M.makeApp
    server <- startServer 3000 app
    json <- jsonMiddleware
    M.use (M.Path "/") json app
    get "/.well-known/webfinger" (Route.WebFinger.handleGet db) app
    post "/api/v1/accounts" (Route.Account.handlePost db) app
    get "/api/v1/accounts/:name" (Route.Account.handleGet db) app

startServer :: Int -> App -> Effect Server
startServer port app = M.listen (M.Port port) cb app
  where cb = log $ "Listening on 0.0.0.0:" <> show port

jsonMiddleware :: Effect Middleware
jsonMiddleware = M.makeJSONMiddleware {}

get :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
get path handler app = M.get (M.Path path) (M.makeHandler handler) app

post :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
post path handler app = M.post (M.Path path) (M.makeHandler handler) app

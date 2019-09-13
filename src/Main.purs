module Main where

import Prelude

import App (AppEnv)
import Config (Config, loadConfig)
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

runServer :: AppEnv -> Aff Unit
runServer env = do
  _ <- queryDB env.dbConn createTable []
  liftEffect $ do
    app <- M.makeApp
    server <- startServer env.port app
    json <- jsonMiddleware
    M.use (M.Path "/") json app
    get "/.well-known/webfinger" (Route.WebFinger.handleGet env.dbConn) app
    post "/api/v1/accounts" (Route.Account.handlePost env.dbConn) app
    get "/api/v1/accounts/:name" (Route.Account.handleGet env.dbConn) app

startServer :: Int -> App -> Effect Server
startServer port app = M.listen (M.Port port) cb app
  where cb = log $ "Listening on 0.0.0.0:" <> show port

jsonMiddleware :: Effect Middleware
jsonMiddleware = M.makeJSONMiddleware {}

get :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
get path handler app = M.get (M.Path path) (M.makeHandler handler) app

post :: String -> (Request -> Response -> Effect Unit) -> App -> Effect Unit
post path handler app = M.post (M.Path path) (M.makeHandler handler) app

makeEnv :: Config -> Aff AppEnv
makeEnv { dbFilename, port } = do
  dbConn <- newDB dbFilename  
  pure $ { dbConn, port }

main :: Effect Unit
main = launchAff_ $ loadConfig >>= makeEnv >>= runServer

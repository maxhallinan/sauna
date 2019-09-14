module Main where

import Prelude

import App (Env)
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
import Server (runServer)
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

makeEnv :: Config -> Aff Env
makeEnv { dbFilename, port } = do
  dbConn <- newDB dbFilename
  pure $ { dbConn, port }

main :: Effect Unit
main = launchAff_ $ loadConfig >>= makeEnv >>= runServer

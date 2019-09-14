module Main where

import Prelude

import App (Env)
import Config (Config, loadConfig)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Server as Server
import SQLite3 (newDB)
import Router as Router

makeEnv :: Config -> Aff Env
makeEnv { dbFilename, port } = do
  dbConn <- newDB dbFilename
  pure $ { dbConn, port }

runServer :: Env -> Aff Unit
runServer env = do 
  router <- Router.makeRouter env
  Server.runServer env.port router

main :: Effect Unit
main = launchAff_ $ loadConfig >>= makeEnv >>= runServer

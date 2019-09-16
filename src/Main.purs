module Main where

import Prelude

import App (Env)
import Config (Config, loadConfig)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Server (port) 
import Server as S
import SQLite3 (newDB)
import Router as R

makeEnv :: Config -> Aff Env
makeEnv { dbFilename, port } = do
  dbConn <- newDB dbFilename
  pure $ { dbConn, port }

runServer :: Env -> Aff Unit
runServer env = do 
  router <- R.makeRouter env
  S.runServer (port env.port) router

main :: Effect Unit
main = launchAff_ $ loadConfig >>= makeEnv >>= runServer

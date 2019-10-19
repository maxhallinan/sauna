module Main where

import Prelude

import App.Env (Env(..))
import Config (Config, loadConfig)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Router as R
import SQLite3 (newDB)
import Server as S

makeEnv :: Config -> Aff Env
makeEnv { dbFilename, port } = do
  dbConn <- newDB dbFilename
  pure $ Env { dbConn
             , port: S.port port
             }

runServer :: Env -> Aff Unit
runServer env@(Env { port }) = do
  router <- R.makeRouter env
  S.runServer port router

main :: Effect Unit
main = launchAff_ $ loadConfig >>= makeEnv >>= runServer

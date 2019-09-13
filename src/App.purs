module App (AppEnv, App) where

import Prelude

import App.Env (Env)
import Effect.Aff (Aff)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)

type AppEnv = Env App

newtype App a = App (ReaderT AppEnv Aff a)

runApp :: forall a. AppEnv -> App a -> Aff a
runApp env (App r) = runReaderT r $ env

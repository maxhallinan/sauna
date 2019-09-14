module App 
  ( module App.Env
  , App
  , runApp
  ) where

import Prelude

import App.Env (Env)
import Effect.Aff (Aff)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)

newtype App a = App (ReaderT Env Aff a)
derive newtype instance functorApp :: Functor App
derive newtype instance applyApp :: Apply App
derive newtype instance applicativeApp :: Applicative App

runApp :: forall a. Env -> App a -> Aff a
runApp env (App r) = runReaderT r $ env

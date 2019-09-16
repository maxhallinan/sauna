module App 
  ( module App.Env
  , AppT(..)
  , App
  , AppErr(..)
  , runApp
  ) where

import Prelude

import App.Env (Env)
import Effect.Aff (Aff)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Newtype (class Newtype)

type App a = AppT AppErr Aff a

data AppErr = NotFound

newtype AppT e m a = AppT (ExceptT e (ReaderT Env m) a)
derive newtype instance functorAppT :: Functor m => Functor (AppT e m)
derive newtype instance applyAppT :: Monad m => Apply (AppT e m)
derive newtype instance applicativeAppT :: Monad m => Applicative (AppT e m)
derive newtype instance monadErrorAppT :: Monad m => MonadError e (AppT e m)
derive newtype instance monadThrowAppT :: Monad m => MonadThrow e (AppT e m)

runApp :: forall a. Env -> App a -> Aff (Either AppErr a)
runApp = runAppT

runAppT :: forall e m a. Env -> AppT e m a -> m (Either e a)
runAppT env (AppT e) = runReaderT (runExceptT e) env

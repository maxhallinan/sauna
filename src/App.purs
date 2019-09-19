module App
  ( AppT(..)
  , App
  , runApp
  ) where

import Prelude

import App.Env (Env)
import App.Err (Err)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

type App a = AppT Err Env Aff a

newtype AppT err env m a = AppT (ExceptT err (ReaderT env m) a)
derive newtype instance functorAppT :: Functor m => Functor (AppT err env m)
derive newtype instance applyAppT :: Monad m => Apply (AppT err env m)
derive newtype instance applicativeAppT :: Monad m => Applicative (AppT err env m)
derive newtype instance bindAppT :: Monad m => Bind (AppT err env m)
derive newtype instance monadAppT :: Monad m => Monad (AppT err env m)
derive newtype instance monadErrorAppT :: Monad m => MonadError err (AppT err env m)
derive newtype instance monadThrowAppT :: Monad m => MonadThrow err (AppT err env m)
derive newtype instance monadAskAppT :: Monad m => MonadAsk env (AppT err env m)
derive newtype instance monadReaderAppT :: Monad m => MonadReader env (AppT err env m)
derive newtype instance monadEffectAppT :: MonadEffect m => MonadEffect (AppT err env m)
derive newtype instance monadAffAppT :: MonadAff m => MonadAff (AppT err env m)

runApp :: forall a. Env -> App a -> Aff (Either Err a)
runApp = runAppT

runAppT :: forall err env m a. env -> AppT err env m a -> m (Either err a)
runAppT env (AppT e) = runReaderT (runExceptT e) env

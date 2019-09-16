module App 
  ( module App.Env
  , AppT(..)
  , App
  , Err(..)
  , ErrName(..)
  , runApp
  ) where

import Prelude

import App.Env (Env)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type App a = AppT Aff a

newtype Err = Err { name :: ErrName, msg :: Maybe String }

data ErrName = NotFound | BadRequest

newtype AppT m a = AppT (ExceptT Err (ReaderT Env m) a)
derive newtype instance functorAppT :: Functor m => Functor (AppT m)
derive newtype instance applyAppT :: Monad m => Apply (AppT m)
derive newtype instance applicativeAppT :: Monad m => Applicative (AppT m)
derive newtype instance bindAppT :: Monad m => Bind (AppT m)
derive newtype instance monadAppT :: Monad m => Monad (AppT m)
derive newtype instance monadErrorAppT :: Monad m => MonadError Err (AppT m)
derive newtype instance monadThrowAppT :: Monad m => MonadThrow Err (AppT m)

runApp :: forall a. Env -> App a -> Aff (Either Err a)
runApp = runAppT

runAppT :: forall m a. Env -> AppT m a -> m (Either Err a)
runAppT env (AppT e) = runReaderT (runExceptT e) env

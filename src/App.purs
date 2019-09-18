module App
  ( module App.Env
  , module App.Err
  , AppT(..)
  , App
  , runApp
  ) where

import Prelude

import App.Env (Env(..))
import App.Err (Err(..), ErrName(..), err, badRequest, badRequest_)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

type App a = AppT Aff a

newtype AppT m a = AppT (ExceptT Err (ReaderT Env m) a)
derive newtype instance functorAppT :: Functor m => Functor (AppT m)
derive newtype instance applyAppT :: Monad m => Apply (AppT m)
derive newtype instance applicativeAppT :: Monad m => Applicative (AppT m)
derive newtype instance bindAppT :: Monad m => Bind (AppT m)
derive newtype instance monadAppT :: Monad m => Monad (AppT m)
derive newtype instance monadErrorAppT :: Monad m => MonadError Err (AppT m)
derive newtype instance monadThrowAppT :: Monad m => MonadThrow Err (AppT m)
derive newtype instance monadAskAppT :: Monad m => MonadAsk Env (AppT m)
derive newtype instance monadReaderAppT :: Monad m => MonadReader Env (AppT m)
derive newtype instance monadEffectAppT :: MonadEffect m => MonadEffect (AppT m)
derive newtype instance monadAffAppT :: MonadAff m => MonadAff (AppT m)

runApp :: forall a. Env -> App a -> Aff (Either Err a)
runApp = runAppT

runAppT :: forall m a. Env -> AppT m a -> m (Either Err a)
runAppT env (AppT e) = runReaderT (runExceptT e) env

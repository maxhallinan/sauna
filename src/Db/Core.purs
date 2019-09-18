module Db.Core (asFirstRow, runQuery) where

import Prelude

import App.Env (class Has, grab)
import App.Err (Err, dbErr_, notFound)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (either)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign (Foreign, F)
import Foreign.Index as F.I
import Effect.Class (class MonadEffect)
import SQLite3 (DBConnection, queryDB)

asFirstRow 
  :: forall env m
   . Has DBConnection env 
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Foreign
  -> m Foreign
asFirstRow = 
  F.I.readIndex 0
  >>> runExcept
  >>> either throwDbErr pure
  where throwDbErr = const $ throwError dbErr_

runQuery 
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => String
  -> Array Foreign
  -> m Foreign
runQuery query params = do
  dbConn <- grab :: m DBConnection
  liftAff $ queryDB dbConn query params

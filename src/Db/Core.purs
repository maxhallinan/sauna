module Db.Core (asFirstRow, runQuery) where

import Prelude

import App.Err (Err, dbErr)
import App.Env (class Has, grab)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (either)
import Db.Err (DbErr(..), fromSQLite3Err)
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (message)
import Foreign (Foreign)
import Foreign.Index as F.I
import SQLite3 (DBConnection)
import SQLite3.Extra (queryDB)

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
  where throwDbErr = const $ throwError err
        err = dbErr ExpectedOneRow "Expected one row but got none."

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
  result <- run dbConn query params
  either throwDbErr pure result
  where run d q p = queryDB d q p # try # liftAff
        throwDbErr err = 
          let
              errName = fromSQLite3Err err
              msg = message err 
          in
          throwError $ dbErr errName msg

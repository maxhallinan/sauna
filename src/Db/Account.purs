module Db.Account (getAccountByUsername, insertAccount) where

import Prelude

import App.Env (class Has)
import App.Err (Err, dbErr)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Db.Core (asFirstRow, runQuery)
import Db.Err (DbErr(..))
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

decodeAccountRow :: Foreign -> F Account
decodeAccountRow f = lift2 toAccount (decodeId f) (decodeUsername f)
  where toAccount id username = Account { id, username }

decodeId :: Foreign -> F Int
decodeId = F.readInt <=< F.I.readProp "id" 

decodeUsername :: Foreign -> F String
decodeUsername = F.readString <=< F.I.readProp "username"

getAccountByUsername
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => String
  -> m Account
getAccountByUsername username = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  if isUndefined firstRow
  then throwRecordNotFound
  else do
    let account = decodeAccount firstRow
    either throwReadErr pure account
  where query = "SELECT * FROM accounts WHERE accounts.username = ?"  
        params = [F.unsafeToForeign username]
        decodeAccount = decodeAccountRow >>> runExcept
        throwRecordNotFound = throwError $ dbErr RecordNotFound ("Account not found: " <> username)
        throwReadErr err = throwError $ dbErr (ReadErr err) (intercalate " " $ map show err)

insertAccount
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { username :: String }
  -> m Account
insertAccount a = do
  x <- runQuery query params
  getAccountByUsername a.username
  where query = "INSERT INTO accounts (username) VALUES (?)"
        params = [F.unsafeToForeign a.username]

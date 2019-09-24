module Db.Account (getAccountByUsername, insertAccount) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Crypto (PrivateKey, PublicKey, unPrivateKey, unPublicKey)
import Data.Either (either)
import Data.Foldable (intercalate)
import Db.Core (asFirstRow, runQuery)
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
  then throwNotFound
  else do
    let account = decodeAccount firstRow
    either throwDbErr pure account
  where query = "SELECT * FROM accounts WHERE accounts.username = ?"  
        params = [F.unsafeToForeign username]
        decodeAccount = decodeAccountRow >>> runExcept
        throwNotFound = throwError $ Err.notFound ("Account not found: " <> username)
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

insertAccount
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { privKey :: PrivateKey, pubKey :: PublicKey, username :: String }
  -> m Account
insertAccount a = do
  _ <- runQuery query params
  getAccountByUsername a.username
  where query = "INSERT INTO accounts (username, privkey, pubkey) VALUES (?, ?, ?)"
        params = [ F.unsafeToForeign a.username
                 , F.unsafeToForeign $ unPrivateKey a.privKey
                 , F.unsafeToForeign $ unPublicKey a.pubKey
                 ]

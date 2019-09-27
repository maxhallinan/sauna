module Db.Account 
  ( getAccountByUsername
  , getAccountPrivKey
  , insertAccount
  ) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Crypto (PrivateKey, PublicKey, makePrivateKey, makePublicKey, unPrivateKey, unPublicKey)
import Data.Either (either)
import Data.Foldable (intercalate)
import Db.Core (asFirstRow, runQuery)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

readAccountRow :: Foreign -> F Account
readAccountRow f =
  makeAccount
  <$> readId f
  <*> readUsername f
  <*> readPubkey f
  where makeAccount id username pubKey = Account { id, pubKey, username }

readId :: Foreign -> F Int
readId = F.readInt <=< F.I.readProp "id"

readUsername :: Foreign -> F String
readUsername = F.readString <=< F.I.readProp "username"

readPubkey :: Foreign -> F PublicKey
readPubkey = map makePublicKey <<< F.readString <=< F.I.readProp "pubkey"

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
    let account = readAccount firstRow
    either throwDbErr pure account
  where query = "SELECT * FROM accounts WHERE accounts.username = ?"
        params = [F.unsafeToForeign username]
        readAccount = readAccountRow >>> runExcept
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

getAccountPrivKey 
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { id :: Int }
  -> m PrivateKey
getAccountPrivKey { id } = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  if isUndefined firstRow
  then throwNotFound
  else do
    let privateKey = readPrivateKey firstRow
    either throwDbErr pure privateKey
  where query = "SELECT privkey FROM accounts WHERE accounts.id = ?"
        params = [ F.unsafeToForeign id
                 ]
        readPrivateKey = readPrivateKeyRow >>> runExcept
        throwNotFound = throwError $ Err.notFound ("Account not found: " <> show id)
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

readPrivateKeyRow :: Foreign -> F PrivateKey
readPrivateKeyRow = map makePrivateKey <<< F.readString <=< F.I.readProp "privkey"

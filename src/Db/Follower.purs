module Db.Follower (deleteFollower, insertFollower) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account(..))
import Crypto (PrivateKey, PublicKey, makePublicKey, unPrivateKey, unPublicKey)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Db.Core (asFirstRow, runQuery)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

type Follower = { id :: Int, accountId :: Int, actorId :: Int }

readCollection :: Foreign -> F (Array Follower)
readCollection = F.readArray >=> traverse readRow

readRow :: Foreign -> F Follower
readRow f =
  makeAccount
  <$> readIntProp "id" f
  <*> readIntProp "account_id" f
  <*> readIntProp "actor_id" f
  where makeAccount id accountId actorId = { id, accountId, actorId }

readIntProp :: String -> Foreign -> F Int
readIntProp p = F.readInt <=< F.I.readProp p

isActorFollower
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, actorId :: Int }
  -> m Boolean
isActorFollower { accountId, actorId } = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  pure $ not (isUndefined firstRow)
  where query = "SELECT * FROM followers WHERE followers.account_id = ? AND followers.actor_id = ?"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign actorId
                 ]

getFollowersByAccountId
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Int
  -> m (Array Follower)
getFollowersByAccountId accountId = do
  rows <- runQuery query params
  let collection = read rows
  either throwDbErr pure collection
  where query = "SELECT * FROM followers WHERE followers.account_id = ?"
        params = [ F.unsafeToForeign accountId
                 ]
        read = readCollection >>> runExcept
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

insertFollower
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, actorId :: Int }
  -> m Unit
insertFollower { accountId, actorId } = do
  isFollower <- isActorFollower { accountId, actorId }
  if not isFollower
    then void $ runQuery query params
    else pure unit
  where query = "INSERT INTO followers (account_id, actor_id) VALUES (?, ?)"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign actorId
                 ]

deleteFollower
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, actorId :: Int }
  -> m Unit
deleteFollower { accountId, actorId } = do
  _ <- runQuery query params
  pure unit
  where query = "DELETE FROM followers WHERE followers.account_id = ? AND followers.actor_id = ?"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign actorId
                 ]

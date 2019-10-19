module Db.Following (insertFollowing) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Db.Core (asFirstRow, runQuery)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

type Following =
  { id :: Int
  , accountId :: Int
  , actorId :: Int
  }

readCollection :: Foreign -> F (Array Following)
readCollection = F.readArray >=> traverse readRow

readRow :: Foreign -> F Following
readRow f =
  makeAccount
  <$> readIntProp "id" f
  <*> readIntProp "account_id" f
  <*> readIntProp "actor_id" f
  where makeAccount id accountId actorId = { id, accountId, actorId }

readIntProp :: String -> Foreign -> F Int
readIntProp p = F.readInt <=< F.I.readProp p

isAccountFollowing
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, actorId :: Int }
  -> m Boolean
isAccountFollowing { accountId, actorId } = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  pure $ not (isUndefined firstRow)
  where query = "SELECT * FROM following WHERE following.account_id = ? AND following.actor_id = ?"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign actorId
                 ]

getFollowingByAccountId
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Int
  -> m (Array Following)
getFollowingByAccountId accountId = do
  rows <- runQuery query params
  let collection = read rows
  either throwDbErr pure collection
  where query = "SELECT * FROM following WHERE following.account_id = ?"
        params = [ F.unsafeToForeign accountId
                 ]
        read = readCollection >>> runExcept
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

insertFollowing
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, actorId :: Int }
  -> m Unit
insertFollowing { accountId, actorId } = do
  isFollowing <- isAccountFollowing { accountId, actorId }
  if not isFollowing
    then void $ runQuery query params
    else pure unit
  where query = "INSERT INTO following (account_id, actor_id) VALUES (?, ?)"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign actorId
                 ]

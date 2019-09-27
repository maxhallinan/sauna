module Db.Follower (deleteFollower) where

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

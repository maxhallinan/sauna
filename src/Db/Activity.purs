module Db.Activity 
  ( getActivityByActivityId
  , insertAccountActivity
  , insertActivity
  ) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.ActivityPub (Activity(..), ActivityType, fromActivityType, toActivityType)
import Data.Either (either)
import Data.Foldable (intercalate)
import Db.Core (asFirstRow, runQuery)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

readActivityRow :: Foreign -> F Activity
readActivityRow f =
  makeActivity
  <$> readId f
  <*> readActivityId f
  <*> readActivityType f
  where makeActivity id activityId activityType = Activity { id, activityId, activityType }

readId :: Foreign -> F Int
readId = F.readInt <=< F.I.readProp "id"

readActivityId :: Foreign -> F String
readActivityId = F.readString <=< F.I.readProp "activity_id"

readActivityType :: Foreign -> F ActivityType
readActivityType = map toActivityType <<< F.readString <=< F.I.readProp "activity_type"

getActivityByActivityId
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => String
  -> m Activity
getActivityByActivityId activityId = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  if isUndefined firstRow
    then throwNotFound
    else do
      let activity = readActivity firstRow
      either throwDbErr pure activity
  where query = "SELECT * FROM activities WHERE activities.activity_id = ?"
        params = [ F.unsafeToForeign activityId ]
        readActivity = readActivityRow >>> runExcept
        throwNotFound = throwError $ Err.notFound ("Activity not found: " <> activityId)
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

insertActivity
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { activityType :: ActivityType, activityId :: String }
  -> m Activity
insertActivity { activityId, activityType } = unlessActivityExists do
  _ <- runQuery query params
  getActivity
  where query = "INSERT INTO activities (activity_id, activity_type) VALUES (?, ?)"
        params = [ F.unsafeToForeign activityId
                 , F.unsafeToForeign $ fromActivityType activityType
                 ]

        getActivity :: m Activity
        getActivity = getActivityByActivityId activityId

        unlessActivityExists :: m Activity -> m Activity
        unlessActivityExists action = catchError getActivity (const action)

insertAccountActivity 
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { accountId :: Int, activityId :: Int }
  -> m Unit
insertAccountActivity { accountId, activityId } = do
  _ <- runQuery query params
  pure unit
  where query = "INSERT INTO account_activities (account_id, activity_id) VALUES (?, ?)"
        params = [ F.unsafeToForeign accountId
                 , F.unsafeToForeign activityId
                 ]

module Db.Actor (getActorByUri, insertActor) where

import Prelude

import App.Env (class Has)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (either)
import Data.Foldable (intercalate)
import Db.Core (asFirstRow, runQuery)
import Effect.Aff.Class (class MonadAff)
import Foreign (Foreign, F, isUndefined)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection)

getActorByUri 
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => String
  -> m { id :: Int, uri :: String }
getActorByUri uri = do
  rows <- runQuery query params
  firstRow <- asFirstRow rows
  if isUndefined firstRow
  then throwNotFound
  else do
    let actor = readActor firstRow
    either throwDbErr pure actor
  where query = "SELECT * FROM actors WHERE actors.uri = ?"
        params = [F.unsafeToForeign uri]
        readActor = readActorRow >>> runExcept
        throwNotFound = throwError $ Err.notFound ("Actor not found: " <> uri)
        throwDbErr = throwError <<< Err.dbErr <<< intercalate " " <<< map show

insertActor 
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => { uri :: String }
  -> m { id :: Int, uri :: String }
insertActor { uri } = unlessActorExists do
  _ <- runQuery query params
  getActor
  where query = "INSERT INTO actors (uri) VALUES (?)"
        params = [ F.unsafeToForeign uri ]

        getActor :: m { id :: Int, uri :: String }
        getActor = getActorByUri uri

        unlessActorExists :: m { id :: Int, uri :: String } -> m { id :: Int, uri :: String } 
        unlessActorExists action = catchError getActor (const action)

readActorRow :: Foreign -> F { id :: Int, uri :: String }
readActorRow f =
  makeActor
  <$> readId f
  <*> readUri f
  where makeActor id uri = { id, uri }

readId :: Foreign -> F Int
readId = F.readInt <=< F.I.readProp "id"

readUri :: Foreign -> F String
readUri = F.readString <=< F.I.readProp "uri"

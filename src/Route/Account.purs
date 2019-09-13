module Route.Account (handleGet, handlePost) where

import Prelude

import Control.Apply (lift2)
import Control.Bind ((<=<))
import Control.Monad.Except (runExcept)
import Data.Account (Account(..))
import Data.Account as Account
import Data.Argonaut as Ar
import Data.Either (Either (..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, launchAff, launchAff_)
import Foreign as F
import Foreign.Index as F.I
import Makkori (Request, Response)
import Makkori as M
import SQLite3 (DBConnection, queryDB)

account :: String -> Int -> Account.Account
account username id = Account {id, username}

handleGet :: DBConnection -> Request -> Response -> Effect Unit
handleGet db req res = launchAff_ do
  name <- liftEffect $ getNameParam req
  case name of
    Left _ ->
      liftEffect $ unknownError res
    Right n -> do
      account <- getAccount n db
      case account of
        Left err -> do
          liftEffect $ notFound res
        Right a ->
          liftEffect $ M.sendResponse (Account.toJsonString a) res

getAccount ::  String -> DBConnection -> Aff (Either F.MultipleErrors Account.Account)
getAccount name db = do
  row <- map firstRow query
  let name' = bind row accountName
  let id = bind row accountId
  pure (runExcept $ lift2 account name' id)
  where query = queryDB db "SELECT id, username FROM accounts WHERE accounts.username = ?" params
        params = [F.unsafeToForeign name]
        firstRow = F.I.readIndex 0
        accountName = F.readString <=< F.I.readProp "username"
        accountId = F.readInt <=< F.I.readProp "id"

notFound :: Response -> Effect Unit
notFound = emptyResponseWithStatus 404

unknownError :: Response -> Effect Unit
unknownError = emptyResponseWithStatus 500

emptyResponseWithStatus :: Int -> Response -> Effect Unit
emptyResponseWithStatus status res = do
  _ <- M.setStatus status res
  M.sendResponse "" res

getNameParam :: Request -> Effect (Either F.MultipleErrors String)
getNameParam = map getName <<< M.getParams
  where getName = runExcept <<< (F.readString <=< F.I.readProp "name")

handlePost :: DBConnection -> Request -> Response -> Effect Unit
handlePost db req res = notFound res

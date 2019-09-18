module Handler.Account (handleGet, handlePost) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Except (runExcept)
import Core.Account (Account(..))
import Core.Account as Account
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
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
      acct <- getAccount n db
      case acct of
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
handlePost db req res = launchAff_ $ do
  postBody <- liftEffect $ getPostBody req
  case postBody of
    Left err ->
      liftEffect $ unknownError res
    Right b -> do
      let params = [F.unsafeToForeign b.name]
      _ <- insert params
      row <- map firstRow (select params)
      let name = bind row accountName
      let id = bind row accountId
      case (runExcept $ lift2 account name id) of
        Left err ->
          liftEffect $ unknownError res
        Right a ->
          liftEffect $ M.sendResponse (Account.toJsonString a) res
  where insert params = queryDB db "INSERT INTO accounts (username) VALUES (?)" params
        select params = queryDB db "SELECT * FROM accounts WHERE accounts.username = ?" params
        firstRow = F.I.readIndex 0
        accountName = F.readString <=< F.I.readProp "username"
        accountId = F.readInt <=< F.I.readProp "id"

getPostBody :: Request -> Effect (Either F.MultipleErrors { name :: String })
getPostBody req = do
  body <- M.getBody req
  let postData = map (\name -> {name}) $ F.I.readProp "username" body >>= F.readString
  pure $ runExcept postData

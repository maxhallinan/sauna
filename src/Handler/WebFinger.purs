module Handler.WebFinger (handleGet) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either (..))
import Data.WebFinger (WebFinger(..), toJsonString)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Foreign as F
import Foreign.Index as F.I
import SQLite3 (DBConnection, queryDB)
import Makkori (Request, Response)
import Makkori as M
import Makkori.Extra as ME

handleGet :: DBConnection -> Request -> Response -> Effect Unit
handleGet db req res = launchAff_ do
  query <- liftEffect $ ME.getQuery req
  let resource = resourceParam query
  case runExcept resource of
    Left err -> do
      liftEffect $ M.sendResponse (show err) res
    Right r -> do
      row <- map firstRow $ queryDB db "SELECT * FROM accounts WHERE accounts.username = ?" [F.unsafeToForeign r]
      let name = bind row accountName
      case runExcept name of
        Left err -> do
          liftEffect $ M.sendResponse "" res
        Right n -> do
          liftEffect $ M.sendResponse (toJsonString $ WebFinger {subject: n}) res
  where resourceParam = F.readString <=< F.I.readProp "resource"
        firstRow = F.I.readIndex 0
        accountName = F.readString <=< F.I.readProp "username"

module Route.WebFinger (handleGet) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either (..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff, launchAff_)
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
    Left err ->
      liftEffect $ M.sendResponse (show err) res
    Right r ->
      liftEffect $ M.sendResponse r res
  where resourceParam = F.readString <=< F.I.readProp "resource"

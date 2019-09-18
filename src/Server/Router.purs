module Server.Router
  ( makeRouter
  , registerRoute
  , useSubRouter
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Server.Core (Handler, Method(..), Path, Request, Response, Router)
import Effect.Aff (Aff, launchAff_)
import Makkori as M
import Makkori.Extra as M.E

makeRouter :: Effect Router
makeRouter = M.E.makeRouter

useSubRouter :: Path -> Router -> Router -> Effect Unit
useSubRouter = M.E.useSubRouter

registerRoute :: Method -> Path -> Handler -> Router -> Effect Unit
registerRoute method path handler router =
  case method of
    Delete ->
      registerWith M.E.delete
    Get ->
      registerWith M.E.get
    Post ->
      registerWith M.E.post
    Put ->
      registerWith M.E.put
  where registerWith r = r path (makeHandler handler) router

makeHandler :: Handler -> M.Handler
makeHandler handler = M.makeHandler wrapped
  where wrapped req res =
          launchAff_ $
            getRequest req
            >>= handler
            >>= sendResponse res

getRequest :: M.Request -> Aff Request
getRequest req = liftEffect $ do
  body <- M.getBody req
  params <- M.getParams req
  query <- M.E.getQuery req
  pure { body, params, query }

sendResponse :: M.Response -> Response -> Aff Unit
sendResponse res { body, headers, status } = liftEffect $ do
  M.setStatus status res
  _ <- traverse (\(Tuple k v) -> M.setHeader k v res) headers
  M.sendResponse body res
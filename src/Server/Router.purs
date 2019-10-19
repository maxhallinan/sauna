module Server.Router
  ( makeRouter
  , registerRoute
  , useRouterMiddleware
  , useSubRouter
  ) where

import Prelude

import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Makkori as M
import Makkori.Extra as M.E
import Server.Core (Handler, Method(..), Middleware, Path, Request, Response, Router)

makeRouter :: Effect Router
makeRouter = M.E.makeRouter

useRouterMiddleware :: Path -> Middleware -> Router -> Effect Unit
useRouterMiddleware = M.E.useRouterMiddleware

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
  let accepts = M.E.makeAccepts req
  body <- M.getBody req
  headers <- M.E.getHeaders req
  hostname <- M.E.getHostname req
  params <- M.getParams req
  query <- M.E.getQuery req
  pure { accepts
       , body
       , headers
       , hostname
       , method: M.E.getMethod req
       , params
       , originalUrl: M.E.getOriginalUrl req
       , query
       }

sendResponse :: M.Response -> Response -> Aff Unit
sendResponse res { body, headers, status } = liftEffect $ do
  M.setStatus status res
  _ <- traverse (\(Tuple k v) -> M.setHeader k v res) headers
  M.sendResponse body res

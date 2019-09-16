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
import Server.Core (class Input, class Output, Method(..), Path, Router, toInput, fromOutput)
import Effect.Aff (Aff, launchAff_)
import Makkori (Request, Response)
import Makkori as M
import Makkori.Extra as ME

makeRouter :: Effect Router
makeRouter = ME.makeRouter

useSubRouter :: String -> Router -> Router -> Effect Unit
useSubRouter = ME.useSubRouter <<< M.Path

registerRoute 
  :: forall i o
   . Input i 
  => Output o 
  => Method
  -> Path
  -> (i -> Aff o)
  -> Router
  -> Effect Unit
registerRoute method path handler router =
  let
    register r = r (M.Path path) (makeHandler handler) router
  in
  case method of
    Delete ->
      register ME.delete
    Get ->
      register ME.get
    Post ->
      register ME.post
    Put ->
      register ME.put

makeHandler
  :: forall i o
   . Input i
  => Output o
  => (i -> Aff o)
  -> M.Handler
makeHandler handler = M.makeHandler h
  where h :: Request -> Response -> Effect Unit
        h req res = launchAff_ do
          input <- getRequest req
          output <- handler input
          sendResponse output res

getRequest :: forall i. Input i => Request -> Aff i
getRequest req = liftEffect $ do
  body <- M.getBody req
  params <- M.getParams req
  query <- ME.getQuery req
  pure $ toInput { body, params, query }

sendResponse :: forall o. Output o => o -> M.Response -> Aff Unit
sendResponse output res = liftEffect $ do
  setStatus r.status
  setHeaders r.headers
  setBody r.body
  where r :: { body :: String , headers :: Array (Tuple String String), status :: Int }
        r = fromOutput output

        setHeaders :: Array (Tuple String String) -> Effect Unit
        setHeaders = void <<< traverse setHeader

        setHeader :: Tuple String String -> Effect Unit
        setHeader (Tuple k v) = M.setHeader k v res

        setStatus :: Int -> Effect Unit
        setStatus = flip M.setStatus res

        setBody :: String -> Effect Unit
        setBody = flip M.sendResponse res

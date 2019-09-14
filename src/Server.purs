module Server (runServer) where

import Prelude

import App (App, Env, runApp)
import Control.Monad (void)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Foreign (Foreign)
import Makkori as M
import Makkori.Extra as ME

runServer :: Env -> Aff Unit
runServer env = liftEffect $ do
  app <- M.makeApp
  json <- M.makeJSONMiddleware {}
  _ <- M.use (M.Path "/") json app
  _ <- M.get (M.Path "/hello-world") (makeHandler env helloWorld) app
  _ <- M.listen (M.Port env.port) (pure unit) app
  pure unit

makeHandler 
  :: forall i o
   . Request i
  => Response o
  => Env
  -> (i -> App o) 
  -> M.Handler
makeHandler env handler = M.makeHandler h
  where h :: M.Request -> M.Response -> Effect Unit 
        h req res = launchAff_ do
          input <- getRequest req
          output <- runApp env (handler input)
          sendResponse output res

getRequest :: forall a. Request a => M.Request -> Aff a
getRequest req = liftEffect $ do
  body <- M.getBody req
  params <- M.getParams req
  query <- ME.getQuery req
  pure $ toInput { body, params, query }

sendResponse :: forall a. Response a => a -> M.Response -> Aff Unit
sendResponse output res = liftEffect $ do
  setStatus r.status
  setHeaders r.headers
  sendResponse r.body
  where r :: { body :: String , headers :: Array (Tuple String String), status :: Int }
        r = fromOutput output

        setHeaders :: Array (Tuple String String) -> Effect Unit
        setHeaders = void <<< traverse setHeader

        setHeader :: Tuple String String -> Effect Unit
        setHeader (Tuple k v) = M.setHeader k v res

        setStatus :: Int -> Effect Unit
        setStatus = flip M.setStatus res

        sendResponse :: String -> Effect Unit
        sendResponse = flip M.sendResponse res

class Request input where
  toInput :: { body :: Foreign, params :: Foreign, query :: Foreign } -> input

class Response output where
  fromOutput :: output -> { body :: String , headers :: Array (Tuple String String), status :: Int }

helloWorld :: HelloWorldReq -> App HelloWorldRes
helloWorld _ = pure (HelloWorldRes unit)

newtype HelloWorldReq = HelloWorldReq Unit

instance requestHelloWorld :: Request HelloWorldReq where
  toInput _ = HelloWorldReq unit

newtype HelloWorldRes = HelloWorldRes Unit

instance responseHelloWorld :: Response HelloWorldRes where
  fromOutput _ = { body: "Hello world!", headers: [Tuple "foo" "bar"], status: 200 }

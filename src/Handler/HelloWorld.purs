module Handler.HelloWorld (HelloWorldReq, HelloWorldRes, getHelloWorld) where

import Prelude

import App (App, Env, runApp)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Server (class Request, class Response, Method(..), Router)
import Server as Server

getHelloWorld :: Env -> (HelloWorldReq -> Aff HelloWorldRes)
getHelloWorld = makeHandler _getHelloWorld

_getHelloWorld :: HelloWorldReq -> App HelloWorldRes
_getHelloWorld _ = pure (HelloWorldRes unit)

newtype HelloWorldReq = HelloWorldReq Unit

instance requestHelloWorld :: Request HelloWorldReq where
  toInput _ = HelloWorldReq unit

newtype HelloWorldRes = HelloWorldRes Unit

instance responseHelloWorld :: Response HelloWorldRes where
  fromOutput _ = { body: "Hello world!", headers: [Tuple "foo" "bar"], status: 200 }

makeHandler 
  :: forall i o
   . Request i 
  => Response o 
  => (i -> App o) 
  -> Env 
  -> (i -> Aff o)
makeHandler handler env = runApp env <<< handler

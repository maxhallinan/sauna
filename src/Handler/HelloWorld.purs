module Handler.HelloWorld (HelloWorldReq, HelloWorldRes, getHelloWorld) where

import Prelude

import App (App, Env, runApp)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Server (class Input, class Output)

getHelloWorld :: Env -> (HelloWorldReq -> Aff HelloWorldRes)
getHelloWorld = makeHandler _getHelloWorld

_getHelloWorld :: HelloWorldReq -> App HelloWorldRes
_getHelloWorld _ = pure (HelloWorldRes unit)

newtype HelloWorldReq = HelloWorldReq Unit

instance inputHelloWorld :: Input HelloWorldReq where
  toInput _ = HelloWorldReq unit

newtype HelloWorldRes = HelloWorldRes Unit

instance outputHelloWorld :: Output HelloWorldRes where
  fromOutput _ = { body: "Hello world!", headers: [Tuple "foo" "bar"], status: 200 }

makeHandler 
  :: forall i o
   . Input i 
  => Output o 
  => (i -> App o) 
  -> Env 
  -> (i -> Aff o)
makeHandler handler env = runApp env <<< handler

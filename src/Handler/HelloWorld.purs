module Handler.HelloWorld (HelloWorldReq, HelloWorldRes, getHelloWorld) where

import Prelude

import App (App, AppErr(..), Env, runApp)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Server (class Input, class Output)

getHelloWorld :: Env -> HelloWorldReq -> Aff HelloWorldRes
getHelloWorld env = map HelloWorldRes <<< runApp env <<< handler
  
handler :: HelloWorldReq -> App String
handler _ = pure "Hello world!"

newtype HelloWorldReq = HelloWorldReq Unit

instance inputHelloWorld :: Input HelloWorldReq where
  toInput _ = HelloWorldReq unit

newtype HelloWorldRes = HelloWorldRes (Either AppErr String)

instance outputHelloWorld :: Output HelloWorldRes where
  fromOutput (HelloWorldRes res) = case res of
    Left NotFound ->
      { body: "Not found!", headers: [], status: 404  }
    Right msg ->
      { body: msg, headers: [Tuple "foo" "bar"], status: 200  }

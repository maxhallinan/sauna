module Server 
  ( module Server.Router 
  , module Server.Core
  , runServer
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Makkori as M
import Makkori.Extra as ME
import Server.Core (Port, Router)
import Server.Router (makeRouter, registerRoute, useSubRouter) as Server.Router
import Server.Core (class Input, class Output, Method(..), Path, Port, Router, fromOutput, toInput) as Server.Core                                                                                          

runServer :: Port -> Router -> Aff Unit
runServer port router = liftEffect $ do
  app <- M.makeApp
  json <- M.makeJSONMiddleware {}
  _ <- M.use (M.Path "/") json app
  _ <- ME.useRouter (M.Path "/") router app
  void $ M.listen (M.Port port) (pure unit) app

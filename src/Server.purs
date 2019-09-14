module Server 
  ( module Server.Router 
  , module Server.Core
  , runServer
  ) where

import Prelude

import Data.Functor (void)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Makkori as M
import Makkori.Extra as ME
import Server.Core (Port, Router)
import Server.Core as Server.Core
import Server.Router as Server.Router

runServer :: Port -> Router -> Aff Unit
runServer port router = liftEffect $ do
  app <- M.makeApp
  json <- M.makeJSONMiddleware {}
  _ <- M.use (M.Path "/") json app
  _ <- ME.useRouter (M.Path "/") router app
  void $ M.listen (M.Port port) (pure unit) app

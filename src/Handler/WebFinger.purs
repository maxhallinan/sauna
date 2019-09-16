module Handler.WebFinger (handleGet) where

import App (Env)
import Effect.Aff (Aff)
import Handler.WebFinger.Get as WebFinger.Get
import Server (Request, Response)

handleGet :: Env -> Request -> Aff Response
handleGet = WebFinger.Get.handleGet

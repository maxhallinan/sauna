module Route.Account (handleGet, handlePost) where

import Prelude

import Effect (Effect)
import Makkori (Request, Response)
import Makkori as M

handleGet :: Request -> Response -> Effect Unit
handleGet _ res = M.sendResponse "Hello world!" res

handlePost :: Request -> Response -> Effect Unit
handlePost _ res = M.sendResponse "Hello world!" res

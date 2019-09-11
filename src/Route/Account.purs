module Route.Account (handleGet, handlePost) where

import Prelude

import Data.Account (Account(..))
import Data.Account as Account
import Effect (Effect)
import Makkori (Request, Response)
import Makkori as M

account :: Account.Account
account = Account {id: "foo", username: "bar"}

handleGet :: Request -> Response -> Effect Unit
handleGet _ res = M.sendResponse (Account.toJsonString account) res

handlePost :: Request -> Response -> Effect Unit
handlePost _ res = M.sendResponse (Account.toJsonString account) res

module Handler.WebFinger.Get (handler) where

import Prelude

import App (App, Env, badRequest)
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either, either)
import Data.WebFinger (WebFinger(..), toJsonString)
import Effect.Aff (Aff)
import Foreign as F
import Foreign.Index as F.I
import Handler (class Input, class Output, runHandler)
import Server (Request, Response)

newtype In = In { resource :: Either F.MultipleErrors String }

instance inputIn :: Input In where
  fromRequest = requestToIn

requestToIn :: Request -> In
requestToIn { query } = In { resource: resourceParam query }
  where resourceParam = runExcept <<< (F.readString <=< F.I.readProp "resource")

newtype Out = Out WebFinger

instance outputOut :: Output Out where
  toResponse = outToResponse

outToResponse :: Out -> Response
outToResponse (Out webFinger) =
  { body: toJsonString webFinger
  , headers: []
  , status: 200
  }

handler :: Env -> Request -> Aff Response
handler env = runHandler env $ 
  unwrapIn
  >>> _.resource
  >>> either handleBadReq handleGoodReq
  >>> wrapOut
  where unwrapIn (In i) = i
        wrapOut = map Out

handleBadReq :: F.MultipleErrors -> App WebFinger
handleBadReq _ = throwError (badRequest "Missing `resource` query parameter.")

handleGoodReq :: String -> App WebFinger
handleGoodReq r = pure $ WebFinger { subject: r }

{-- handleGet :: DBConnection -> Request -> Response -> Effect Unit --}
{-- handleGet db req res = launchAff_ do --}
{--   query <- liftEffect $ ME.getQuery req --}
{--   let resource = resourceParam query --}
{--   case runExcept resource of --}
{--     Left err -> do --}
{--       liftEffect $ M.sendResponse (show err) res --}
{--     Right r -> do --}
{--       row <- map firstRow $ queryDB db "SELECT * FROM accounts WHERE accounts.username = ?" [F.unsafeToForeign r] --}
{--       let name = bind row accountName --}
{--       case runExcept name of --}
{--         Left err -> do --}
{--           liftEffect $ M.sendResponse "" res --}
{--         Right n -> do --}
{--           liftEffect $ M.sendResponse (toJsonString $ WebFinger {subject: n}) res --}
{--   where resourceParam = F.readString <=< F.I.readProp "resource" --}
{--         firstRow = F.I.readIndex 0 --}
{--         accountName = F.readString <=< F.I.readProp "username" --}

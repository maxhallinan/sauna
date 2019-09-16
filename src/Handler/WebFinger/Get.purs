module Handler.WebFinger.Get (handleGet) where

import Prelude

import App (App, Err(..), ErrName(..), Env)
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.WebFinger (WebFinger(..), toJsonString)
import Foreign as F
import Foreign.Index as F.I
{-- import SQLite3 (DBConnection, queryDB) --}
import Effect.Aff (Aff)
import Server (Request, Response)
import Handler (class Input, class Output, runHandler)

handleGet :: Env -> Request -> Aff Response
handleGet env = runHandler env handler
  where handler = wrapOut <<< either handleBadReq handleGetReq <<< _.resource <<< unwrapIn
        unwrapIn (In i) = i
        wrapOut = map Out

handleBadReq :: F.MultipleErrors -> App WebFinger
handleBadReq _ = throwError $ Err { name: BadRequest, msg: Just "Missing `resource` query parameter." }

handleGetReq :: String -> App WebFinger
handleGetReq r = pure $ WebFinger { subject: r }

newtype In = In { resource :: Either F.MultipleErrors String }

instance inputIn :: Input In where
  fromRequest { query } =
    In { resource: runExcept $ F.I.readProp "resource" query >>= F.readString }

newtype Out = Out WebFinger

instance outputOut :: Output Out where
  toResponse (Out webFinger) =
    { body: toJsonString webFinger
    , headers: []
    , status: 200
    }

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


module App.Err
  ( Err(..)
  , ErrName(..)
  , err
  , badRequest
  , dbErr
  , unknown
  ) where

import Data.Maybe (Maybe(..))
import Db.Err (DbErr)

newtype Err = Err { name :: ErrName, msg :: Maybe String }

data ErrName = BadRequest | DbErr DbErr | Unknown

makeErr :: ErrName -> Maybe String -> Err
makeErr name msg = Err { name, msg }

err :: ErrName -> String -> Err
err name msg = makeErr name (Just msg)

dbErr :: DbErr -> String -> Err
dbErr name msg = err (DbErr name) msg

badRequest :: String -> Err
badRequest = err BadRequest

unknown :: String -> Err
unknown = err Unknown

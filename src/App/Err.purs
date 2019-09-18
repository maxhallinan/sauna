module App.Err 
  ( Err(..)
  , ErrName(..)
  , err
  , err_
  , badRequest
  , badRequest_
  , conflict
  , conflict_
  , dbErr
  , dbErr_
  , notFound
  , notFound_
  , unknown
  , unknown_
  ) where

import Data.Maybe (Maybe(..))

newtype Err = Err { name :: ErrName, msg :: Maybe String }

data ErrName = BadRequest | Conflict | DbErr | NotFound | Unknown

makeErr :: ErrName -> Maybe String -> Err
makeErr name msg = Err { name, msg }

err :: ErrName -> String -> Err
err name msg = makeErr name (Just msg)

err_ :: ErrName -> Err
err_ name = makeErr name Nothing

dbErr :: String -> Err
dbErr = err DbErr

dbErr_ :: Err
dbErr_ = err_ DbErr

badRequest :: String -> Err
badRequest = err BadRequest

badRequest_ :: Err
badRequest_ = err_ BadRequest

notFound :: String -> Err
notFound = err NotFound

notFound_ :: Err
notFound_ = err_ NotFound

conflict :: String -> Err
conflict = err Conflict

conflict_ :: Err
conflict_ = err_ Conflict

unknown :: String -> Err
unknown = err Unknown

unknown_ :: Err
unknown_ = err_ Unknown

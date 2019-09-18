module App.Err 
  ( Err(..)
  , ErrName(..)
  , err
  , err_
  , badRequest
  , badRequest_
  , dbErr
  , dbErr_
  , notFound
  , notFound_
  ) where

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype Err = Err { name :: ErrName, msg :: Maybe String }

data ErrName = DbErr | NotFound | BadRequest

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

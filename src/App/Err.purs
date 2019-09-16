module App.Err 
  ( Err(..)
  , ErrName(..)
  , err
  , err_
  , badRequest
  , badRequest_
  , notFound
  , notFound_
  ) where

import Data.Maybe (Maybe(..))

newtype Err = Err { name :: ErrName, msg :: Maybe String }

data ErrName = NotFound | BadRequest

makeErr :: ErrName -> Maybe String -> Err
makeErr name msg = Err { name, msg }

err :: ErrName -> String -> Err
err name msg = makeErr name (Just msg)

err_ :: ErrName -> Err
err_ name = makeErr name Nothing

badRequest :: String -> Err
badRequest = err BadRequest

badRequest_ :: Err
badRequest_ = err_ BadRequest

notFound :: String -> Err
notFound = err NotFound

notFound_ :: Err
notFound_ = err_ BadRequest

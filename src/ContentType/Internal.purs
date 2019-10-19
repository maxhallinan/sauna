module ContentType.Internal (ContentType, parse) where

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Foreign (Foreign)

type ContentType =
  { mimeType :: String
  , parameters :: Foreign
  }

parse :: String -> Either String ContentType
parse = runFn3 _parse Left Right

foreign import _parse :: Fn3 (String -> Either String ContentType) (ContentType -> Either String ContentType) String (Either String ContentType)

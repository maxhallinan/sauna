module App.Err
  ( Err(..)
  , ErrCode(..)
  , err
  , badRequest
  , conflict
  , dbErr
  , invalidData
  , notFound
  , unknown
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype Err = Err { code :: ErrCode, msg :: Maybe String }

instance encodeJsonErr :: EncodeJson Err where
  encodeJson (Err { code, msg }) = A.fromObject obj
    where obj = O.fromFoldable [ Tuple "code" (encodeJson code)
                               , Tuple "msg" (A.fromString $ fromMaybe "" msg)
                               ]

data ErrCode 
  = BadRequest
  | Conflict
  | DbErr
  | InvalidData
  | NotFound
  | Unknown

newtype FieldErr = 
  FieldErr { code :: String
           , field :: String
           }

instance encodeJsonErrCode :: EncodeJson ErrCode where
  encodeJson BadRequest = A.fromString "BAD_REQUEST"
  encodeJson Conflict = A.fromString "CONFLICT"
  encodeJson DbErr = A.fromString "DB_ERROR"
  encodeJson InvalidData = A.fromString "INVALID_DATA"
  encodeJson NotFound = A.fromString "NOT_FOUND"
  encodeJson Unknown = A.fromString "UNKNOWN"

makeErr :: ErrCode -> Maybe String -> Err
makeErr code msg = Err { code, msg }

err :: ErrCode -> String -> Err
err code msg = makeErr code (Just msg)

badRequest :: String -> Err
badRequest = err BadRequest

conflict :: String -> Err
conflict = err Conflict

dbErr :: String -> Err
dbErr = err DbErr

invalidData :: String -> Err
invalidData = err InvalidData

notFound :: String -> Err
notFound = err NotFound

unknown :: String -> Err
unknown = err Unknown

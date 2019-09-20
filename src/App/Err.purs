module App.Err
  ( Err(..)
  , ErrCode(..)
  , FieldErr(..)
  , FieldErrCode(..)
  , err
  , badRequest
  , conflict
  , dbErr
  , fieldErr
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
  encodeJson = encodeErr

encodeErr :: Err -> A.Json
encodeErr (Err { code, msg }) =
  case code of
       InvalidData fieldErrs ->
         fromFoldable [ Tuple "code" $ encodeJson code
                      , Tuple "fields" $  A.fromArray (map encodeJson fieldErrs)
                      ]
       _ ->
         fromFoldable [ Tuple "code" $ encodeJson code
                      , Tuple "msg" $ A.fromString (fromMaybe "" msg)
                      ]
  where fromFoldable = A.fromObject <<< O.fromFoldable

data ErrCode
  = BadRequest
  | Conflict
  | DbErr
  | InvalidData (Array FieldErr)
  | NotFound
  | Unknown

instance encodeJsonErrCode :: EncodeJson ErrCode where
  encodeJson = A.fromString <<< toErrName

toErrName :: ErrCode -> String 
toErrName BadRequest = "BAD_REQUEST"
toErrName Conflict = "CONFLICT"
toErrName DbErr = "DB_ERROR"
toErrName (InvalidData _) = "INVALID_DATA"
toErrName NotFound = "NOT_FOUND"
toErrName Unknown = "UNKNOWN"

newtype FieldErr =
  FieldErr { code :: FieldErrCode
           , field :: String
           }

instance encodeJsonFieldErr :: EncodeJson FieldErr where
  encodeJson = encodeFieldErr

encodeFieldErr :: FieldErr -> A.Json
encodeFieldErr (FieldErr { code, field }) = A.fromObject $ O.fromFoldable
  [ Tuple "code" $ encodeJson code
  , Tuple "field" $ A.fromString field
  ]

data FieldErrCode
  = NotUnique
  | Required

instance encodeJsonFieldErrCode :: EncodeJson FieldErrCode where
  encodeJson = A.fromString <<< toFieldErrName

toFieldErrName :: FieldErrCode -> String
toFieldErrName NotUnique = "NOT_UNIQUE"
toFieldErrName Required = "REQUIRED"

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

invalidData :: Array FieldErr -> Err
invalidData fieldErrs = makeErr (InvalidData fieldErrs) Nothing

notFound :: String -> Err
notFound = err NotFound

unknown :: String -> Err
unknown = err Unknown

fieldErr :: String -> FieldErrCode -> FieldErr
fieldErr field code = FieldErr { field, code }

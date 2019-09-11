module Data.Account (Account(..), AccountFields, toJsonString) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype Account = Account AccountFields

type AccountFields = 
  { id :: String
  , username :: String
  }

instance encodeJsonAccount :: EncodeJson Account where
  encodeJson (Account fields) = A.fromObject obj
    where obj = O.fromFoldable [ Tuple "id" (A.fromString fields.id)
                               , Tuple "username" (A.fromString fields.username)
                               ]

toJsonString :: Account -> String
toJsonString = A.stringify <<< encodeJson

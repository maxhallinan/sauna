module Core.Account (Account(..), AccountFields, toJsonString) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype Account = Account AccountFields

type AccountFields = 
  { id :: Int
  , username :: String
  }

instance encodeJsonAccount :: EncodeJson Account where
  encodeJson (Account fields) = A.fromObject obj
    where obj = O.fromFoldable [ Tuple "id" (A.fromNumber $ toNumber fields.id)
                               , Tuple "username" (A.fromString fields.username)
                               ]

toJsonString :: Account -> String
toJsonString = A.stringify <<< encodeJson

module Data.WebFinger (WebFinger(..), WebFingerFields, toJsonString) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype WebFinger = WebFinger WebFingerFields

type WebFingerFields =
  { subject :: String
  }

instance encodeJsonWebFinger :: EncodeJson WebFinger where
  encodeJson (WebFinger fields) = A.fromObject obj
    where obj = O.fromFoldable [ Tuple "subject" (A.fromString fields.subject)
                               ]

toJsonString :: WebFinger -> String
toJsonString = A.stringify <<< encodeJson

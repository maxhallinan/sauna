module Core.WebFinger
  ( Link(..)
  , LinkType(..)
  , WebFinger(..)
  , WebFingerFields
  , toJsonString
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype WebFinger = WebFinger WebFingerFields

type WebFingerFields =
  { aliases :: Array String
  , links :: Array Link
  , subject :: String
  }

instance encodeJsonWebFinger :: EncodeJson WebFinger where
  encodeJson = encodeWebFinger

encodeWebFinger :: WebFinger -> Json
encodeWebFinger (WebFinger wf) = A.fromObject $ O.fromFoldable $
  [ Tuple "aliases" $ A.fromArray (map A.fromString wf.aliases)
  , Tuple "links" $ A.fromArray (map encodeLink wf.links)
  , Tuple "subject" $ A.fromString wf.subject
  ]

newtype Link = Link
  { href :: String
  , rel :: String
  , type_ :: LinkType
  }

instance encodeJsonLink :: EncodeJson Link where
  encodeJson = encodeLink

encodeLink :: Link -> Json
encodeLink (Link link) = A.fromObject $ O.fromFoldable
  [ Tuple "href" $ A.fromString link.href
  , Tuple "rel" $ A.fromString link.rel
  , Tuple "type" $ A.fromString (toMediaType link.type_)
  ]

data LinkType = ActivityJson

toMediaType :: LinkType -> String
toMediaType ActivityJson = "application/activity+json"

toJsonString :: WebFinger -> String
toJsonString = A.stringify <<< encodeJson

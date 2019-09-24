module Core.ActivityPub (Person(..), PublicKeyProp) where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as A
import Data.Tuple (Tuple(..))
import Foreign.Object as O

newtype Person = Person
  { context :: Array String
  , id :: String
  , inbox :: String
  , name :: String
  , outbox :: String
  , publicKey :: PublicKeyProp
  }

instance encodeJsonPerson :: EncodeJson Person where
  encodeJson = encodePerson

encodePerson :: Person -> Json
encodePerson (Person actor) = A.fromObject $ O.fromFoldable
  [ Tuple "@context" $ A.fromArray $ map A.fromString actor.context
  , Tuple "id" $ A.fromString actor.id
  , Tuple "inbox" $ A.fromString actor.inbox
  , Tuple "name" $ A.fromString actor.name
  , Tuple "outbox" $ A.fromString actor.outbox
  , Tuple "type" $ A.fromString "Person"
  , Tuple "publicKey" $ encodePublicKeyProp actor.publicKey
  ]

type PublicKeyProp = { id :: String, owner :: String, publicKeyPem :: String }

encodePublicKeyProp :: PublicKeyProp -> Json
encodePublicKeyProp { id, owner, publicKeyPem } = A.fromObject $ O.fromFoldable
  [ Tuple "id" $ A.fromString id
  , Tuple "owner" $ A.fromString owner
  , Tuple "publicKeyPem" $ A.fromString publicKeyPem
  ]

module Core.ActivityPub
  ( Activity(..)
  , ActivityType(..)
  , Person(..)
  , PublicKeyProp
  , fromActivityType
  , toActivityType
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut as A
import Data.Maybe (Maybe)
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
  [ Tuple "@context" $ encodeJson actor.context
  , Tuple "id" $ encodeJson actor.id
  , Tuple "inbox" $ encodeJson actor.inbox
  , Tuple "name" $ encodeJson actor.name
  , Tuple "outbox" $ encodeJson actor.outbox
  , Tuple "type" $ encodeJson "Person"
  , Tuple "publicKey" $ encodePublicKeyProp actor.publicKey
  ]

type PublicKeyProp = { id :: String, owner :: String, publicKeyPem :: String }

encodePublicKeyProp :: PublicKeyProp -> Json
encodePublicKeyProp { id, owner, publicKeyPem } = A.fromObject $ O.fromFoldable
  [ Tuple "id" $ encodeJson id
  , Tuple "owner" $ encodeJson owner
  , Tuple "publicKeyPem" $ encodeJson publicKeyPem
  ]

newtype OrderedCollection a = OrderedCollection
  { context :: Array String
  , first :: Maybe String
  , id :: String
  , orderedItems :: Maybe String
  , totalItems :: Int
  }

instance encodeJsonOrderedCollection :: EncodeJson a => EncodeJson (OrderedCollection a) where
  encodeJson = encodeOrderedCollection

encodeOrderedCollection :: forall a. EncodeJson a => OrderedCollection a -> Json
encodeOrderedCollection (OrderedCollection c) = A.fromObject $ O.fromFoldable
  [ Tuple "context" $ encodeJson c.context
  , Tuple "first" $ encodeJson c.first
  , Tuple "id" $ encodeJson c.id
  , Tuple "orderedItems" $ encodeJson c.orderedItems
  , Tuple "totalItems" $ encodeJson c.totalItems
  , Tuple "type" $ encodeJson "OrderedCollection"
  ]

newtype OrderedCollectionPage a = OrderedCollectionPage
  { context :: Array String
  , id :: String
  , next :: Maybe String
  , partOf :: String
  , prev :: Maybe String
  , orderedItems :: Array a
  , totalItems :: Int
  }

instance encodeJsonOrderedCollectionPage :: EncodeJson a => EncodeJson (OrderedCollectionPage a) where
  encodeJson = encodeOrderedCollectionPage

encodeOrderedCollectionPage :: forall a. EncodeJson a => OrderedCollectionPage a -> Json
encodeOrderedCollectionPage (OrderedCollectionPage p) = A.fromObject $ O.fromFoldable
  [ Tuple "context" $ encodeJson p.context
  , Tuple "id" $ encodeJson p.id
  , Tuple "next" $ encodeJson p.next
  , Tuple "partOf" $ encodeJson p.partOf
  , Tuple "prev" $ encodeJson p.prev
  , Tuple "orderedItems" $ encodeJson p.orderedItems
  , Tuple "totalItems" $ encodeJson p.totalItems
  , Tuple "type" $ encodeJson "OrderedCollectionPage"
  ]

data ActivityType = Accept | Create | Follow | Other String | Reject | Remove

toActivityType :: String -> ActivityType
toActivityType "Accept" = Accept
toActivityType "Create" = Create
toActivityType "Follow" = Follow
toActivityType "Reject" = Reject
toActivityType "Remove" = Remove
toActivityType t = Other t

fromActivityType :: ActivityType -> String
fromActivityType Accept = "Accept"
fromActivityType Create = "Create"
fromActivityType Follow = "Follow"
fromActivityType (Other t) = t
fromActivityType Reject = "Reject"
fromActivityType Remove = "Remove"

newtype Activity = Activity
  { activityBlob :: String
  , activityId :: String
  , activityType :: ActivityType
  , id :: Int
  }

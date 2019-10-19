module Core.WebFinger
  ( Link(..)
  , LinkType(..)
  , Jrd(..)
  , Uri
  , UriScheme(..)
  , parseUri
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as A
import Data.Array as Array
import Data.Either (Either)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Text.Parsing.Parser as P
import Text.Parsing.Parser.String as PS

newtype Jrd a = Jrd
  { aliases :: Array String
  , links :: Array Link
  , subject :: String
  }

instance encodeJsonJrd :: EncodeJson (Jrd a) where
  encodeJson = encodeJrd

encodeJrd :: forall a. Jrd a -> Json
encodeJrd (Jrd { aliases, links, subject }) = A.fromObject $ O.fromFoldable $
  [ Tuple "aliases" $ A.fromArray (map A.fromString aliases)
  , Tuple "links" $ A.fromArray (map encodeLink links)
  , Tuple "subject" $ A.fromString subject
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

type Uri =
  { host :: String
  , name :: String
  , raw :: String
  , scheme :: UriScheme
  }

data UriScheme = Acct | Http | Https

parseUri :: String -> Either P.ParseError Uri
parseUri s = P.runParser s (acctUriParser s)

type Parser a = P.Parser String a

acctUriParser :: String -> Parser Uri
acctUriParser raw = do
  _ <- PS.string "acct:"
  name <- fromCharArray <$> Array.some (PS.noneOf ['@'])
  _ <- PS.string "@"
  host <- fromCharArray <$> Array.some PS.anyChar
  _ <- PS.eof
  pure { host, name, raw, scheme: Acct }

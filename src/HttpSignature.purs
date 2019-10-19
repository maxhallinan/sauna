module HttpSignature
  ( Param(..)
  , ParseError(..)
  , SignatureParams
  , makeStringToSign
  , parseSignatureParams
  ) where

import Prelude

import Data.Array (some)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

type SignatureParams =
  { algorithm :: Maybe String
  , headers :: Maybe (Array String)
  , keyId :: String
  , signature :: String
  }

data ParseError
  = InvalidSyntax P.ParseError
  | MissingParam Param

data Param
  = Algorithm
  | Headers
  | KeyId
  | Signature

type Field = Tuple String String

parseSignatureParams :: String -> Either (Array ParseError) SignatureParams
parseSignatureParams = runParser >=> validateSignatureParams
  where runParser =
          flip P.runParser signatureParser
          >>> lmap (pure <<< InvalidSyntax)

makeStringToSign :: { reqMethod :: String, reqUrl :: String } -> Array { k :: String, v :: String } -> String
makeStringToSign reqPieces = intercalate "\\n" <<< map (makeLine reqPieces)
  where makeLine { reqMethod, reqUrl } { k, v } =
          if k == "(request-target)"
          then format k (reqMethod <> " " <> reqUrl)
          else format k v
        format k v = k <> ": " <> v

validateSignatureParams :: Map String String -> Either (Array ParseError) SignatureParams
validateSignatureParams fields = V.toEither (makeParams <$> validateKeyId fields <*> validateSignature fields)
  where makeParams keyId signature =
          { algorithm: M.lookup "algorithm" fields
          , headers: parseHeaders <$> M.lookup "headers" fields
          , keyId
          , signature
          }

validateSignature :: Map String String -> V (Array ParseError) String
validateSignature =
  M.lookup "signature"
  >>> maybe (missingParam Signature) pure

validateKeyId :: Map String String -> V (Array ParseError) String
validateKeyId =
  M.lookup "keyId"
  >>> maybe (missingParam KeyId) pure

missingParam :: forall a. Param -> V (Array ParseError) a
missingParam = V.invalid <<< pure <<< MissingParam

parseHeaders :: String -> Array String
parseHeaders = split (Pattern " ")

type Parser = P.Parser String

signatureParser :: Parser (Map String String)
signatureParser = do
  void $ S.string "Signature "
  fields <- C.sepBy field (S.char ',')
  S.eof
  pure $ M.fromFoldable fields

field :: Parser Field
field = do
  k <- nonEmptyStringOf anyCharNotSyntax
  void $ S.char '='
  v <- quoted $ nonEmptyStringOf anyCharNotSyntax
  pure $ Tuple k v

anyCharNotSyntax :: Parser Char
anyCharNotSyntax = S.satisfy (\c -> c /= '=' && c /= ',' && c /= '"')

nonEmptyStringOf :: Parser Char -> Parser String
nonEmptyStringOf p = fromCharArray <$> some p

quoted :: Parser String -> Parser String
quoted p = C.between (S.char '"') (S.char '"') p

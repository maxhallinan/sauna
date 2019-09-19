module SQLite3.Extra (queryDB) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import Effect.Uncurried as EU
import Foreign (Foreign)
import SQLite3 (DBConnection, Query, Param)

queryDB :: DBConnection -> Query -> Array Param -> Aff Foreign
queryDB conn query params = makeAff \cb ->
  mempty <$
    EU.runEffectFn5 _queryDB conn query params
      (EU.mkEffectFn1 $ cb <<< Left)
      (EU.mkEffectFn1 $ cb <<< Right)

foreign import _queryDB ::
  EU.EffectFn5
    DBConnection
    Query
    (Array Param)
    (EU.EffectFn1 Error Unit)
    (EU.EffectFn1 Foreign Unit)
    Unit

module Makkori.Extra (getQuery) where

import Effect (Effect)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Makkori (Request)

getQuery :: Request -> Effect Foreign
getQuery = EU.runEffectFn1 _getQuery

foreign import _getQuery :: EU.EffectFn1 Request Foreign

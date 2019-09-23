module Makkori.Extra 
  ( Router
  , delete
  , get
  , getHostname
  , getQuery
  , makeRouter
  , post
  , put
  , useRouter
  , useSubRouter
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Makkori (App, Handler, Path, Request)

getHostname :: Request -> Effect String
getHostname = EU.runEffectFn1 _getHostname

getQuery :: Request -> Effect Foreign
getQuery = EU.runEffectFn1 _getQuery

makeRouter :: Effect Router
makeRouter = _makeRouter

useRouter :: Path -> Router -> App -> Effect Unit
useRouter = EU.runEffectFn3 _useRouter

useSubRouter :: Path -> Router -> Router -> Effect Unit
useSubRouter = EU.runEffectFn3 _useSubRouter

delete :: Path -> Handler -> Router -> Effect Unit
delete = EU.runEffectFn3 _delete

get :: Path -> Handler -> Router -> Effect Unit
get = EU.runEffectFn3 _get

post :: Path -> Handler -> Router -> Effect Unit
post = EU.runEffectFn3 _post

put :: Path -> Handler -> Router -> Effect Unit
put = EU.runEffectFn3 _put

foreign import data Router :: Type
foreign import _getHostname :: EU.EffectFn1 Request String
foreign import _getQuery :: EU.EffectFn1 Request Foreign
foreign import _makeRouter :: Effect Router
foreign import _useRouter :: EU.EffectFn3 Path Router App Unit
foreign import _useSubRouter :: EU.EffectFn3 Path Router Router Unit
foreign import _delete :: EU.EffectFn3 Path Handler Router Unit
foreign import _get :: EU.EffectFn3 Path Handler Router Unit
foreign import _post :: EU.EffectFn3 Path Handler Router Unit
foreign import _put :: EU.EffectFn3 Path Handler Router Unit

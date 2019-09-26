module Makkori.Extra
  ( Router
  , delete
  , get
  , getHeaders
  , getHostname
  , getMethod
  , getOriginalUrl
  , getQuery
  , makeAccepts
  , makeJsonMiddleware
  , makeRouter
  , post
  , put
  , useRouter
  , useRouterMiddleware
  , useSubRouter
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Makkori (App, Handler, Middleware, Path, Request)

getHeaders :: Request -> Effect Foreign
getHeaders = EU.runEffectFn1 _getHeaders

getHostname :: Request -> Effect String
getHostname = EU.runEffectFn1 _getHostname

getMethod :: Request -> String
getMethod = _getMethod

getOriginalUrl :: Request -> String
getOriginalUrl = _getOriginalUrl

getQuery :: Request -> Effect Foreign
getQuery = EU.runEffectFn1 _getQuery

makeAccepts :: Request -> Array String -> Maybe String
makeAccepts req types = _makeAccepts Just Nothing req types

makeJsonMiddleware :: Array String -> Effect Middleware
makeJsonMiddleware = EU.runEffectFn1 _makeJsonMiddleware

makeRouter :: Effect Router
makeRouter = _makeRouter

useRouterMiddleware :: Path -> Middleware -> Router -> Effect Unit
useRouterMiddleware = EU.runEffectFn3 _useRouterMiddleware

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
foreign import _getHeaders :: EU.EffectFn1 Request Foreign
foreign import _getHostname :: EU.EffectFn1 Request String
foreign import _getMethod :: Request -> String
foreign import _getOriginalUrl :: Request -> String
foreign import _getQuery :: EU.EffectFn1 Request Foreign
foreign import _makeRouter :: Effect Router
foreign import _makeAccepts :: (String -> Maybe String) -> Maybe String -> Request -> Array String -> Maybe String
foreign import _makeJsonMiddleware :: EU.EffectFn1 (Array String) Middleware
foreign import _useRouter :: EU.EffectFn3 Path Router App Unit
foreign import _useRouterMiddleware :: EU.EffectFn3 Path Middleware Router Unit
foreign import _useSubRouter :: EU.EffectFn3 Path Router Router Unit
foreign import _delete :: EU.EffectFn3 Path Handler Router Unit
foreign import _get :: EU.EffectFn3 Path Handler Router Unit
foreign import _post :: EU.EffectFn3 Path Handler Router Unit
foreign import _put :: EU.EffectFn3 Path Handler Router Unit

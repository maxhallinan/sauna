module Server.Core 
  ( Handler
  , Method(..)
  , Path
  , Port(..)
  , Request
  , Response
  , Router
  , path
  , port
  ) where

import Effect.Aff (Aff)
import Data.Tuple (Tuple)
import Foreign (Foreign)
import Makkori as Makkori
import Makkori.Extra as Makkori.Extra

type Handler = Request -> Aff Response

data Method = Delete | Get | Post | Put

type Path = Makkori.Path

path :: String -> Path
path = Makkori.Path

newtype Port = Port Int

port :: Int -> Port
port = Port

type Request = { body :: Foreign, hostname :: String, params :: Foreign, query :: Foreign }

type Response = { body :: String, headers :: Array (Tuple String String), status :: Int }

type Router = Makkori.Extra.Router

module Server.Core 
  ( class Request
  , class Response
  , Method(..)
  , Path
  , Port
  , Router
  , fromOutput
  , toInput
  ) where

import Prelude

import Data.Tuple (Tuple)
import Foreign (Foreign)
import Makkori.Extra as Makkori.Extra

data Method = Delete | Get | Post | Put

type Path = String

type Port = Int

type Router = Makkori.Extra.Router

class Request input where
  toInput :: { body :: Foreign, params :: Foreign, query :: Foreign } -> input

class Response output where
  fromOutput :: output -> { body :: String , headers :: Array (Tuple String String), status :: Int }

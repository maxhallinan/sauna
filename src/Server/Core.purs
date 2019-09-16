module Server.Core 
  ( class Input
  , class Output
  , Method(..)
  , Path
  , Port
  , Router
  , fromOutput
  , toInput
  ) where

import Data.Tuple (Tuple)
import Foreign (Foreign)
import Makkori.Extra as Makkori.Extra

data Method = Delete | Get | Post | Put

type Path = String

type Port = Int

type Router = Makkori.Extra.Router

class Input input where
  toInput :: { body :: Foreign, params :: Foreign, query :: Foreign } -> input

class Output output where
  fromOutput :: output -> { body :: String , headers :: Array (Tuple String String), status :: Int }

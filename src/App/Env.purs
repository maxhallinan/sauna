module App.Env
  ( class Has
  , Env(..)
  , grab
  , obtain
  ) where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import SQLite3 (DBConnection)
import Server (Port)

newtype Env =
  Env { dbConn :: DBConnection
      , port :: Port
      }

class Has field env where
  obtain :: env -> field

instance hasDBConnection :: Has DBConnection Env where
  obtain (Env { dbConn }) = dbConn

instance hasPort :: Has Port Env where
  obtain (Env { port }) = port

grab :: forall field env m. MonadReader env m => Has field env => m field
grab = map obtain ask

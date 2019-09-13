module App.Env (Env(..)) where

import Prelude

import SQLite3 (DBConnection, queryDB)

type Env (m :: Type -> Type) = 
  { dbConn :: DBConnection 
  , port :: Int
  }

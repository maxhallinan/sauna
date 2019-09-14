module App.Env (Env(..)) where

import Prelude

import SQLite3 (DBConnection, queryDB)

type Env = 
  { dbConn :: DBConnection 
  , port :: Int
  }

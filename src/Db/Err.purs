module Db.Err (DbErr(..), fromSQLite3Err) where

import Effect.Exception (Error, name)
import Foreign (MultipleErrors)

data DbErr 
  = ConstraintViolation 
  | ExpectedOneRow
  | RecordNotFound 
  | ReadErr MultipleErrors 
  | UnknownDbErr

fromSQLite3Err :: Error -> DbErr
fromSQLite3Err err = 
  case name err of
    "SQLITE_CONSTRAINT" ->
      ConstraintViolation
    _ ->
      UnknownDbErr

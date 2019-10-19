module Handler.Account.Post (handlePost) where

import Prelude

import App.Env (class Has, Env)
import App.Err (Err)
import App.Err as Err
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Except (Except, withExcept)
import Control.Monad.Reader.Class (class MonadReader)
import Core.Account (Account)
import Crypto (generateRSAKeypair)
import Data.Either (either)
import Db.Account (getAccountByUsername, insertAccount)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign as F
import Foreign.Index as F.I
import Handler (class Input, runJsonHandler)
import SQLite3 (DBConnection)
import Server (Request, Response)

newtype Params = Params { username :: String }

instance inputParams :: Input Params where
  fromRequest = toParams

toParams :: Request -> Except Err Params
toParams req = do
  username <- readUsername req.body
  pure $ Params { username }

readUsername :: Foreign -> Except Err String
readUsername = withExcept toInvalidData <<< read
  where read = F.readString <=< F.I.readProp "username"
        toInvalidData _ = Err.invalidData fieldErrs
          where fieldErrs = [Err.fieldErr "username" Err.Required]

handlePost :: Env -> Request -> Aff Response
handlePost env = runJsonHandler env 201 handler

handler
  :: forall env m
   . Has DBConnection env
  => MonadReader env m
  => MonadAff m
  => MonadError Err m
  => MonadThrow Err m
  => Params
  -> m Account
handler (Params { username }) = do
  keypair <- liftEffect generateRSAKeypair
  unlessUsernameTaken $ insertAccount { privKey: keypair.private
                                      , pubKey: keypair.public
                                      , username
                                      }
  where
    unlessUsernameTaken action = do
      account <- try $ getAccountByUsername username
      either (const action) (const throwInvalidData) account
    throwInvalidData = throwError $ Err.invalidData fieldErrs
      where fieldErrs = [Err.fieldErr "username" Err.NotUnique ]

module Config (Config(..), DbFilename, Port, loadConfig, findSetting) where

import Prelude

import Data.Foldable (find)
import Data.Int as Int
import Data.Maybe (Maybe, maybe)
import Data.Tuple (fst, snd)
import Dotenv (Name, Setting, Settings, loadFile)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)

type DbFilename = String

type Port = Int

type Config =
  { dbFilename :: DbFilename
  , port :: Port
  }

makeConfig :: DbFilename -> Port -> Config
makeConfig dbFilename port = { dbFilename, port }

loadConfig :: Aff Config
loadConfig = do
  settings <- loadFile
  dbFilename <- getDbFilename settings
  port <- getPort settings
  pure $ makeConfig dbFilename port

getDbFilename :: Settings -> Aff DbFilename
getDbFilename = getValue "DB_FILENAME" readVal
  where readVal = pure <<< identity

getPort :: Settings -> Aff Port
getPort = getValue "PORT" readVal
  where readVal = Int.fromString

getValue
  :: forall a
   . Name
  -> (String -> Maybe a)
  -> Settings
  -> Aff a
getValue name toValue =
  maybe exception pure
  <<< (toValue
  <=< findSetting name)
  where exception = throwError $ error errMsg
        errMsg = makeErrMsg name

makeErrMsg :: Name -> String
makeErrMsg name = "Missing or invalid env variable: "  <> name

findSetting :: Name -> Settings -> Maybe String
findSetting name = snd <=< find (isName name)

isName :: Name -> Setting -> Boolean
isName name = eq name <<< fst

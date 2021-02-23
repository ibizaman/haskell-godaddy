{-# LANGUAGE LambdaCase #-}

-- |
module ConfigFile
  ( ConfigFile (..),
    ParsedConfigFile (..),
    defaultConfigFiles,
    parse,
  )
where

import Control.Exception (tryJust)
import Data.Foldable (asum)
import qualified Data.Ini.Config as Config
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Godaddy
import qualified System.IO.Error as Error

newtype ConfigFile = ConfigFile
  { apiKey :: Godaddy.APIKey
  }

data ParsedConfigFile = ParsedConfigFile
  { failures :: [(Text, String)],
    parsedConfigFile :: Maybe ConfigFile
  }

instance Semigroup ParsedConfigFile where
  ParsedConfigFile f Nothing <> ParsedConfigFile f' s' =
    ParsedConfigFile (f <> f') s'
  ParsedConfigFile f s <> ParsedConfigFile f' _ = ParsedConfigFile (f <> f') s

apikeyConfig :: Config.IniParser ConfigFile
apikeyConfig =
  ConfigFile
    <$> Config.section
      "CREDENTIALS"
      ( Godaddy.APIKey
          <$> Config.fieldOf "apikey" Config.string
          <*> Config.fieldOf "apisecret" Config.string
      )

parse :: NonEmpty Text -> IO (Either String ConfigFile)
parse = fmap asum . sequenceA . fmap parseFile
  where
    parseFile :: Text -> IO (Either String ConfigFile)
    parseFile file = do
      \case
        Error e -> do
          Left $ "error in file " <> T.unpack file <> ": " <> e
        FileContent f -> case Config.parseIniFile f apikeyConfig of
          Left failure -> do
            Left $ "error in file " <> T.unpack file <> ": " <> failure
          Right v -> do
            Right v
        <$> readFileSafe file

defaultConfigFiles :: NonEmpty Text
defaultConfigFiles = "godaddy.conf" :| ["/etc/godaddy/godaddy.conf"]

data File = Error String | FileContent Text

readFileSafe :: Text -> IO File
readFileSafe file =
  either Error (FileContent . T.pack)
    <$> tryJust handleException (readFile $ T.unpack file)
  where
    handleException :: IOError -> Maybe String
    handleException er
      | Error.isDoesNotExistError er = Just "does not exist"
      | Error.isPermissionError er = Just "permission error"
      | otherwise = Nothing

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
module Env
  ( Vars (..),
    getVars,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified System.Envy as Envy

data Vars = Vars
  { godaddyApiKey :: Text,
    godaddyApiSecret :: Text
  }
  deriving (Generic)

instance Envy.FromEnv Vars

getVars :: IO (Either String Vars)
getVars = f <$> Envy.decodeWithDefaults (Vars "" "")
  where
    -- We set the error message ourselves because the default error
    -- message is not clear.
    f = \case
      (Vars "" _) -> Left "Please set the GODADDY_API_KEY environment variable."
      (Vars _ "") ->
        Left "Please set the GODADDY_API_SECRET environment variable."
      v -> Right v

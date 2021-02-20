{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
module Env
  ( Vars (..),
    Endpoint (..),
    getVars,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Servant.Client as SC
import qualified System.Envy as Envy

data Vars = Vars
  { godaddyApiKey :: Text,
    godaddyApiSecret :: Text,
    godaddyCustomEndpoint :: Maybe Endpoint,
    godaddyTestEndpoint :: Bool
  }
  deriving (Generic)

instance Envy.FromEnv Vars

newtype Endpoint = Endpoint {unEndpoint :: SC.BaseUrl}

instance Envy.Var Endpoint where
  toVar = SC.showBaseUrl . unEndpoint
  fromVar = fmap Endpoint . SC.parseBaseUrl

getVars :: IO (Either String Vars)
getVars = f <$> Envy.decodeWithDefaults (Vars "" "" Nothing False)
  where
    -- We set the error message ourselves because the default error
    -- message is not clear.
    f = \case
      (Vars "" _ _ _) ->
        Left "Please set the GODADDY_API_KEY environment variable."
      (Vars _ "" _ _) ->
        Left "Please set the GODADDY_API_SECRET environment variable."
      v -> Right v

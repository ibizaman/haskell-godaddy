{-# LANGUAGE DeriveGeneric #-}

-- |
module Env
  ( Env,
    apiKey,
    endpoint,
    APIKey (..),
    getEnv,
  )
where

import GHC.Generics (Generic)
import qualified Godaddy
import qualified Servant.Client as SC
import qualified System.Envy as Envy

data Env = Env
  { godaddyApiCredentials :: Maybe APIKey,
    godaddyCustomEndpoint :: Maybe Endpoint,
    godaddyTestEndpoint :: Bool
  }
  deriving (Generic)

instance Envy.FromEnv Env

newtype Endpoint = Endpoint {unEndpoint :: SC.BaseUrl}

instance Envy.Var Endpoint where
  toVar = SC.showBaseUrl . unEndpoint
  fromVar = fmap Endpoint . SC.parseBaseUrl

newtype APIKey = APIKey {unAPIKey :: Godaddy.APIKey}

instance Envy.Var APIKey where
  toVar = Godaddy.printApiKey . unAPIKey
  fromVar = fmap APIKey . Godaddy.parseApiKey

getEnv :: IO Env
getEnv = Envy.decodeWithDefaults (Env Nothing Nothing False)

endpoint :: Env -> SC.BaseUrl
endpoint env = case (godaddyTestEndpoint env, godaddyCustomEndpoint env) of
  (False, Just custom) -> Env.unEndpoint custom
  (True, _) -> Godaddy.testBaseUrl
  _ -> Godaddy.defaultBaseUrl

apiKey :: Env -> Maybe Godaddy.APIKey
apiKey = fmap unAPIKey . godaddyApiCredentials

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- |
module IP where

import Data.Aeson (FromJSON)
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant ((:>))
import qualified Servant as S
import qualified Servant.Client as SC

newtype IP = IP {ip :: Text}
  deriving (Generic)

instance FromJSON IP

defaultIpifyEndpoint :: SC.BaseUrl
defaultIpifyEndpoint = SC.BaseUrl SC.Https "api.ipify.org" 443 ""

type IpifyAPI =
  S.QueryParam "format" Format :> S.Get '[S.JSON] IP

data Format = FormatJSON
  deriving (Generic)

instance S.ToHttpApiData Format where
  toUrlPiece FormatJSON = "json"

newtype IpifyClient = IpifyClient
  { getIP :: SC.ClientM IP
  }

mkClient :: Maybe Format -> IpifyClient
mkClient format = IpifyClient {..}
  where
    client = SC.client (Proxy :: Proxy IpifyAPI)

    getIP = client format

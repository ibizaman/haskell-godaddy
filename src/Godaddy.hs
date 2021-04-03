{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module  : Godaddy
-- Description : Call Godaddy API through servant functions
--
-- The `Godaddy` module provides access to the Godaddy API through a
-- 'Servant' type and 'Servant.Client' functions.
--
-- The following snippet shows how to set the Servant 'Servant.Client'
-- up.
--
-- > import Control.Monad (void)
-- > import Data.Either.Combinators (mapLeft)
-- > import qualified Godaddy
-- > import Network.HTTP.Client.TLS (newTlsManager)
-- > import qualified Servant.Client as SC
-- >
-- > run :: SC.BaseUrl -> SC.ClientM a -> IO (Either Godaddy.Error a)
-- > run endpoint query = do
-- >   manager' <- newTlsManager
-- >   mapLeft Godaddy.parseError
-- >     <$> SC.runClientM query (SC.mkClientEnv manager' endpoint)
--
-- The following snippet shows how to make queries thanks to the run
-- function we just defined above.
--
-- > {-# LANGUAGE RecordWildCards #-}
-- >
-- > endpoint :: SC.BaseUrl
-- > endpoint = Godaddy.defaultBaseUrl
-- >
-- > apiKey :: Godaddy.APIKey
-- > apiKey = Godaddy.APIKey "KEY" "SECRET"
-- >
-- > getDomainsQuery :: Godaddy.APIKey -> SC.ClientM [Godaddy.Domain]
-- > getDomainsQuery apiKey = do
-- >   let Godaddy.Client {..} = Godaddy.mkClient apiKey
-- >   getDomains
-- >
-- > main :: IO ()
-- > main = void $ run endpoint (getDomainsQuery apiKey)
module Godaddy
  ( -- * Setup API
    defaultBaseUrl,
    testBaseUrl,
    APIKey (..),
    parseApiKey,
    printApiKey,

    -- * Client functions to call the Godaddy API

    -- | https://developer.godaddy.com/getstarted
    mkClient,
    Client (..),
    DomainClient (..),
    RecordsTypeClient (..),
    RecordsTypeNameClient (..),
    DomainId (..),
    Domain (..),
    Record (..),
    RecordType (..),

    -- * Interpret Error from Godaddy
    parseError,
    Error (..),
    Status (..),
    GodaddyError (..),
    ErrorField (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (fieldLabelModifier, omitNothingFields),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified HumanReadable as HR
import qualified Network.HTTP.Types as HTTPTypes
import Servant ((:<|>) (..), (:>))
import qualified Servant as S
import qualified Servant.Client as SC
import qualified Utils

-- | The API key to connect to Godaddy. Remember, the secret should
-- stay secret, don't save it not encrypted to disk.
data APIKey = APIKey
  { -- | The "key" part of the API key.
    key :: Text,
    -- | The "secret" part of the API key.
    secret :: Text
  }

-- | Parses a "KEY:SECRET" string to an APIKey.
parseApiKey :: String -> Maybe APIKey
parseApiKey = fmap (uncurry Godaddy.APIKey . bimap T.pack T.pack) . Utils.splitStringOnLastChar ':'

-- | Prints an APIKey in the "KEY:SECRET" format.
printApiKey :: APIKey -> String
printApiKey APIKey {..} = T.unpack $ key <> ":" <> secret

-- | Represent the APIKey as the header "sso-key KEY:SECRET". This is
-- transparent when using a servant 'Servant.Client'.
instance S.ToHttpApiData APIKey where
  toUrlPiece (APIKey key secret) = "sso-key " <> key <> ":" <> secret

-- | Default 'SC.BaseUrl' for the prod Godaddy endpoint "api.godaddy.com".
defaultBaseUrl :: SC.BaseUrl
defaultBaseUrl = SC.BaseUrl SC.Https "api.godaddy.com" 443 ""

-- | 'SC.BaseUrl' for the test Godaddy endpoint "api.ote-godaddy.com".
testBaseUrl :: SC.BaseUrl
testBaseUrl = SC.BaseUrl SC.Https "api.ote-godaddy.com" 443 ""

-- | The Domain ID of a Godaddy domain.
newtype DomainId = DomainId Int
  deriving (Generic)

instance Show DomainId where
  show (DomainId id') = show id'

instance S.ToHttpApiData DomainId where
  toUrlPiece (DomainId domainId) = S.toUrlPiece domainId

instance FromJSON DomainId

-- | A Godaddy domain.
data Domain = Domain
  { -- | Domain created at date.
    createdAt :: UTCTime,
    -- | Domain name.
    domain :: Text,
    -- | Domain ID.
    domainId :: DomainId,
    -- | If the domain is renewed automatically.
    renewAuto :: Bool,
    -- | The renewal deadline.
    renewDeadline :: Maybe UTCTime,
    -- | Domain status.
    status :: Text
  }
  deriving (Show, Generic)

instance FromJSON Domain

instance HR.HumanReadable Domain where
  name Domain {domain} = T.unpack domain
  body Domain {..} =
    [ "DomainID:  " <> show domainId,
      "Status:    " <> T.unpack status,
      "CreatedAt: " <> show createdAt,
      "RenewAuto: " <> show renewAuto
    ]

instance HR.HumanReadable [Domain] where
  name _ = "Domains:"
  body domains = map HR.printForHumans domains

-- | A Godaddy record.
data Record = Record
  { -- | Either an IP or a domain name.
    recordData :: Text,
    recordName :: Text,
    recordPort :: Maybe Int,
    recordPriority :: Maybe Int,
    recordProtocol :: Maybe Text,
    recordService :: Maybe Text,
    recordTtl :: Maybe Int,
    recordType :: RecordType,
    recordWeight :: Maybe Int
  }
  deriving (Show, Generic)

instance HR.HumanReadable Record where
  name Record {recordName} = T.unpack recordName
  body Record {..} =
    catMaybes
      [ fmap ((<>) "Type:     " . show) (Just recordType),
        fmap ((<>) "Protocol: " . show) recordProtocol,
        fmap ((<>) "Data:     " . show) (Just recordData),
        fmap ((<>) "Service:  " . show) recordService,
        fmap ((<>) "Port:     " . show) recordPort,
        fmap ((<>) "Priority: " . show) recordPriority,
        fmap ((<>) "Weight:   " . show) recordWeight,
        fmap ((<>) "TTL:      " . show) recordTtl
      ]

instance HR.HumanReadable [Record] where
  name _ = "Records:"
  body domains = map HR.printForHumans domains

removePrefix :: String -> String -> String
removePrefix prefix str = case drop (length prefix) str of
  (x : xs) -> toLower x : xs
  xs -> xs

instance FromJSON Record where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = removePrefix "record"
          }
      )

instance ToJSON Record where
  toJSON =
    genericToJSON
      ( defaultOptions
          { fieldLabelModifier = removePrefix "record",
            omitNothingFields = True
          }
      )

data RecordType = A | AAAA | CNAME | MX | NS | SOA | SRV | TXT
  deriving (Show, Eq, Generic)

instance FromJSON RecordType

instance ToJSON RecordType

instance S.ToHttpApiData RecordType where
  toUrlPiece A = "A"
  toUrlPiece AAAA = "AAAA"
  toUrlPiece CNAME = "CNAME"
  toUrlPiece MX = "MX"
  toUrlPiece NS = "NS"
  toUrlPiece SOA = "SOA"
  toUrlPiece SRV = "SRV"
  toUrlPiece TXT = "TXT"

-- | Top-level queries. Use the 'mkDomainClient' function to access
-- domain-level functions.
data Client = Client
  { -- | Get all domains
    getDomains :: SC.ClientM [Domain],
    -- | Return a 'DomainClient' object making requests for the given
    -- domain.
    mkDomainClient :: Text -> DomainClient
  }

type ClientAPI =
  "v1" :> S.Header "Authorization" APIKey :> DomainsAPI

type DomainsAPI =
  "domains"
    :> ( S.Get '[S.JSON] [Domain]
           :<|> DomainAPI
       )

-- | Queries for a domain. 'mkDomainClient' returns this object. Use
-- the 'mkRecordsWithTypeClient' function to access functions
-- operating on all records of a given 'RecordType'.
data DomainClient = DomainClient
  { -- | Get information about the domain.
    getDomain :: SC.ClientM Domain,
    -- | Get all records from a domain.
    getRecords :: SC.ClientM [Record],
    -- | Add records to a domain. Fails if the records are duplicate.
    addRecords :: [Record] -> SC.ClientM S.NoContent,
    -- | Replace records of a domain by the given ones.
    replaceRecords :: [Record] -> SC.ClientM S.NoContent,
    -- | Return a 'RecordsTypeClient' object making requests for the
    -- records of a given 'RecordType'.
    mkRecordsWithTypeClient :: RecordType -> RecordsTypeClient
  }

type DomainAPI =
  S.Capture "domain" Text
    :> ( S.Get '[S.JSON] Domain
           :<|> RecordsAPI
       )

type RecordsAPI =
  "records"
    :> ( S.Get '[S.JSON] [Record]
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Patch '[S.OctetStream] S.NoContent
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Put '[S.OctetStream] S.NoContent
           :<|> RecordsTypeAPI
       )

-- | Queries for all records of a given domain and 'RecordType'.
-- 'mkRecordsWithTypeClient' returns this object. Use the
-- 'mkRecordsWithTypeNameClient' function to access functions
-- operating on all records of a given type and a given name.
data RecordsTypeClient = RecordsTypeClient
  { -- | Get all records of the given 'RecordType'.
    getRecordsWithType :: SC.ClientM [Record],
    -- | Replace all records of the given 'RecordType' with those given.
    replaceRecordsWithType :: [Record] -> SC.ClientM S.NoContent,
    -- | Delete all records of the given 'RecordType'.
    deleteRecordsWithType :: SC.ClientM S.NoContent,
    -- | Return a 'RecordsTypeNameClient' object making request for
    -- the records of a given 'RecordType' and a given name.
    mkRecordsWithTypeNameClient :: Text -> RecordsTypeNameClient
  }

type RecordsTypeAPI =
  S.Capture "recordType" RecordType
    :> ( S.Get '[S.JSON] [Record]
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Put '[S.JSON] S.NoContent
           :<|> RecordsTypeNameAPI
       )

-- | Queries for all records of a given domain, 'RecordType' and name.
-- 'mkRecordsWithTypeNameClient' returns this object.
data RecordsTypeNameClient = RecordsTypeNameClient
  { -- | Get all records of the given 'RecordType' and name.
    getRecordsWithTypeName :: SC.ClientM [Record],
    -- | Replace all records of the given 'RecordType' and name with those given.
    replaceRecordsWithTypeName :: [Record] -> SC.ClientM S.NoContent,
    -- | Delete all records of the given 'RecordType' and name.
    deleteRecordsWithTypeName :: SC.ClientM S.NoContent
  }

type RecordsTypeNameAPI =
  S.Capture "name" Text
    :> ( S.Get '[S.JSON] [Record]
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Put '[S.JSON] S.NoContent
       )

-- | Return a convenience 'Client' object containing functions to call the Godaddy API.
--
-- Example usage:
--
-- > {-# LANGUAGE RecordWildCards #-}
-- >
-- > getRecordsQuery :: APIKey -> Text -> SC.ClientM [Record]
-- > getRecordsQuery apiKey domain = do
-- >   let Client {..} = mkClient apiKey
-- >       DomainClient {..} = mkDomainClient domain
-- >   getRecords
mkClient :: APIKey -> Client
mkClient apiKey = Client {..}
  where
    client = SC.client (Proxy :: Proxy ClientAPI)

    getDomains :<|> domainClient = client (Just apiKey)

    mkDomainClient domainId = DomainClient {..}
      where
        getDomain :<|> getRecords :<|> addRecords :<|> replaceRecords :<|> recordsWithTypeClient = domainClient domainId

        mkRecordsWithTypeClient recordType = RecordsTypeClient {..}
          where
            getRecordsWithType :<|> replaceRecordsWithType :<|> recordsWithTypeNameClient = recordsWithTypeClient recordType

            deleteRecordsWithType = getRecords >>= replaceRecords . removeRecordsWithType recordType

            removeRecordsWithType :: RecordType -> [Record] -> [Record]
            removeRecordsWithType delRecordType = filter (\Record {recordType = rt} -> rt /= delRecordType)

            mkRecordsWithTypeNameClient name = RecordsTypeNameClient {..}
              where
                getRecordsWithTypeName :<|> replaceRecordsWithTypeName = recordsWithTypeNameClient name
                deleteRecordsWithTypeName =
                  getRecords >>= replaceRecords . removeRecordsWithNameType name recordType

                removeRecordsWithNameType :: Text -> RecordType -> [Record] -> [Record]
                removeRecordsWithNameType delName delRecordType = filter (\Record {recordName, recordType = rt} -> recordName /= delName || rt /= delRecordType)

-- | The error returned by Godaddy.
data Error
  = -- | An error returned by Godaddy. For example for a missing field
    -- or an invalid IP field.
    Error Status GodaddyError
  | -- | A request or response could not be decoded.
    DecodeError Status String String
  | -- | An unexpected error.
    OtherError Status String
  | -- | A network error.
    ConnectionError String
  deriving (Generic, Show)

instance FromJSON Error

-- | Status of an API call.
data Status = Status
  {statusCode :: Int, statusMessage :: String}
  deriving (Generic, Show)

instance FromJSON Status

-- | An error returned by Godaddy if a request is invalid. For example
-- for a missing field or an invalid IP field.
data GodaddyError = GodaddyError
  { errorCode :: String,
    -- | The fields which produced an error.
    errorFields :: Maybe [ErrorField],
    errorMessage :: String,
    -- | In case of rate limiting by Godaddy.
    errorRetryAfterSec :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON GodaddyError where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = removePrefix "error"
          }
      )

data ErrorField = ErrorField
  { fieldCode :: String,
    fieldMessage :: String,
    fieldPath :: String,
    fieldPathRelated :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON ErrorField where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = removePrefix "field"
          }
      )

-- | Parse JSON error coming from Godaddy.
parseError :: SC.ClientError -> Error
parseError clientError =
  case clientError of
    (SC.FailureResponse _ SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody}) ->
      let p = Aeson.eitherDecode responseBody
          s = Status statusCode $ show statusMessage
       in either (DecodeError s $ show responseBody) (Error s) p
    SC.DecodeFailure failure SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      let s = Status statusCode $ show statusMessage
       in DecodeError s (T.unpack failure) $ show responseBody
    SC.UnsupportedContentType mediaType SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      let s = Status statusCode $ show statusMessage
       in OtherError s $ "unsupported media type: \"" <> show mediaType <> "\", reponse body: " <> show responseBody
    SC.InvalidContentTypeHeader SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      let s = Status statusCode $ show statusMessage
       in OtherError s $ show responseBody
    SC.ConnectionError exception ->
      ConnectionError $ show exception

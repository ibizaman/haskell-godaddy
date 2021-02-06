{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- |
module Godaddy
  ( Godaddy (..),
    APIKey (..),
    APIType (..),
    DomainId (..),
    Domain (..),
    Record (..),
    RecordType (..),
    mkBaseUrl,
    Client (..),
    DomainClient (..),
    RecordsTypeClient (..),
    RecordsTypeNameClient (..),
    mkClient,
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

data APIKey = APIKey Text Text

newtype Godaddy = Godaddy
  { apiKey :: APIKey
  }

instance S.ToHttpApiData APIKey where
  toUrlPiece (APIKey key secret) = "sso-key " <> key <> ":" <> secret

data APIType = Prod | Test | Debug String Int

mkBaseUrl :: APIType -> SC.BaseUrl
mkBaseUrl Prod = SC.BaseUrl SC.Https "api.godaddy.com" 443 ""
mkBaseUrl Test = SC.BaseUrl SC.Https "api.ote-godaddy.com" 443 ""
mkBaseUrl (Debug host port) = SC.BaseUrl SC.Http host port ""

newtype DomainId = DomainId Int
  deriving (Generic)

instance Show DomainId where
  show (DomainId id') = show id'

instance S.ToHttpApiData DomainId where
  toUrlPiece (DomainId domainId) = S.toUrlPiece domainId

instance FromJSON DomainId

data Domain = Domain
  { createdAt :: UTCTime,
    domain :: Text,
    domainId :: DomainId,
    renewAuto :: Bool,
    renewDeadline :: Maybe UTCTime,
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

data Record = Record
  { recordData :: Text,
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

data Client = Client
  { getDomains :: SC.ClientM [Domain],
    mkDomainClient :: Text -> DomainClient
  }

type ClientAPI =
  "v1" :> S.Header "Authorization" APIKey :> DomainsAPI

type DomainsAPI =
  "domains"
    :> ( S.Get '[S.JSON] [Domain]
           :<|> DomainAPI
       )

data DomainClient = DomainClient
  { getDomain :: SC.ClientM Domain,
    getRecords :: SC.ClientM [Record],
    addRecords :: [Record] -> SC.ClientM S.NoContent,
    replaceRecords :: [Record] -> SC.ClientM S.NoContent,
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

data RecordsTypeClient = RecordsTypeClient
  { getRecordsWithType :: SC.ClientM [Record],
    replaceRecordsWithType :: [Record] -> SC.ClientM S.NoContent,
    deleteRecordsWithType :: SC.ClientM S.NoContent,
    mkRecordsWithTypeNameClient :: Text -> RecordsTypeNameClient
  }

type RecordsTypeAPI =
  S.Capture "recordType" RecordType
    :> ( S.Get '[S.JSON] [Record]
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Put '[S.JSON] S.NoContent
           :<|> RecordsTypeNameAPI
       )

data RecordsTypeNameClient = RecordsTypeNameClient
  { getRecordsWithTypeName :: SC.ClientM [Record],
    replaceRecordsWithTypeName :: [Record] -> SC.ClientM S.NoContent,
    deleteRecordsWithTypeName :: SC.ClientM S.NoContent
  }

type RecordsTypeNameAPI =
  S.Capture "name" Text
    :> ( S.Get '[S.JSON] [Record]
           :<|> S.ReqBody '[S.JSON] [Record] :> S.Put '[S.JSON] S.NoContent
       )

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

data Error
  = Error Status GodaddyError
  | DecodeError Status String String
  | OtherError Status String
  | ConnectionError String
  deriving (Generic, Show)

instance FromJSON Error

data Status = Status
  {statusCode :: Int, statusMessage :: String}
  deriving (Generic, Show)

instance FromJSON Status

data GodaddyError = GodaddyError
  { errorCode :: String,
    errorFields :: [ErrorField],
    errorMessage :: String,
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

parseError :: SC.ClientError -> Error
parseError clientError =
  case clientError of
    (SC.FailureResponse _ SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody}) ->
      let p = Aeson.eitherDecode responseBody
          s = Status statusCode $ show statusMessage
       in either (DecodeError s $ show responseBody) (Error s) p
    SC.DecodeFailure failure SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      -- let p = Aeson.eitherDecode responseBody
      --     s = Status statusCode $ show statusMessage
      --  in either (DecodeError s $ "2" <> show responseBody) (DecodeError s $ T.unpack failure) p
      let s = Status statusCode $ show statusMessage
       in DecodeError s (T.unpack failure) $ show responseBody
    SC.UnsupportedContentType mediaType SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      -- let p = Aeson.eitherDecode responseBody
      --     s = Status statusCode $ show statusMessage
      --  in either (DecodeError s $ "3" <> show responseBody) (\x -> OtherError s $ "unsupported media type: \"" <> show mediaType <> "\": " <> x) p
      let s = Status statusCode $ show statusMessage
       in OtherError s $ "unsupported media type: \"" <> show mediaType <> "\", reponse body: " <> show responseBody
    SC.InvalidContentTypeHeader SC.Response {responseStatusCode = HTTPTypes.Status statusCode statusMessage, responseBody} ->
      -- let p = Aeson.eitherDecode responseBody
      --     s = Status statusCode $ show statusMessage
      --  in either (DecodeError s $ "4" <> show responseBody) (OtherError s) p
      let s = Status statusCode $ show statusMessage
       in OtherError s $ show responseBody
    SC.ConnectionError exception ->
      ConnectionError $ show exception

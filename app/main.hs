{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import qualified Args
import qualified ConfigFile
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text
  ( Text,
    isPrefixOf,
  )
import qualified Data.Text as T
import qualified Env
import qualified Godaddy
import HumanReadable
  ( HumanReadable,
    printForHumans,
  )
import qualified IP
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Options.Applicative.Help as Help
import qualified Servant as S
import qualified Servant.Client as SC
import System.Environment (getProgName)

main :: IO ()
main = do
  Args.getArgs >>= \args -> do
    env <- Env.getEnv
    configFile <-
      eitherToMaybe
        <$> ConfigFile.parse
          (maybe ConfigFile.defaultConfigFiles (:| []) $ Args.configFile args)
    let apiKey =
          Args.credentials args
            <|> Env.apiKey env
            <|> (ConfigFile.apiKey <$> configFile)
    let endpoint = Env.endpoint env
    case apiKey of
      Nothing -> do
        progName <- getProgName
        Help.putDoc $
          "Please set the Godaddy API credentials. Run "
            <> Help.underline (Help.text progName <> " credentials")
            <> " for more info."
            <> Help.hardline
      Just apiKey' -> command endpoint apiKey' (Args.command args)
  where
    command _ _ Args.ConfigHelp = return ()
    command endpoint apiKey Args.Domains =
      run endpoint (getDomainsQuery apiKey) >>= display
    command endpoint apiKey (Args.Records domain) =
      run endpoint (getRecordsQuery apiKey $ Args.unDomain domain) >>= display
    command endpoint apiKey Args.Create {..} =
      run
        endpoint
        ( addRecordsQuery
            apiKey
            (Args.unDomain createDomain)
            [ Godaddy.Record
                { recordName = createName,
                  recordData = createData,
                  recordType = createType,
                  recordPort = createPort,
                  recordPriority = createPriority,
                  recordProtocol = createProtocol,
                  recordService = createService,
                  recordTtl = createTtl,
                  recordWeight = createWeight
                }
            ]
        )
        >>= displayErr
    command endpoint apiKey (Args.Delete domain recordType name) =
      run
        endpoint
        ( deleteRecordsWithTypeNameQuery
            apiKey
            (Args.unDomain domain)
            recordType
            name
        )
        >>= displayErr
    command endpoint apiKey (Args.Servers domain) =
      run
        endpoint
        (getRecordsWithTypeQuery apiKey (Args.unDomain domain) Godaddy.A)
        >>= display
    command endpoint apiKey (Args.ServerAdd domain serverIps) =
      forM_ serverIps $ \(Args.ServerIP server ip) ->
        run
          endpoint
          ( addRecordsQuery
              apiKey
              (Args.unDomain domain)
              [ Godaddy.Record
                  { recordName = Args.unServer server,
                    recordData = Args.unIP ip,
                    recordType = Godaddy.A,
                    recordPort = Nothing,
                    recordPriority = Nothing,
                    recordProtocol = Nothing,
                    recordService = Nothing,
                    recordTtl = Nothing,
                    recordWeight = Nothing
                  }
              ]
          )
          >>= displayErr
    command endpoint apiKey (Args.ServerDelete domain servers) =
      forM_ servers $ \server ->
        run
          endpoint
          ( deleteRecordsWithTypeNameQuery
              apiKey
              (Args.unDomain domain)
              Godaddy.A
              (Args.unServer server)
          )
          >>= displayErr
    command endpoint apiKey (Args.ServerReplace domain serverIps) = do
      forM_ serverIps $ \(Args.ServerIP server _ip) ->
        run
          endpoint
          ( deleteRecordsWithTypeNameQuery
              apiKey
              (Args.unDomain domain)
              Godaddy.A
              (Args.unServer server)
          )
          >>= displayErr
      forM_ serverIps $ \(Args.ServerIP server ip) ->
        run
          endpoint
          ( addRecordsQuery
              apiKey
              (Args.unDomain domain)
              [ Godaddy.Record
                  { recordName = Args.unServer server,
                    recordData = Args.unIP ip,
                    recordType = Godaddy.A,
                    recordPort = Nothing,
                    recordPriority = Nothing,
                    recordProtocol = Nothing,
                    recordService = Nothing,
                    recordTtl = Nothing,
                    recordWeight = Nothing
                  }
              ]
          )
          >>= displayErr
    command endpoint apiKey (Args.Subdomains domain server) =
      run
        endpoint
        (getRecordsWithTypeQuery apiKey (Args.unDomain domain) Godaddy.CNAME)
        >>= display
          . fmap (filterRecordsByDataPrefix $ Args.unServer server)
    command endpoint apiKey (Args.SubdomainAdd domain server subdomains) =
      forM_ subdomains $ \subdomain ->
        let serverName = case Args.unServer server of
              "@" -> "@"
              s -> s <> "." <> Args.unDomain domain
         in run
              endpoint
              ( addRecordsQuery
                  apiKey
                  (Args.unDomain domain)
                  [ Godaddy.Record
                      { recordName = Args.unSubdomain subdomain,
                        recordData = serverName,
                        recordType = Godaddy.CNAME,
                        recordPort = Nothing,
                        recordPriority = Nothing,
                        recordProtocol = Nothing,
                        recordService = Nothing,
                        recordTtl = Nothing,
                        recordWeight = Nothing
                      }
                  ]
              )
              >>= displayErr
    command endpoint apiKey (Args.SubdomainDelete domain _server subdomains) =
      forM_ subdomains $ \subdomain ->
        run
          endpoint
          ( deleteRecordsWithTypeNameQuery
              apiKey
              (Args.unDomain domain)
              Godaddy.CNAME
              (Args.unSubdomain subdomain)
          )
          >>= displayErr
    command endpoint apiKey (Args.Dyndns domain servers) =
      run' IP.defaultIpifyEndpoint getIPQuery >>= \case
        Left err -> putStrLn $ "Could not get IP: " <> show err
        Right (IP.IP ip) -> forM_ servers $ \server ->
          run
            endpoint
            ( replaceRecordsWithTypeNameQuery
                apiKey
                (Args.unDomain domain)
                Godaddy.A
                (Args.unServer server)
                Godaddy.Record
                  { recordName = Args.unServer server,
                    recordData = ip,
                    recordType = Godaddy.A,
                    recordPort = Nothing,
                    recordPriority = Nothing,
                    recordProtocol = Nothing,
                    recordService = Nothing,
                    recordTtl = Nothing,
                    recordWeight = Nothing
                  }
            )
            >>= \e -> do
              displayErr'
                ( Just $
                    "while updating server '"
                      <> Args.unServer server
                      <> "': "
                )
                e
    command endpoint apiKey (Args.Certbot (Args.CertbotDNS01AuthHook authSecret) domain) =
      run
        endpoint
        ( addRecordsQuery
            apiKey
            (Args.unDomain domain)
            [ Godaddy.Record
                { recordName = "_acme-challenge." <> Args.unDomain domain,
                  recordData = Args.unAuthSecret authSecret,
                  recordType = Godaddy.TXT,
                  recordPort = Nothing,
                  recordPriority = Nothing,
                  recordProtocol = Nothing,
                  recordService = Nothing,
                  recordTtl = Nothing,
                  recordWeight = Nothing
                }
            ]
        )
        >>= displayErr
    command endpoint apiKey (Args.Certbot Args.CertbotDNS01CleanupHook domain) =
      run
        endpoint
        ( deleteRecordsWithTypeNameQuery
            apiKey
            (Args.unDomain domain)
            Godaddy.TXT
            ("_acme-challenge." <> Args.unDomain domain)
        )
        >>= displayErr

getDomainsQuery :: Godaddy.APIKey -> SC.ClientM [Godaddy.Domain]
getDomainsQuery apiKey = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
  getDomains

getRecordsQuery :: Godaddy.APIKey -> Text -> SC.ClientM [Godaddy.Record]
getRecordsQuery apiKey domain = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
      Godaddy.DomainClient {..} = mkDomainClient domain
  getRecords

addRecordsQuery ::
  Godaddy.APIKey -> Text -> [Godaddy.Record] -> SC.ClientM S.NoContent
addRecordsQuery apiKey domain records = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
      Godaddy.DomainClient {..} = mkDomainClient domain
  addRecords records

getRecordsWithTypeQuery ::
  Godaddy.APIKey -> Text -> Godaddy.RecordType -> SC.ClientM [Godaddy.Record]
getRecordsWithTypeQuery apiKey domain recordType = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
      Godaddy.DomainClient {..} = mkDomainClient domain
      Godaddy.RecordsTypeClient {..} = mkRecordsWithTypeClient recordType
  getRecordsWithType

filterRecordsByDataPrefix :: Text -> [Godaddy.Record] -> [Godaddy.Record]
filterRecordsByDataPrefix prefix =
  filter (\Godaddy.Record {recordData} -> prefix `isPrefixOf` recordData)

replaceRecordsWithTypeNameQuery ::
  Godaddy.APIKey ->
  Text ->
  Godaddy.RecordType ->
  Text ->
  Godaddy.Record ->
  SC.ClientM S.NoContent
replaceRecordsWithTypeNameQuery apiKey domain recordType name record = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
      Godaddy.DomainClient {..} = mkDomainClient domain
      Godaddy.RecordsTypeClient {..} = mkRecordsWithTypeClient recordType
      Godaddy.RecordsTypeNameClient {..} = mkRecordsWithTypeNameClient name
  replaceRecordsWithTypeName [record]

deleteRecordsWithTypeNameQuery ::
  Godaddy.APIKey ->
  Text ->
  Godaddy.RecordType ->
  Text ->
  SC.ClientM S.NoContent
deleteRecordsWithTypeNameQuery apiKey domain recordType name = do
  let Godaddy.Client {..} = Godaddy.mkClient apiKey
      Godaddy.DomainClient {..} = mkDomainClient domain
      Godaddy.RecordsTypeClient {..} = mkRecordsWithTypeClient recordType
      Godaddy.RecordsTypeNameClient {..} = mkRecordsWithTypeNameClient name
  deleteRecordsWithTypeName

getIPQuery :: SC.ClientM IP.IP
getIPQuery = do
  let IP.IpifyClient {..} = IP.mkClient (Just IP.FormatJSON)
  getIP

run' :: SC.BaseUrl -> SC.ClientM a -> IO (Either SC.ClientError a)
run' endpoint query = do
  manager' <- newTlsManager
  SC.runClientM query (SC.mkClientEnv manager' endpoint)

run :: SC.BaseUrl -> SC.ClientM a -> IO (Either Godaddy.Error a)
run endpoint query = mapLeft Godaddy.parseError <$> run' endpoint query

displayErr :: Either Godaddy.Error a -> IO ()
displayErr = displayErr' Nothing

displayErr' :: Maybe Text -> Either Godaddy.Error a -> IO ()
displayErr' _ (Right _) = return ()
displayErr' prefix (Left err) =
  let prefix' = T.unpack $ fromMaybe "" prefix
   in case err of
        Godaddy.Error Godaddy.Status {statusCode, statusMessage} e ->
          putStrLn $
            prefix'
              <> "got an error from Godaddy: ["
              <> show statusCode
              <> "] "
              <> statusMessage
              <> ":\n"
              <> printError e
          where
            printError :: Godaddy.GodaddyError -> String
            printError Godaddy.GodaddyError {..} =
              "["
                <> errorCode
                <> "] "
                <> errorMessage
                <> maybe "" (\e' -> ":\n" <> printFields e') errorFields

            printFields :: [Godaddy.ErrorField] -> String
            printFields = List.intercalate "\n" . map printField

            printField :: Godaddy.ErrorField -> String
            printField Godaddy.ErrorField {..} =
              "  field \"" <> fieldPath <> "\": " <> fieldMessage
        Godaddy.DecodeError Godaddy.Status {statusCode, statusMessage} e decodeError ->
          putStrLn $
            prefix'
              <> "got an error \""
              <> decodeError
              <> "\" while decoding a status code from Godaddy ["
              <> show statusCode
              <> "] "
              <> statusMessage
              <> ": "
              <> e
        Godaddy.ConnectionError e ->
          putStrLn $
            prefix'
              <> "got a connection error while talking with Godaddy: "
              <> e
        Godaddy.OtherError Godaddy.Status {statusCode, statusMessage} e ->
          putStrLn $
            prefix'
              <> "got an error status code from Godaddy ["
              <> show statusCode
              <> "] "
              <> statusMessage
              <> ": "
              <> e

display :: HumanReadable a => Either Godaddy.Error a -> IO ()
display (Left err) = displayErr $ Left err
display (Right x) = putStrLn $ printForHumans x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b

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
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text
  ( Text,
    isPrefixOf,
  )
import qualified Env
import qualified Godaddy
import HumanReadable
  ( HumanReadable,
    printForHumans,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import qualified Servant as S
import qualified Servant.Client as SC

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
      Nothing -> putStrLn "Please set the Godaddy API credentials."
      Just apiKey' -> domainArgs endpoint apiKey' (Args.domainSubcommand args)
  where
    domainArgs endpoint apiKey Args.DomainsList =
      run endpoint (getDomainsQuery apiKey) >>= display
    domainArgs endpoint apiKey (Args.Domain domain s) =
      serverArgs endpoint apiKey domain s

    serverArgs endpoint apiKey domain Args.ServersList =
      run endpoint (getRecordsWithTypeQuery apiKey domain Godaddy.A) >>= display
    serverArgs endpoint apiKey domain Args.AllRecords =
      run endpoint (getRecordsQuery apiKey domain) >>= display
    serverArgs endpoint apiKey domain (Args.ServerAdd server ip) =
      run
        endpoint
        ( addRecordsQuery
            apiKey
            domain
            [ Godaddy.Record
                { recordName = server,
                  recordData = ip,
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
    serverArgs endpoint apiKey domain (Args.ServerDelete server) =
      run endpoint (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.A server)
        >>= displayErr
    serverArgs endpoint apiKey domain (Args.ServerReplace server ip) =
      run endpoint (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.A server)
        >> run
          endpoint
          ( addRecordsQuery
              apiKey
              domain
              [ Godaddy.Record
                  { recordName = server,
                    recordData = ip,
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
    serverArgs endpoint apiKey domain (Args.Server server sd) =
      subdomainArgs endpoint apiKey domain server sd

    subdomainArgs endpoint apiKey domain server Args.SubdomainsList =
      run endpoint (getRecordsWithTypeQuery apiKey domain Godaddy.CNAME)
        >>= display
          . fmap (filterRecordsByDataPrefix server)
    subdomainArgs endpoint apiKey domain server (Args.SubdomainAdd subdomain) =
      let serverName = case server of
            "@" -> "@"
            s -> s <> "." <> domain
       in run
            endpoint
            ( addRecordsQuery
                apiKey
                domain
                [ Godaddy.Record
                    { recordName = subdomain,
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
    subdomainArgs endpoint apiKey domain _server (Args.SubdomainDelete subdomain) =
      run
        endpoint
        (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.CNAME subdomain)
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

run :: SC.BaseUrl -> SC.ClientM a -> IO (Either Godaddy.Error a)
run endpoint query = do
  manager' <- newTlsManager
  mapLeft Godaddy.parseError
    <$> SC.runClientM query (SC.mkClientEnv manager' endpoint)

displayErr :: Either Godaddy.Error a -> IO ()
displayErr (Right _) = return ()
displayErr (Left err) = case err of
  Godaddy.Error Godaddy.Status {statusCode, statusMessage} e ->
    putStrLn $
      "Got an error from Godaddy: ["
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
      "Got an error \""
        <> decodeError
        <> "\" while decoding a status code from Godaddy ["
        <> show statusCode
        <> "] "
        <> statusMessage
        <> ": "
        <> e
  Godaddy.ConnectionError e ->
    putStrLn $ "Got a connection error while talking with Godaddy: " <> e
  Godaddy.OtherError Godaddy.Status {statusCode, statusMessage} e ->
    putStrLn $
      "Got an error status code from Godaddy ["
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

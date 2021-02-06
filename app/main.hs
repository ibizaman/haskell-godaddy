{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import qualified Args
import qualified Data.List as List
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
  Args.getArgs >>= \case
    Args.DomainSubcommand a ->
      Env.getVars >>= \case
        Left err -> putStrLn err
        Right Env.Vars {..} -> do
          let apiKey = Godaddy.APIKey godaddyApiKey godaddyApiSecret
          domainArgs apiKey a
  where
    domainArgs apiKey Args.DomainsList = run (getDomainsQuery apiKey) >>= display
    domainArgs apiKey (Args.Domain domain s) = serverArgs apiKey domain s

    serverArgs apiKey domain Args.ServersList =
      run (getRecordsWithTypeQuery apiKey domain Godaddy.A) >>= display
    serverArgs apiKey domain Args.AllRecords =
      run (getRecordsQuery apiKey domain) >>= display
    serverArgs apiKey domain (Args.ServerAdd server ip) =
      run
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
    serverArgs apiKey domain (Args.ServerDelete server) =
      run (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.A server)
        >>= displayErr
    serverArgs apiKey domain (Args.ServerReplace server ip) =
      run (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.A server)
        >> run
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
    serverArgs apiKey domain (Args.Server server sd) =
      subdomainArgs apiKey domain server sd

    subdomainArgs apiKey domain server Args.SubdomainsList =
      run (getRecordsWithTypeQuery apiKey domain Godaddy.CNAME)
        >>= display
          . fmap
            (filterRecordsByDataPrefix server)
    subdomainArgs apiKey domain server (Args.SubdomainAdd subdomain) =
      let serverName = case server of
            "@" -> "@"
            s -> s <> "." <> domain
       in run
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
    subdomainArgs apiKey domain _server (Args.SubdomainDelete subdomain) =
      run (deleteRecordsWithTypeNameQuery apiKey domain Godaddy.CNAME subdomain)
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

run :: SC.ClientM a -> IO (Either Godaddy.Error a)
run query = do
  manager' <- newTlsManager
  mapLeft Godaddy.parseError
    <$> SC.runClientM
      query
      ( SC.mkClientEnv
          manager'
          (Godaddy.mkBaseUrl $ Godaddy.Debug "127.0.0.1" 8080)
      )

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
          <> ":\n"
          <> printFields errorFields

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

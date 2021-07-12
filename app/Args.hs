{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
module Args
  ( Args (..),
    Command (..),
    Domain (..),
    Server (..),
    IP (..),
    ServerIP (..),
    Subdomain (..),
    getArgs,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.List as List
import Data.List.NonEmpty
  ( NonEmpty,
    some1,
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Godaddy
import Options.Applicative
  ( Alternative ((<|>)),
    (<**>),
  )
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as P
import qualified Utils

data Args = Args
  { configFile :: Maybe Text,
    credentials :: Maybe Godaddy.APIKey,
    command :: Command
  }

newtype Domain = Domain {unDomain :: Text}

newtype Server = Server {unServer :: Text}

newtype IP = IP {unIP :: Text}

data ServerIP = ServerIP Server IP

newtype Subdomain = Subdomain {unSubdomain :: Text}

newtype AuthSecret = AuthSecret {unAuthSecret :: Text}

data Command
  = ConfigHelp
  | Domains
  | Records Domain
  | Create
      { createDomain :: Domain,
        createType :: Godaddy.RecordType,
        createName :: Text,
        createData :: Text,
        createPort :: Maybe Int,
        createPriority :: Maybe Int,
        createProtocol :: Maybe Text,
        createService :: Maybe Text,
        createTtl :: Maybe Int,
        createWeight :: Maybe Int
      }
  | Delete Domain Godaddy.RecordType Text
  | Servers Domain
  | ServerAdd Domain (NonEmpty ServerIP)
  | ServerDelete Domain (NonEmpty Server)
  | ServerReplace Domain (NonEmpty ServerIP)
  | Subdomains Domain Server
  | SubdomainAdd Domain Server (NonEmpty Subdomain)
  | SubdomainDelete Domain Server (NonEmpty Subdomain)
  | Dyndns Domain (NonEmpty Server)

argsParser :: Opts.Parser Args
argsParser = Args <$> configFileParser <*> credentialsParser <*> commandParser

configFileParser :: Opts.Parser (Maybe Text)
configFileParser =
  Opts.optional $
    Opts.option
      Opts.str
      (Opts.long "config" <> Opts.metavar "CONFIGFILE" <> Opts.showDefault)

credentialsParser :: Opts.Parser (Maybe Godaddy.APIKey)
credentialsParser =
  Opts.optional $
    Opts.option
      (Opts.maybeReader Godaddy.parseApiKey)
      (Opts.long "credentials" <> Opts.metavar "KEY:SECRET")

recordTypeParser :: Opts.ReadM Godaddy.RecordType
recordTypeParser =
  Opts.str @String >>= \case
    "A" -> return Godaddy.A
    "AAAA" -> return Godaddy.AAAA
    "CNAME" -> return Godaddy.CNAME
    "MX" -> return Godaddy.MX
    "NS" -> return Godaddy.NS
    "SOA" -> return Godaddy.SOA
    "SRV" -> return Godaddy.SRV
    "TXT" -> return Godaddy.TXT
    _ -> Opts.readerError "Accepted types are: A, AAAA, CNAME, MX, NS, SOA, SRV and TXT."

commandParser :: Opts.Parser Command
commandParser =
  Opts.hsubparser
    ( Opts.command
        "domains"
        (Opts.info (pure Domains) (Opts.progDesc "List all domains"))
        <> Opts.command
          "records"
          ( Opts.info
              (Records <$> domainParser)
              (Opts.progDesc "List all records of a domain")
          )
        <> Opts.command
          "create"
          ( Opts.info
              ( Create
                  <$> domainParser
                    <*> Opts.argument recordTypeParser (Opts.metavar "RECORDTYPE")
                    <*> Opts.argument Opts.str (Opts.metavar "NAME")
                    <*> Opts.argument Opts.str (Opts.metavar "DATA")
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "port" <> Opts.metavar "PORT"))
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "priority" <> Opts.metavar "PRIORITY"))
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "protocol" <> Opts.metavar "PROTOCOL"))
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "service" <> Opts.metavar "SERVICE"))
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "ttl" <> Opts.metavar "TTL"))
                    <*> Opts.optional (Opts.option Opts.auto (Opts.long "weight" <> Opts.metavar "WEIGHT"))
              )
              (Opts.progDesc "Create a record on a domain")
          )
        <> Opts.command
          "delete"
          ( Opts.info
              ( Delete
                  <$> domainParser
                    <*> Opts.argument recordTypeParser (Opts.metavar "RECORDTYPE")
                    <*> Opts.argument Opts.str (Opts.metavar "NAME")
              )
              (Opts.progDesc "Delete a record by type and name from a domain")
          )
        <> Opts.commandGroup "General commands:"
    )
    <|> Opts.hsubparser
      ( Opts.command
          "servers"
          ( Opts.info
              serversParser
              ( Opts.progDesc "Manage servers of the domain"
                  <> Opts.footerDoc
                    ( Just $
                        "Request to a "
                          <> P.underline "server"
                          <> " are answered by a physical server reachable by an IP."
                          <> " A "
                          <> P.underline "server"
                          <> " is implemented by an A record."
                    )
              )
          )
          <> Opts.command
            "subdomains"
            ( Opts.info
                subdomainsParser
                ( Opts.progDesc "Manage subdomains pointing to servers"
                    <> Opts.footerDoc
                      ( Just $
                          "Requests to a "
                            <> P.underline "subdomain"
                            <> " are answered by a "
                            <> P.underline "server"
                            <> ". A "
                            <> P.underline "subdomain"
                            <> " is implemented by a CNAME record pointing to an A record."
                      )
                )
            )
          <> Opts.command
            "dyndns"
            ( Opts.info
                dyndnsParser
                (Opts.progDesc "Update servers with current external IP")
            )
          <> Opts.commandGroup "Opinionated commands:"
      )
    <|> Opts.hsubparser
      ( Opts.command
          "config"
          ( Opts.info
              (showAlwaysHelp ConfigHelp)
              ( Opts.progDesc "Print help about config file"
                  <> Opts.footerDoc
                    ( Just $
                        "All commands require the credentials to be set, either in the "
                          <> P.underline "config file"
                          <> ", in the environment variable "
                          <> P.underline "GODADDY_API_CREDENTIALS"
                          <> " or using the "
                          <> P.underline "--credentials"
                          <> " argument. "
                          <> "The latter two expect the credentials to be given in a "
                          <> P.underline "KEY:SECRET"
                          <> " format."
                          <> P.hardline
                          <> P.hardline
                          <> "If a path to a filename is given to the "
                          <> P.underline "--config"
                          <> " argument, the config file is read there. "
                          <> "When no such argument is given, the config file is searched first under the current directory "
                          <> P.underline "./godaddy.conf"
                          <> " then under "
                          <> P.underline "/etc/godaddy/godaddy.conf"
                          <> P.hardline
                          <> P.hardline
                          <> "The config file is composed of a unique [CREDENTIALS] section:"
                          <> P.hardline
                          <> P.hardline
                          <> P.indent
                            2
                            ( "[CREDENTIALS]"
                                <> P.hardline
                                <> "apikey=KEY"
                                <> P.hardline
                                <> "apisecret=SECRET"
                            )
                          <> P.hardline
                          <> "The KEY and SECRET should not be quoted."
                    )
              )
          )
          <> Opts.commandGroup "Help commands:"
      )

domainParser :: Opts.Parser Domain
domainParser = Domain <$> Opts.argument Opts.str (Opts.metavar "DOMAIN")

serverParser :: Opts.Parser Server
serverParser = Server <$> Opts.argument Opts.str (Opts.metavar "SERVER")

serverIPParser :: Opts.Parser ServerIP
serverIPParser =
  Opts.argument
    ( uncurry ServerIP
        . bimap (Server . T.pack) (IP . T.pack)
        <$> Opts.eitherReader parseServerIP
    )
    (Opts.metavar "SERVER:IP")
  where
    parseServerIP =
      maybeToEither "expected format is SERVER:IP"
        . Utils.splitStringOnLastChar ':'

    maybeToEither :: a -> Maybe b -> Either a b
    maybeToEither _ (Just b) = Right b
    maybeToEither a Nothing = Left a

subdomainParser :: Opts.Parser Subdomain
subdomainParser =
  Subdomain <$> Opts.argument Opts.str (Opts.metavar "SUBDOMAIN")

serversParser :: Opts.Parser Command
serversParser =
  Opts.hsubparser
    ( Opts.command
        "list"
        ( Opts.info
            (Servers <$> domainParser)
            ( Opts.progDescDoc
                (Just $ "List all " <> P.underline "servers" <> " of the DOMAIN.")
            )
        )
        <> Opts.command
          "add"
          ( Opts.info
              (ServerAdd <$> domainParser <*> some1 serverIPParser)
              ( Opts.progDescDoc
                  ( Just $ "Add a " <> P.underline "server" <> " pointing to an IP."
                  )
                  <> Opts.footer
                    ( "Multiple A records with the same name can exist if they point to different IPs."
                        <> " To create a server handling requests to the DOMAIN directly, use @:IP (a literal \"at\")."
                    )
              )
          )
        <> Opts.command
          "delete"
          ( Opts.info
              (ServerDelete <$> domainParser <*> some1 serverParser)
              ( Opts.progDescDoc (Just $ "Delete a " <> P.underline "server" <> ".")
                  <> Opts.footer "Do nothing if it does not exist."
              )
          )
        <> Opts.command
          "replace"
          ( Opts.info
              (ServerReplace <$> domainParser <*> some1 serverIPParser)
              ( Opts.progDescDoc
                  ( Just $
                      "Replace all "
                        <> P.underline "servers"
                        <> " with new ones pointing to the given IP."
                  )
                  <> Opts.footer "Add it if does not exist."
              )
          )
    )

subdomainsParser :: Opts.Parser Command
subdomainsParser =
  Opts.hsubparser
    ( Opts.command
        "list"
        ( Opts.info
            (Subdomains <$> domainParser <*> serverParser)
            ( Opts.progDesc
                "List all CNAME records for the DOMAIN pointing to the given SERVER."
            )
        )
        <> Opts.command
          "add"
          ( Opts.info
              ( SubdomainAdd
                  <$> domainParser
                  <*> serverParser
                  <*> some1 subdomainParser
              )
              (Opts.progDesc "Add a CNAME record pointing to the given SERVER.")
          )
        <> Opts.command
          "delete"
          ( Opts.info
              ( SubdomainDelete
                  <$> domainParser
                  <*> serverParser
                  <*> some1 subdomainParser
              )
              (Opts.progDesc "Delete a CNAME record pointing to the given SERVER.")
          )
    )

authSecretParser :: Opts.Parser AuthSecret
authSecretParser =
  AuthSecret <$> Opts.argument Opts.str (Opts.metavar "AUTHSECRET")

dyndnsParser :: Opts.Parser Command
dyndnsParser =
  Dyndns
    <$> Opts.argument (Domain <$> Opts.str) (Opts.metavar "DOMAIN")
    <*> some1 (Opts.argument (Server <$> Opts.str) (Opts.metavar "SERVER"))

getArgs :: IO Args
getArgs = Opts.customExecParser p opts
  where
    p = Opts.prefs Opts.showHelpOnEmpty
    opts =
      Opts.info
        (argsParser <**> Opts.helper)
        ( Opts.header "Opinionated godaddy cli"
            <> Opts.progDescDoc
              ( Just $
                  mintercalate
                    (P.hardline <> P.hardline)
                    [ "This program allows you to manage Godaddy records.",
                      "All commands require the credentials to be set, run the "
                        <> P.underline "config"
                        <> " command for more information.",
                      "The Godaddy endpoint can be modified with the "
                        <> P.underline "GODADDY_CUSTOM_ENDPOINT"
                        <> " environment variable."
                        <> " This is useful mostly to debug the requests made to Godaddy."
                    ]
              )
        )

mintercalate :: P.Doc -> [P.Doc] -> P.Doc
mintercalate separator = mconcat . List.intersperse separator

showAlwaysHelp :: a -> Opts.Parser a
showAlwaysHelp p =
  p <$ Opts.argument (Opts.eitherReader Left) (Opts.metavar "")

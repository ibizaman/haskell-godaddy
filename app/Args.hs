-- |
module Args
  ( Args (..),
    Domains (..),
    Servers (..),
    Subdomains (..),
    getArgs,
  )
where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
  ( Alternative ((<|>)),
    (<**>),
  )
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as P
import qualified Utils

newtype Args = DomainSubcommand Domains

data Domains
  = DomainsList
  | Domain Text Servers

data Servers
  = ServersList
  | AllRecords
  | ServerAdd Text Text
  | ServerDelete Text
  | ServerReplace Text Text
  | Server Text Subdomains

data Subdomains
  = SubdomainsList
  | SubdomainAdd Text
  | SubdomainDelete Text

argsParser :: Opts.Parser Args
argsParser = DomainSubcommand <$> domainsParser

domainsParser :: Opts.Parser Domains
domainsParser =
  pure DomainsList
    <|> ( Domain
            <$> Opts.argument Opts.str (Opts.metavar "DOMAIN")
            <*> serversParser
        )

serversParser :: Opts.Parser Servers
serversParser =
  pure ServersList
    <|> Opts.flag' AllRecords (Opts.long "records")
    <|> ( (\(server, ip) -> ServerAdd (T.pack server) (T.pack ip))
            <$> Opts.option
              (Opts.eitherReader parseServerIP)
              (Opts.long "add" <> Opts.metavar "SERVER:IP")
        )
    <|> ( ServerDelete
            <$> Opts.strOption (Opts.long "delete" <> Opts.metavar "SERVER")
        )
    <|> ( (\(server, ip) -> ServerReplace (T.pack server) (T.pack ip))
            <$> Opts.option
              (Opts.eitherReader parseServerIP)
              (Opts.long "replace" <> Opts.metavar "SERVER:IP")
        )
    <|> ( Server
            <$> Opts.argument Opts.str (Opts.metavar "SERVER")
            <*> subdomainsParser
        )
  where
    parseServerIP =
      maybeToEither "expected format is SERVER:IP"
        . Utils.splitStringOnLastChar ':'

    maybeToEither :: a -> Maybe b -> Either a b
    maybeToEither _ (Just b) = Right b
    maybeToEither a Nothing = Left a

subdomainsParser :: Opts.Parser Subdomains
subdomainsParser =
  pure SubdomainsList
    <|> ( SubdomainAdd
            <$> Opts.strOption (Opts.long "add" <> Opts.metavar "SUBDOMAIN")
        )
    <|> ( SubdomainDelete
            <$> Opts.strOption (Opts.long "delete" <> Opts.metavar "SUBDOMAIN")
        )

getArgs :: IO Args
getArgs = Opts.execParser opts
  where
    opts =
      Opts.info
        (argsParser <**> Opts.helper)
        ( Opts.header "Opinionated godaddy cli"
            <> Opts.progDescDoc
              ( Just $
                  mintercalate
                    (P.hardline <> P.hardline)
                    [ "godaddy allows you to create and delete A records and CNAME records.",
                      "All commands require the "
                        <> P.underline "GODADDY_API_KEY"
                        <> " and "
                        <> P.underline "GODADDY_API_SECRET"
                        <> " environment variables to be set.",
                      cmdlineExplanation
                        "godaddy"
                        ""
                        "Without arguments, print all domains.",
                      cmdlineExplanation
                        "godaddy"
                        "DOMAIN [--add SERVER:IP | --replace SERVER:IP | --delete SERVER | --records]"
                        ( "Print all servers under the given DOMAIN. "
                            <> "Optionally add a server, replace all servers, delete all records of a server or show all records for the given DOMAIN."
                            <> P.line
                            <> "A server is an A record associated with an IP. "
                            <> "To create a server handling requests to the DOMAIN directly, use --add @:IP (a literal \"at\"). "
                            <> "For requests to SERVER.DOMAIN, use --add SERVER:IP. "
                            <> "Multiple records with the same SERVER name can be added as long as you assign different IPs."
                        ),
                      cmdlineExplanation
                        "godaddy"
                        "DOMAIN SERVER [--add SUBDOMAIN | --delete SUBDOMAIN]"
                        ( "Print all subdomains for the given SERVER.DOMAIN."
                            <> "Optionally add or delete a subdomain. "
                            <> "No check is made if no corresponding SERVER.DOMAIN record exists on add."
                            <> P.line
                            <> "A subdomain is a CNAME pointing to a given SERVER.DOMAIN."
                        )
                    ]
              )
        )

    cmdlineExplanation name cmd expl =
      P.underline (P.bold name)
        <> " "
        <> P.bold cmd
        <> P.hardline
        <> P.indent 4 expl

mintercalate :: P.Doc -> [P.Doc] -> P.Doc
mintercalate separator = mconcat . List.intersperse separator

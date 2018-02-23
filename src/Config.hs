module Config
  ( Options(..)
  , MilliSeconds(..)
  , parseOptionsFromEnvAndCli
  ) where

import Control.Applicative ((<*>), (<|>))
import Data.Monoid ((<>))

import Options.Applicative (value, long, auto, option, metavar, help, flag,
                            str, argument, many, strOption, (<**>))

import qualified Options.Applicative as OptParse
import qualified Options.Applicative.Builder.Internal as OptParse
import qualified Text.Read as Read

-- ! Type alias for enviornment variables, used for readability in this module.
type EnvVar = (String, String)

-- ! Newtype wrapper for millisecond values.
newtype MilliSeconds = MilliSeconds { unMilliSeconds :: Int }
  deriving (Eq, Show)

-- | @Options@ contains all the configuration we support in vaultenv. It is
-- used in our @Main@ module to specify behaviour.
data Options = Options
  { oVaultHost       :: String
  , oVaultPort       :: Int
  , oVaultToken      :: String
  , oSecretFile      :: FilePath
  , oCmd             :: String
  , oArgs            :: [String]
  , oNoConnectTls    :: Bool
  , oNoValidateCerts :: Bool
  , oNoInheritEnv    :: Bool
  , oRetryBaseDelay  :: MilliSeconds
  , oRetryAttempts   :: Int
  } deriving (Eq, Show)

-- | Behaviour flags that we allow users to set via environment variables.
-- This type is internal to the workings of this module. It is used as an
-- intermediate value to get optparse-applicative to play nice with environment
-- variables as used for behavior flags.
data EnvFlags = EnvFlags
  { efNoConnectTls :: Bool
  , efNoValidateCerts :: Bool
  , efNoInheritEnv :: Bool
  }

-- | Parse program options from the command line and the process environment.
parseOptionsFromEnvAndCli :: [EnvVar] -> IO Options
parseOptionsFromEnvAndCli envVars =
  let envFlags = parseEnvFlags envVars
      parser = optionsParserWithInfo envFlags envVars
  in OptParse.execParser parser

-- | Parses behaviour flags from a list of environment variables. If an
-- environment variable corresponding to the flag is set to @"true"@ or
-- @"false"@, we use that as the default on the corresponding CLI option.
--
-- If these variables aren't present, we default to @False@. We print an error
-- if they're set to anything else than @"true"@ or @"false"@.
parseEnvFlags :: [EnvVar] -> EnvFlags
parseEnvFlags envVars
  = EnvFlags
  { efNoConnectTls = lookupEnvFlag "VAULTENV_NO_CONNECT_TLS"
  , efNoValidateCerts = lookupEnvFlag "VAULTENV_NO_VALIDATE_CERTS"
  , efNoInheritEnv = lookupEnvFlag "VAULTENV_NO_INHERIT_ENV"
  }
  where
    lookupEnvFlag key =
      case lookup key envVars of
        Just "true" -> True
        Just "false" -> False
        Nothing -> False
        _ -> errorWithoutStackTrace $ "[ERROR]: Invalid value for environment variable " ++ key

-- | This function adds metadata to the @Options@ parser so it can be used with
-- execParser.
optionsParserWithInfo :: EnvFlags -> [EnvVar] -> OptParse.ParserInfo Options
optionsParserWithInfo envFlags localEnvVars =
  OptParse.info
    (optionsParser envFlags localEnvVars <**> OptParse.helper)
    (OptParse.fullDesc <> OptParse.header "vaultenv - run programs with secrets from HashiCorp Vault")

-- | Parser for our CLI options. Seems intimidating, but is straightforward
-- once you know about applicative parsing patterns. We construct a parser for
-- @Options@ by combining parsers for parts of the record.
--
-- Toy example to illustrate the pattern:
--
-- @
--     OptionRecord <$> parser1 <*> parser2 <*> parser3
-- @
--
-- Here, the parser for @OptionRecord@ is the combination of parsers of its
-- internal fields.
--
-- The parsers get constructed by using different combinators from the
-- @Options.Applicative@ module. Here, we use @strOption@, @option@, @argument@
-- and flag. These take a @Mod@ value, which can specify how to parse an
-- option. These @Mod@ values have monoid instances and are composed as such.
--
-- So in our example above, we could have the following definition for
-- @parser1@:
--
-- @
--    parser1 = strOption $ long "my-option" <> value "default"
-- @
--
-- And have the thing compile if the first member of @OptionRecord@ would have
-- type @String@.
--
-- Another thing that should be noted is the way we use the Alternate instance
-- of Parsers in order to provide dual options. The @noConnectTls@,
-- @noValidateCerts@ and @noInheritEnv@ parsers constist of two parts. One for
-- the affirmative and one for the negative case. This is required so
-- we can override environment variables, since optparse-applicative does not
-- provide an abstraction for this by itself.
optionsParser :: EnvFlags -> [EnvVar] -> OptParse.Parser Options
optionsParser envFlags envVars = Options
    <$> host
    <*> port
    <*> token
    <*> secretsFile
    <*> cmd
    <*> cmdArgs
    <*> (noConnectTls    <|> connectTls)
    <*> (noValidateCerts <|> validateCerts)
    <*> (noInheritEnv    <|> inheritEnv)
    <*> baseDelayMs
    <*> retryAttempts
  where
    host
      =  strOption
      $  long "host"
      <> metavar "HOST"
      <> value "localhost"
      <> readValueFromEnv "VAULT_HOST" envVars
      <> help ("Vault host, either an IP address or DNS name. Defaults to localhost. " ++
               "Also configurable via VAULT_HOST.")
    port
      =  option auto
      $  long "port"
      <> metavar "PORT"
      <> readValueFromEnvWithDefault "VAULT_PORT" 8200 envVars
      <> help "Vault port. Defaults to 8200. Also configurable via VAULT_PORT."
    token
      =  strOption
      $  long "token"
      <> metavar "TOKEN"
      <> readValueFromEnv "VAULT_TOKEN" envVars
      <> help "Token to authenticate to Vault with. Also configurable via VAULT_TOKEN."
    secretsFile
      =  strOption
      $  long "secrets-file"
      <> metavar "FILENAME"
      <> readValueFromEnv "VAULTENV_SECRETS_FILE" envVars
      <> help ("Config file specifying which secrets to request. Also configurable " ++
               "via VAULTENV_SECRETS_FILE." )
    cmd
      =  argument str
      $  metavar "CMD"
      <> help "command to run after fetching secrets"
    cmdArgs
      =  many $ argument str
      (  metavar "ARGS..."
      <> help "Arguments to pass to CMD, defaults to nothing")
    noConnectTls
      =  flag (efNoConnectTls envFlags) True
      $  long "no-connect-tls"
      <> help ("Don't use TLS when connecting to Vault. Default: use TLS. Also " ++
              "configurable via VAULTENV_NO_CONNECT_TLS.")
    connectTls
      =  flag (efNoConnectTls envFlags) False
      $  long "connect-tls"
      <> help ("Always connect to Vault via TLS. Default: use TLS. Can be used " ++
                "to override VAULTENV_NO_CONNECT_TLS.")
    noValidateCerts
      =  flag (efNoValidateCerts envFlags) True
      $  long "no-validate-certs"
      <> help ("Don't validate TLS certificates when connecting to Vault. Default: " ++
              "validate certs. Also configurable via VAULTENV_NO_VALIDATE_CERTS.")
    validateCerts
      =  flag (efNoValidateCerts envFlags) False
      $  long "validate-certs"
      <> help ("Always validate TLS certificates when connecting to Vault. Default: " ++
                "validate certs. Can be used to override VAULTENV_NO_CONNECT_TLS.")
    noInheritEnv
      =  flag (efNoInheritEnv envFlags) True
      $  long "no-inherit-env"
      <> help ("Don't merge the parent environment with the secrets file. Default: " ++
              "merge environments. Also configurable via VAULTENV_NO_INHERIT_ENV.")
    inheritEnv
      =  flag (efNoInheritEnv envFlags) False
      $  long "inherit-env"
      <> help ("Always merge the parent environment with the secrets file. Default: " ++
                "merge environments. Can be used to override VAULTENV_NO_INHERIT_ENV.")
    baseDelayMs
      =  MilliSeconds <$> (option auto
      $  long "retry-base-delay-milliseconds"
      <> metavar "MILLISECONDS"
      <> readValueFromEnvWithDefault "VAULTENV_RETRY_BASE_DELAY_MS" 40 envVars
      <> help ("Base delay for vault connection retrying. Defaults to 40ms. " ++
                "Also configurable via VAULTENV_RETRY_BASE_DELAY_MS."))
    retryAttempts
      =  option auto
      $  long "retry-attempts"
      <> metavar "NUM"
      <> readValueFromEnvWithDefault "VAULTENV_RETRY_ATTEMPTS" 9 envVars
      <> help "Maximum number of vault connection retries. Defaults to 9"

-- ! Attempt to parse an optparse default value modifier from a list of
-- environment variables. This function returns an empty option modifier in
-- case the environment variable is missing or does not parse.
readValueFromEnv :: (Read a, OptParse.HasValue f)
                 => String
                 -> [EnvVar]
                 -> OptParse.Mod f a
readValueFromEnv var envVars =
  let parseResult = lookup var envVars >>= Read.readMaybe
  in foldMap value parseResult

-- | Attempt to parse an optparse default value modifier from the process
-- environment with a default.
readValueFromEnvWithDefault :: (Read a, OptParse.HasValue f)
                            => String
                            -> a
                            -> [EnvVar]
                            -> OptParse.Mod f a
readValueFromEnvWithDefault key defVal envVars
  =  value defVal
  <> readValueFromEnv key envVars

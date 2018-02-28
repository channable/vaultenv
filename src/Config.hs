module Config
  ( Options(..)
  , MilliSeconds(..)
  , parseOptionsFromEnvAndCli
  , LogLevel(..)
  ) where

import Control.Applicative ((<*>), (<|>))
import Data.List (intercalate)
import Data.Monoid ((<>))

import Options.Applicative (value, long, auto, option, metavar, help, flag,
                            str, argument, many, strOption)

import qualified Options.Applicative as OptParse
import qualified Options.Applicative.Builder.Internal as OptParse
import qualified Text.Read as Read

-- | Type alias for enviornment variables, used for readability in this module.
type EnvVar = (String, String)

-- | Newtype wrapper for millisecond values.
newtype MilliSeconds = MilliSeconds { unMilliSeconds :: Int }
  deriving (Eq, Show)

-- | @Options@ contains all the configuration we support in vaultenv. It is
-- used in our @Main@ module to specify behavior.
data Options = Options
  { oVaultHost       :: String
  , oVaultPort       :: Int
  , oVaultToken      :: String
  , oSecretFile      :: FilePath
  , oCmd             :: String
  , oArgs            :: [String]
  , oConnectTls      :: Bool
  , oValidateCerts   :: Bool
  , oInheritEnv      :: Bool
  , oRetryBaseDelay  :: MilliSeconds
  , oRetryAttempts   :: Int
  , oLogLevel        :: LogLevel
  } deriving (Eq)

instance Show Options where
  show opts = intercalate "\n"
    [ "Host:           " ++ oVaultHost opts
    , "Port:           " ++ (show $ oVaultPort opts)
    , "Token:          " ++ "*****"
    , "Secret file:    " ++ oSecretFile opts
    , "Command:        " ++ oCmd opts
    , "Arguments:      " ++ (show $ oArgs opts)
    , "Use TLS:        " ++ (show $ oConnectTls opts)
    , "Validate certs: " ++ (show $ oValidateCerts opts)
    , "Inherit env:    " ++ (show $ oInheritEnv opts)
    , "Base delay:     " ++ (show . unMilliSeconds $ oRetryBaseDelay opts)
    , "Retry attempts: " ++ (show $ oRetryAttempts opts)
    , "Log-level:      " ++ (show $ oLogLevel opts)
    ]

-- | Behavior flags that we allow users to set via environment variables.
-- This type is internal to the workings of this module. It is used as an
-- intermediate value to get optparse-applicative to play nice with environment
-- variables as used for behavior flags. All flags are off by default.
data EnvFlags = EnvFlags
  { efConnectTls :: Bool
  , efValidateCerts :: Bool
  , efInheritEnv :: Bool
  }

-- | LogLevel to run vaultenv under. Under @Error@, which is the default, we
-- will print error messages in error cases. Examples: Vault gives 404s, our
-- token is invalid, we don't have permissions for a certain path, Vault is
-- unavailable.
--
-- Under @Info@, we print some additional information related to the config.
data LogLevel
  = Info
  | Error
  deriving (Eq, Ord, Show)

instance Read LogLevel where
  readsPrec _ "error" = [(Error, "")]
  readsPrec _ "info" = [(Info, "")]
  readsPrec _ _ = []

-- | Parse program options from the command line and the process environment.
parseOptionsFromEnvAndCli :: [EnvVar] -> IO Options
parseOptionsFromEnvAndCli envVars =
  let envFlags = parseEnvFlags envVars
      parser = optionsParserWithInfo envFlags envVars
  in OptParse.execParser parser

-- | Parses behavior flags from a list of environment variables. If an
-- environment variable corresponding to the flag is set to @"true"@ or
-- @"false"@, we use that as the default on the corresponding CLI option.
--
-- If these variables aren't present, we default to @False@. We print an error
-- if they're set to anything else than @"true"@ or @"false"@.
parseEnvFlags :: [EnvVar] -> EnvFlags
parseEnvFlags envVars
  = EnvFlags
  { efConnectTls = lookupEnvFlag "VAULTENV_CONNECT_TLS"
  , efValidateCerts = lookupEnvFlag "VAULTENV_VALIDATE_CERTS"
  , efInheritEnv = lookupEnvFlag "VAULTENV_INHERIT_ENV"
  }
  where
    lookupEnvFlag key =
      case lookup key envVars of
        Just "true" -> True
        Just "false" -> False
        Nothing -> True
        _ -> errorWithoutStackTrace $ "[ERROR]: Invalid value for environment variable " ++ key

-- | This function adds metadata to the @Options@ parser so it can be used with
-- execParser.
optionsParserWithInfo :: EnvFlags -> [EnvVar] -> OptParse.ParserInfo Options
optionsParserWithInfo envFlags localEnvVars =
  OptParse.info
    (OptParse.helper <*> optionsParser envFlags localEnvVars)
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
-- of @Parser@s. The @connectTls@, @validateCerts@ and @inheritEnv@ parsers all
-- have a cousin prefixed with @no@. Combining these with the alternate
-- instance like so
--
-- @
--    noConnectTls <|> connectTls
-- @
--
-- means that they are both mutually exclusive.
--
-- Why do we have the options for the affirmative case? (e.g. why does
-- @--connect-tls@ exist if we default to that behavior?) Because we want to be
-- able to override all config that happens via environment variables on the
-- CLI. So @VAULTENV_CONNECT_TLS=false vaultenv --connect-tls@ should connect
-- to Vault over a secure connection. Without this option, this use case is not
-- possible.
--
-- The way we do this in the parser is by taking an @EnvFlags@ record which
-- contains the settings that were provided via environment variables (and the
-- defaults, if there wasn't anything configured). We use this record to set
-- the default values of the different behaviour switches. So, if an
-- environment variable is used to configure the TLS option, that value will
-- always be used, except if it is overridden on the CLI.
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
    <*> logLevel
  where
    host
      =  strOption
      $  long "host"
      <> metavar "HOST"
      <> stringFromEnvWithDefault "VAULT_HOST" "localhost" envVars
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
      <> stringFromEnv "VAULT_TOKEN" envVars
      <> help "Token to authenticate to Vault with. Also configurable via VAULT_TOKEN."
    secretsFile
      =  strOption
      $  long "secrets-file"
      <> metavar "FILENAME"
      <> stringFromEnv "VAULTENV_SECRETS_FILE" envVars
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
      =  flag (efConnectTls envFlags) False
      $  long "no-connect-tls"
      <> help ("Don't use TLS when connecting to Vault. Default: use TLS. Also " ++
              "configurable via VAULTENV_CONNECT_TLS.")
    connectTls
      =  flag (efConnectTls envFlags) True
      $  long "connect-tls"
      <> help ("Always connect to Vault via TLS. Default: use TLS. Can be used " ++
                "to override VAULTENV_CONNECT_TLS.")
    noValidateCerts
      =  flag (efValidateCerts envFlags) False
      $  long "no-validate-certs"
      <> help ("Don't validate TLS certificates when connecting to Vault. Default: " ++
              "validate certs. Also configurable via VAULTENV_VALIDATE_CERTS.")
    validateCerts
      =  flag (efValidateCerts envFlags) True
      $  long "validate-certs"
      <> help ("Always validate TLS certificates when connecting to Vault. Default: " ++
                "validate certs. Can be used to override VAULTENV_CONNECT_TLS.")
    noInheritEnv
      =  flag (efInheritEnv envFlags) False
      $  long "no-inherit-env"
      <> help ("Don't merge the parent environment with the secrets file. Default: " ++
              "merge environments. Also configurable via VAULTENV_INHERIT_ENV.")
    inheritEnv
      =  flag (efInheritEnv envFlags) True
      $  long "inherit-env"
      <> help ("Always merge the parent environment with the secrets file. Default: " ++
                "merge environments. Can be used to override VAULTENV_INHERIT_ENV.")
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
      <> help ("Maximum number of vault connection retries. Defaults to 9. " ++
               "Also configurable through VAULTENV_RETRY_ATTEMPTS.")
    logLevel
      =  option auto
      $  long "log-level"
      <> metavar "error | info"
      <> readValueFromEnvWithDefault "VAULTENV_LOG_LEVEL" Error envVars
      <> help ("Log-level to run vaultenv under. Options: 'error' or 'info'. " ++
               "Defaults to 'error'. Also configurable via VAULTENV_LOG_LEVEL")


-- | Specialization of @readValueFromEnv@ that does not use a @Read@ instance.
-- This is useful for "plain" string values, so the user does not have to
-- format environment variables like @VAULT_HOST='"localhost"'@. Note the
-- double quoting.
stringFromEnv :: OptParse.HasValue f
              => String
              -> [EnvVar]
              -> OptParse.Mod f String
stringFromEnv key envVars = foldMap value $ lookup key envVars

-- | Like @stringFromEnv@, but with a default value.
stringFromEnvWithDefault :: OptParse.HasValue f
                         => String
                         -> String
                         -> [EnvVar]
                         -> OptParse.Mod f String
stringFromEnvWithDefault key defVal envVars
  =  value defVal
  <> stringFromEnv key envVars

-- | Attempt to parse an optparse default value modifier from a list of
-- environment variables. This function returns an empty option modifier in
-- case the environment variable is missing or does not parse.
readValueFromEnv :: (Read a, OptParse.HasValue f)
                 => String
                 -> [EnvVar]
                 -> OptParse.Mod f a
readValueFromEnv key envVars =
  let parseResult = lookup key envVars >>= Read.readMaybe
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

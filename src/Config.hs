{-|
Module      : Config
Description : Read config from CLI options and environment

This module uses optparse-applicative to parse CLI options. It augments the
optparse parser with a mechanism to allow for overrides in environment
variables.

The main entry point is @parseOptions@.
-}
module Config
  ( Options(..)
  , MilliSeconds(..)
  , parseOptions
  , LogLevel(..)
  , readConfigFromEnvFiles
  , OptionsError
  , ValidScheme(..)
  , defaultOptions, isOptionsComplete, castOptions
  , splitAddress, validateCopyAddr, mergeOptions, getOptionsValue
  , Validated(), Completed()
  ) where

import Control.Applicative ((<*>), (<|>))
import Control.Monad(when)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, isNothing, isJust)
import Data.Monoid ((<>))
import Data.Char (isDigit)
import Data.Either (lefts, rights, isLeft)
import Data.Version (showVersion)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Options.Applicative (value, long, auto, option, metavar, help, flag,
                            str, argument, many)
-- Cabal generates the @Paths_vaultenv_real@ module, which contains a @version@
-- binding with the value out of the Cabal file. This feature is documented at:
-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
import Paths_vaultenv_real (version)
import System.IO.Error (catchIOError)
import System.Exit (die)
import Text.Read (readMaybe)

import qualified Configuration.Dotenv as DotEnv
import qualified Options.Applicative as OptParse
import qualified System.Directory as Dir

-- | Type alias for enviornment variables, used for readability in this module.
type EnvVar = (String, String)

-- | Newtype wrapper for millisecond values.
newtype MilliSeconds = MilliSeconds { unMilliSeconds :: Int }
  deriving (Eq, Show)

-- | @Options@ contains all the configuration we support in vaultenv. It is
-- used in our @Main@ module to specify behavior.
data Options validated completed = Options
  { oVaultHost       :: Maybe String
  , oVaultPort       :: Maybe Int
  , oVaultAddr       :: Maybe URI
  , oVaultToken      :: Maybe String
  , oSecretFile      :: Maybe FilePath
  , oCmd             :: Maybe String
  , oArgs            :: Maybe [String]
  , oConnectTls      :: Maybe Bool
  , oValidateCerts   :: Maybe Bool
  , oInheritEnv      :: Maybe Bool
  , oInheritEnvBlacklist :: Maybe [String]
  , oRetryBaseDelay  :: Maybe MilliSeconds
  , oRetryAttempts   :: Maybe Int
  , oLogLevel        :: Maybe LogLevel
  , oUsePath         :: Maybe Bool
  } deriving (Eq)

-- | Phantom type that indicates that an option is
-- checked to be completed by ```isOptionsComplete``` and is validated by
-- ```validateCopyAddr```
data Completed

-- | Phantom type that indicates that an option is not yet checked to be complete
data UnCompleted

-- | Phantom type that indicates that an option is
-- validated by ```validateCopyAddr```
data Validated

-- | Phantom type that indicates that an option is
-- not validated by ```isOptionsComplete```
data UnValidated

-- | The default options that should be used when no value is specified
-- If this default changes, please update the function isOptionsComplete if needed.
defaultOptions :: Options Validated UnCompleted
defaultOptions = Options
  { oVaultHost      = Just "localhost"
  , oVaultPort      = Just 8200
  , oVaultAddr      = parseURI "https://localhost:8200"
  , oVaultToken     = Nothing
  , oSecretFile     = Nothing
  , oCmd            = Nothing
  , oArgs           = Just []
  , oConnectTls     = Just True
  , oValidateCerts  = Just True
  , oInheritEnv     = Just True
  , oInheritEnvBlacklist = Just []
  , oRetryBaseDelay = Just (MilliSeconds 40)
  , oRetryAttempts  = Just 9
  , oLogLevel       = Just Error
  , oUsePath        = Just True
}

-- | Casts one options structure into antoher, use only when certain that
-- the validated and completed options can be given to a options file.
castOptions :: Options a b -> Options c d
castOptions opts = Options
  { oVaultHost      = oVaultHost opts
  , oVaultPort      = oVaultPort opts
  , oVaultAddr      = oVaultAddr opts
  , oVaultToken     = oVaultToken opts
  , oSecretFile     = oSecretFile opts
  , oCmd            = oCmd opts
  , oArgs           = oArgs opts
  , oConnectTls     = oConnectTls opts
  , oValidateCerts  = oValidateCerts opts
  , oInheritEnv     = oInheritEnv opts
  , oInheritEnvBlacklist = oInheritEnvBlacklist opts
  , oRetryBaseDelay = oRetryBaseDelay opts
  , oRetryAttempts  = oRetryAttempts opts
  , oLogLevel       = oLogLevel opts
  , oUsePath        = oUsePath opts
  }

instance Show (Options valid complete) where
  show opts = intercalate "\n"
    [ "Host:           " ++ showSpecifiedString (oVaultHost opts)
    , "Port:           " ++ showSpecified (oVaultPort opts)
    , "Addr:           " ++ showSpecified (oVaultAddr opts)
    , "Token:          " ++ maybe "Unspecified" (const "*****") (oVaultToken opts)
    , "Secret file:    " ++ showSpecifiedString (oSecretFile opts)
    , "Command:        " ++ showSpecifiedString (oCmd opts)
    , "Arguments:      " ++ showSpecified (oArgs opts)
    , "Use TLS:        " ++ showSpecified (oConnectTls opts)
    , "Validate certs: " ++ showSpecified (oValidateCerts opts)
    , "Inherit env:    " ++ showSpecified (oInheritEnv opts)
    , "Inherit env blacklist: " ++ showSpecified (oInheritEnvBlacklist opts)
    , "Base delay:     " ++ showSpecified (unMilliSeconds <$> oRetryBaseDelay opts)
    , "Retry attempts: " ++ showSpecified (oRetryAttempts opts)
    , "Log-level:      " ++ showSpecified (oLogLevel opts)
    , "Use PATH:       " ++ showSpecified (oUsePath opts)
    ] where
      showSpecified :: Show a => Maybe a -> String
      showSpecified (Just x) = show x
      showSpecified Nothing = "Unspecified"
      showSpecifiedString = fromMaybe "Unspecified"

-- | Gets the option from a Maybe, ```name``` should only be used for debugging
-- purposes, as ```isOptionsComplete``` should verify that every property that
-- does not have a default value, is present in the options.
getOptionsValue :: (Options Validated Completed -> Maybe c) -> Options Validated Completed -> c
getOptionsValue f opts=
    fromMaybe (error "Complete options has an unknown key") (f opts)

-- | The possible errors that can occur in the construction of an Options datatype
data OptionsError
  = UnspecifiedValue String -- ^ A value is missing and no default is specified
  | URIParseError URI -- ^ The URI could not be parsed
  | UnknownScheme URI -- ^ The scheme of the address is invalid, e.g. ftp://
  | NonNumericPort String -- ^ The port of the address is not an valid integer
  | HostPortSchemeAddrMismatch String (Maybe Bool) (Maybe String) (Maybe Int) URI
      -- ^ The source, useTls, host, port and scheme do not match the provided address
  deriving (Eq)

instance Show OptionsError where
  show (UnspecifiedValue s) = "The option " ++ s ++ " is required but not specified"
  show (URIParseError uri) = "The address" ++ show uri ++ " could not be parsed properly. Maybe the schema is missing?"
  show (UnknownScheme uri)  = "The address " ++ show uri ++ " has no recognisable scheme, "
      ++ " expected http:// or https:// at the beginning of the address."
  show (NonNumericPort s) = "The port " ++ s ++ " is not a valid int value"
  show (HostPortSchemeAddrMismatch source useTLs host port addr) =
    concat [
      "Confliciting configuration values in ", source, "\n",
      "Vault address: ", show addr, "\n",
      "Vault host: ",  maybe "Unspecified" show host, "\n",
      "Vault port: ",  maybe "Unspecified" show port, "\n",
      "Use TLS: ",  maybe "Unspecified" show useTLs, "\n",
      "Hint: This can happen when you provide a `addr` which ",
      "conflicts with `port`, `host`, or `[no-]connect-tls` in either the environment variables or the CLI.\n"
    ]

-- | Validates for a set of options that any provided addr is valid and that either the
-- scheme, host and port or that any given addr matches the other provided information.
validateCopyAddr :: String -> Options UnValidated completed -> Either OptionsError (Options Validated completed)
validateCopyAddr source opts = case oVaultAddr opts of
  Nothing -> Right $ castOptions opts
  Just addr -> do
    (scheme, addrHost, addrPort) <- splitAddress addr
    let mHost = oVaultHost opts
        mPort = oVaultPort opts
        mUseTLS = oConnectTls opts
        addrTLS = case scheme of
          HTTPS -> True
          HTTP -> False
        doesAddrDiffer =
            (isJust mHost && Just addrHost /= mHost) -- Is the Host set and the same
            ||
            (isJust mPort && Just addrPort /= mPort) -- Is the Port set and the same
            ||
            (isJust mUseTLS && Just addrTLS /= mUseTLS) -- Is the UseTLS set and the same
    when doesAddrDiffer
      (Left $ HostPortSchemeAddrMismatch
          source
          mUseTLS
          mHost
          mPort
          addr
      )
    pure opts
      { oVaultHost = Just addrHost
      , oVaultPort = Just addrPort
      , oConnectTls = Just addrTLS
      }

-- | This functions merges two options, where every specific option in the
-- second options parameter is only used if for that option no value is
-- specified in the first options parameter.
mergeOptions  :: Options Validated UnCompleted
              -> Options Validated UnCompleted
              -> Options Validated UnCompleted
mergeOptions opts1 opts2 = let
    combine :: (Options Validated UnCompleted -> Maybe a) -> Maybe a
    combine f | isJust (f opts1) = f opts1
              | otherwise = f opts2
  in
  Options
  { oVaultHost      = combine oVaultHost
  , oVaultPort      = combine oVaultPort
  , oVaultAddr      = combine oVaultAddr
  , oVaultToken     = combine oVaultToken
  , oSecretFile     = combine oSecretFile
  , oCmd            = combine oCmd
  , oArgs           = combine oArgs
  , oConnectTls     = combine oConnectTls
  , oValidateCerts  = combine oValidateCerts
  , oInheritEnv     = combine oInheritEnv
  , oInheritEnvBlacklist = combine oInheritEnvBlacklist
  , oRetryBaseDelay = combine oRetryBaseDelay
  , oRetryAttempts  = combine oRetryAttempts
  , oLogLevel       = combine oLogLevel
  , oUsePath        = combine oUsePath
  }



-- | Verifies that either an ```Options``` is complete, according to the test,
-- or it gives a list of errors. Only checks for values that are not included
-- in the default.
isOptionsComplete :: Options Validated UnCompleted
                  -> Either [OptionsError] (Options Validated Completed)
isOptionsComplete opts =
      let errors = concat
            [
              [UnspecifiedValue "Token"       | isNothing (oVaultToken opts)]
            , [UnspecifiedValue "Command"     | isNothing (oCmd opts)]
            , [UnspecifiedValue "Secret file" | isNothing (oSecretFile opts)]
            ]
      in  if not (null errors)
          then Left errors
          else Right (castOptions opts)

-- | The host part of an address
type Host       = String
-- | The scheme part of address
data ValidScheme = HTTP | HTTPS deriving (Eq, Show)

-- | This function splits the address into three different parts. The first
-- part is a Scheme, which must be either http or https.
-- The Host part of the return type is the part between the scheme and the
-- last colon in the address. The port is the part after the colon.
--
-- Examples:
-- http://localhost:80 -> (HTTP, "localhost", "80")
-- https://localhost:80 -> (HTTPS, "localhost", "80")
-- ftp://localhost:80 -> Left $ UnknownScheme ftp://localhost:80
-- localhost:80 -> Left $ URIParseError localhost:80
-- http://localhost:80/foo/bar -> Right (HTTP, "localhost", "80")
splitAddress :: URI -> Either OptionsError (ValidScheme, Host, Int)
splitAddress addr = do
  uriAuth <- maybe (Left $ URIParseError addr) Right $ uriAuthority addr
  scheme <- case uriScheme addr of
    "https:" -> Right HTTPS
    "http:" -> Right HTTP
    _ -> Left $ UnknownScheme addr

  let
    host = uriRegName uriAuth
    portSection = uriPort uriAuth

  port <-
    -- Default port from scheme when no port is given
    if null portSection
    then case scheme of
      HTTPS -> Right 443
      HTTP -> Right 80
      -- Strip the leading colon before reading the value
    else
      -- The parseURI itself should reject non-numeric ports of its own, but
      -- since they represent the port as a string one cannot be sure.
      maybe (Left $ NonNumericPort portSection) Right $
        readMaybe $ tail portSection

  pure (scheme, host, port)

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
parseOptions :: [EnvVar] -> [[EnvVar]] -> IO (Options Validated Completed)
parseOptions localEnvVars envFileSettings =
  let eLocalEnvFlagsOptions = validateCopyAddr "local environment variables" $ parseEnvOptions localEnvVars
      eEnvFileSettingsOptions = map (validateCopyAddr "environment file" . parseEnvOptions) envFileSettings
  in do
    eParseResult <- validateCopyAddr "cli options" <$> OptParse.execParser parserCliOptions
    let results = eEnvFileSettingsOptions ++ [eLocalEnvFlagsOptions, eParseResult]
    if any isLeft results then
      die (unlines (map (("[ERROR] "++). show) $ lefts results))
    else
        let
          combined = foldl (flip mergeOptions) defaultOptions (rights results)
          completed = isOptionsComplete combined
          in either
                (\ls -> die (unlines (map (("[ERROR] "++). show) ls)))
                return
                completed

-- | Parses options from a list of environment variables. If an
-- environment variable corresponding to the flag is set to @"true"@ or
-- @"false"@, we use that as the default on the corresponding CLI option.
-- Other options are either String, Int, [String] or LogLevel.
parseEnvOptions :: [EnvVar] -> Options UnValidated UnCompleted
parseEnvOptions envVars
  = Options
  { oVaultHost      = lookupEnvString   "VAULT_HOST"
  , oVaultPort      = lookupEnvInt      "VAULT_PORT"
  , oVaultAddr      = lookupVaultAddr
  , oVaultToken     = lookupEnvString   "VAULT_TOKEN"
  , oSecretFile     = lookupEnvString   "VAULTENV_SECRETS_FILE"
  , oCmd            = lookupEnvString   "CMD"
  , oArgs           = lookupStringList  "ARGS..."
  , oConnectTls     = lookupEnvFlag     "VAULTENV_CONNECT_TLS"
  , oValidateCerts  = lookupEnvFlag     "VAULTENV_VALIDATE_CERTS"
  , oInheritEnv     = lookupEnvFlag     "VAULTENV_INHERIT_ENV"
  , oInheritEnvBlacklist = lookupCommaSeparatedList "VAULTENV_INHERIT_ENV_BLACKLIST"
  , oRetryBaseDelay = MilliSeconds <$> lookupEnvInt      "VAULTENV_RETRY_BASE_DELAY"
  , oRetryAttempts  = lookupEnvInt      "VAULTENV_RETRY_ATTEMPTS"
  , oLogLevel       = lookupEnvLogLevel "VAULTENV_LOG_LEVEL"
  , oUsePath        = lookupEnvFlag     "VAULTENV_USE_PATH"
  }
  where
    -- | Throws an error for an invalid key
    err :: String -> a
    err key = errorWithoutStackTrace $ "[ERROR]: Invalid value for environment variable " ++ key
    -- | Lookup a string in the ```envVars``` list
    lookupEnvString key = lookup key envVars
    -- | Lookup an integer using ```lookupEnvString```,
    -- parses is to a Just, Nothing means not an Int
    lookupEnvInt :: String -> Maybe Int
    lookupEnvInt key
      | isNothing sVal = Nothing
      | not (null sVal) && all isDigit (fromJust sVal) = read <$> sVal
      | otherwise = err key
      where sVal = lookupEnvString key
    -- | Lookup a list of strings using ```lookupEnvString```
    lookupStringList :: String -> Maybe [String]
    lookupStringList key = words <$> lookupEnvString key
    -- | Lookup a comma-separated list of strings using ```lookupEnvString```
    lookupCommaSeparatedList :: String -> Maybe [String]
    lookupCommaSeparatedList key = splitOn ',' <$> lookupEnvString key
    -- | Lookup an log level using ```lookupEnvString```
    lookupEnvLogLevel :: String -> Maybe LogLevel
    lookupEnvLogLevel key =
      case lookup key envVars of
        Just "info" -> Just Info
        Just "error" -> Just Error
        Nothing -> Nothing
        _ -> err key
    -- | Look up and parse VAULD_ADDR
    lookupVaultAddr :: Maybe URI
    lookupVaultAddr = do
      addrString <- lookupEnvString "VAULT_ADDR"
      pure $ fromMaybe
        (errorWithoutStackTrace "[Error]: Invalid value for environment variable VAULT_ADDR") $
        parseURI addrString
    -- | Lookup a boolean flag using ```lookupEnvString```
    lookupEnvFlag :: String -> Maybe Bool
    lookupEnvFlag key =
      case lookup key envVars of
        Just "true" -> Just True
        Just "false" -> Just False
        Nothing -> Nothing
        _ -> err key

-- | This function adds metadata to the @Options@ parser so it can be used with
-- execParser.
parserCliOptions :: OptParse.ParserInfo (Options UnValidated UnCompleted)
parserCliOptions =
  OptParse.info
    (OptParse.helper <*> versionOption <*> optionsParser)
    (OptParse.fullDesc <> OptParse.header header)
  where
    versionOption = OptParse.infoOption (showVersion version) (long "version" <> help "Show version")
    header = "vaultenv " ++ showVersion version ++ " - run programs with secrets from HashiCorp Vault"


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
optionsParser :: OptParse.Parser (Options UnValidated UnCompleted)
optionsParser = Options
    <$> host
    <*> port
    <*> addr
    <*> token
    <*> secretsFile
    <*> cmd
    <*> cmdArgs
    <*> (noConnectTls    <|> connectTls)
    <*> (noValidateCerts <|> validateCerts)
    <*> (noInheritEnv    <|> inheritEnv)
    <*> inheritEnvBlacklist
    <*> baseDelayMs
    <*> retryAttempts
    <*> logLevel
    <*> usePath
  where
    maybeStr = Just <$> str
    maybeStrOption = option maybeStr
    host
      =  maybeStrOption
      $  long "host"
      <> metavar "HOST"
      <> value Nothing
      <> help ("Vault host, either an IP address or DNS name. Defaults to localhost. " ++
               "Also configurable via VAULT_HOST.")
    port
      =  option (Just <$> auto)
      $  long "port"
      <> metavar "PORT"
      <> value Nothing
      <> help "Vault port. Defaults to 8200. Also configurable via VAULT_PORT."
    addr = option (parseURI <$> str)
        $  long "addr"
        <> metavar "ADDR"
        <> value Nothing
        <> help ("Vault address, the scheme, either http:// or https://, the ip-address or DNS name, " ++
            "followed by the port, separated with a ':'." ++
            " Cannot be combined with either VAULT_PORT or VAULT_HOST")
    token
      =  maybeStrOption
      $  long "token"
      <> metavar "TOKEN"
      <> value Nothing
      <> help "Token to authenticate to Vault with. Also configurable via VAULT_TOKEN."
    secretsFile
      =  maybeStrOption
      $  long "secrets-file"
      <> metavar "FILENAME"
      <> value Nothing
      <> help ("Config file specifying which secrets to request. Also configurable " ++
               "via VAULTENV_SECRETS_FILE." )
    cmd
      =  argument maybeStr
      $  metavar "CMD"
      <> help "command to run after fetching secrets"
      <> value Nothing
    cmdArgs
      = (\lst -> if null lst then Nothing else Just lst) <$>
        many ( argument str
        (     metavar "ARGS..."
          <>  help "Arguments to pass to CMD, defaults to nothing")
        )
    noConnectTls
      =  flag Nothing (Just False)
      $  long "no-connect-tls"
      <> help ("Don't use TLS when connecting to Vault. Default: use TLS. Also " ++
              "configurable via VAULTENV_CONNECT_TLS.")
    connectTls
      =  flag Nothing (Just True)
      $  long "connect-tls"
      <> help ("Always connect to Vault via TLS. Default: use TLS. Can be used " ++
                "to override VAULTENV_CONNECT_TLS.")
    noValidateCerts
      =  flag Nothing (Just True)
      $  long "no-validate-certs"
      <> help ("Don't validate TLS certificates when connecting to Vault. Default: " ++
              "validate certs. Also configurable via VAULTENV_VALIDATE_CERTS.")
    validateCerts
      =  flag Nothing (Just True)
      $  long "validate-certs"
      <> help ("Always validate TLS certificates when connecting to Vault. Default: " ++
                "validate certs. Can be used to override VAULTENV_CONNECT_TLS.")
    noInheritEnv
      =  flag Nothing (Just False)
      $  long "no-inherit-env"
      <> help ("Don't merge the parent environment with the secrets file. Default: " ++
              "merge environments. Also configurable via VAULTENV_INHERIT_ENV.")
    inheritEnv
      =  flag Nothing (Just True)
      $  long "inherit-env"
      <> help ("Always merge the parent environment with the secrets file. Default: " ++
                "merge environments. Can be used to override VAULTENV_INHERIT_ENV.")

    maybeStrList = Just . splitOn ',' <$> str
    maybeStrListOption = option maybeStrList

    inheritEnvBlacklist
      = maybeStrListOption
      $ long "inherit-env-blacklist"
      <> metavar "COMMA_SEPARATED_NAMES"
      <> value Nothing
      <> help ("Comma-separated list of environment variable names to remove from " ++
               "the environment before executing CMD. Also configurable via " ++
               "VAULTENV_INHERIT_ENV_BLACKLIST. Has no effect if no-inherit-env is set!")

    baseDelayMs
      =  fmap MilliSeconds <$> option (Just <$> auto)
      (  long "retry-base-delay-milliseconds"
      <> metavar "MILLISECONDS"
      <> value Nothing
      <> help ("Base delay for vault connection retrying. Defaults to 40ms. " ++
                "Also configurable via VAULTENV_RETRY_BASE_DELAY_MS.")
      )
    retryAttempts
      =  option (Just <$> auto)
      $  long "retry-attempts"
      <> metavar "NUM"
      <> value Nothing
      <> help ("Maximum number of vault connection retries. Defaults to 9. " ++
               "Also configurable through VAULTENV_RETRY_ATTEMPTS.")
    logLevel
      =  option (Just <$> auto)
      $  long "log-level"
      <> value Nothing
      <> metavar "error | info"

      <> help ("Log-level to run vaultenv under. Options: 'error' or 'info'. " ++
               "Defaults to 'error'. Also configurable via VAULTENV_LOG_LEVEL")
    usePath
      =  flag Nothing (Just True)
      $  long "use-path"
      <> help ("Use PATH for finding the executable that vaultenv should call. Default: " ++
              "don't search PATH. Also configurable via VAULTENV_USE_PATH.")

-- | Split a list of elements on the given separator, returning sublists without this
-- separator item.
--
-- Example:
--
-- >>> splitOn ',' "somestring,anotherstring"
-- ["somestring", "anotherstring"]
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn sep x
  = case span (/= sep) x of
    (y, []) -> [y]
    (y, ys) -> y : splitOn sep (tail ys)

-- | Search for environment files in default locations and load them in order.
--
-- This function tries to read the following files in order to obtain
-- environment configuration. This is implicit behavior and allows the user to
-- configure vaultenv without setting up environment variables or passing CLI
-- flags. This is nicer for interactive usage.
readConfigFromEnvFiles :: IO [[(String, String)]]
readConfigFromEnvFiles = do
  xdgDir <- (Just <$> Dir.getXdgDirectory Dir.XdgConfig "vaultenv")
    `catchIOError` const (pure Nothing)
  cwd <- Dir.getCurrentDirectory
  let
    machineConfigFile = "/etc/vaultenv.conf"

    userConfigFile :: Maybe FilePath
    userConfigFile = fmap (++ "/vaultenv.conf") xdgDir
    cwdConfigFile = cwd ++ "/.env"

  -- @doesFileExist@ doesn't throw exceptions, it catches @IOError@s and
  -- returns @False@ if those are encountered.
  machineConfigExists <- Dir.doesFileExist machineConfigFile
  machineConfig <- if machineConfigExists
    then DotEnv.parseFile machineConfigFile
    else pure []

  userConfigExists <- case userConfigFile of
     Nothing -> pure False
     Just fp -> Dir.doesFileExist fp
  userConfig <- if userConfigExists
    then DotEnv.parseFile (fromJust userConfigFile) -- safe because of loadUserConfig
    else pure []

  cwdConfigExists <- Dir.doesFileExist cwdConfigFile
  cwdConfig <- if cwdConfigExists
    then DotEnv.parseFile cwdConfigFile
    else pure []

  -- Deduplicate, user config takes precedence over machine config
  let config = [machineConfig, userConfig, cwdConfig]
  pure config

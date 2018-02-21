{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad          (forM)
import Control.Lens           (reindexed, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char              (toUpper)
import Data.Either            (isLeft)
import Data.List              (findIndex, lookup)
import Data.Monoid            ((<>))
import Network.Connection     (TLSSettings(..))
import Network.HTTP.Client    (defaultManagerSettings)
import Network.HTTP.Conduit   (Manager, newManager, mkManagerSettings)
import Network.HTTP.Simple    (HttpException(..), Request, Response,
                               defaultRequest, setRequestHeader, setRequestPort,
                               setRequestPath, setRequestHost, setRequestManager,
                               setRequestSecure, httpLBS, getResponseBody,
                               getResponseStatusCode)
import Options.Applicative    hiding (Parser, command)
import System.Environment     (getEnvironment)
import System.Posix.Process   (executeFile)
import System.IO              (stderr, hPutStrLn)
import Control.Monad.Except   (ExceptT, MonadError, runExceptT, throwError)

import qualified Control.Concurrent.Async   as Async
import qualified Control.Exception          as Exception
import qualified Control.Retry              as Retry
import qualified Data.Aeson.Lens            as Lens (key, members, _String)
import qualified Data.Bifunctor             as Bifunctor
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy       as LBS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.Map                   as Map
import qualified Data.Map.Lens              as Lens (toMapOf)
import qualified Data.Text                  as Text
import qualified Options.Applicative        as O
import qualified Text.Read as Read

--
-- Datatypes
--

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

data EnvSwitches = EnvSwitches
  { esNoConnectTls :: Bool
  , esNoValidateCerts :: Bool
  , esNoInheritEnv :: Bool
  }

data Secret = Secret
  { sPath    :: String
  , sKey     :: String
  , sVarName :: String
  } deriving (Eq, Show)

type EnvVar = (String, String)

data Context
  = Context
  { cLocalEnvVars :: [EnvVar]
  , cCliOptions :: Options
  , cHttpManager :: Manager
  }

type VaultData = Map.Map String String

data VaultError
  = SecretNotFound    String
  | IOError           FilePath
  | ParseError        FilePath
  | KeyNotFound       Secret
  | BadRequest        LBS.ByteString
  | Forbidden
  | ServerError       LBS.ByteString
  | ServerUnavailable LBS.ByteString
  | ServerUnreachable HttpException
  | InvalidUrl        String
  | DuplicateVar      String
  | Unspecified       Int LBS.ByteString

newtype MilliSeconds = MilliSeconds { unMilliSeconds :: Int }
  deriving (Eq, Show)

--
-- Argument parsing
--

-- | Parser for our CLI options. Seems intimidating, but is straightforward
-- once you know about applicative parsing patterns. We construct a parser for
-- @Options@ by concatenating parsers for parts of the record.
--
-- Toy example to illustrate the pattern:
--
-- @
--     OptionRecord <$> parser1 <*> parser2 <*> parser3
-- @
--
-- Here, the parser for @OptionRecord@ is the combination of parsers of it's
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
optionsParser :: EnvSwitches -> [EnvVar] -> O.Parser Options
optionsParser envSwitches environment = Options
    <$> strOption
      (  long "host"
      <> metavar "HOST"
      <> value "localhost"
      <> lookupFromEnv "VAULT_HOST"
      <> help ("Vault host, either an IP address or DNS name. Defaults to localhost. " ++
               "Also configurable via VAULT_HOST."))
    <*> option auto
      (  long "port"
      <> metavar "PORT"
      <> lookupFromEnvWithDefault "VAULT_PORT" 8200
      <> help "Vault port. Defaults to 8200. Also configurable via VAULT_PORT." )
    <*> strOption
      (  long "token"
      <> metavar "TOKEN"
      <> lookupFromEnv "VAULT_TOKEN"
      <> help "Token to authenticate to Vault with. Also configurable via VAULT_TOKEN.")
    <*> strOption
      (  long "secrets-file"
      <> metavar "FILENAME"
      <> lookupFromEnv "VAULTENV_SECRETS_FILE"
      <> help ("Config file specifying which secrets to request. Also configurable " ++
               "via VAULTENV_SECRETS_FILE." ))
    <*> argument str
      (  metavar "CMD"
      <> help "command to run after fetching secrets")
    <*> many (argument str
      (  metavar "ARGS..."
      <> help "Arguments to pass to CMD, defaults to nothing"))
    <*>
      ( flag (esNoConnectTls envSwitches) True
        (  long "no-connect-tls"
        <> help ("Don't use TLS when connecting to Vault. Default: use TLS. Also " ++
                "configurable via VAULTENV_NO_CONNECT_TLS."))
      <|> flag (esNoConnectTls envSwitches) False
        (  long "connect-tls"
        <> help ("Always connect to Vault via TLS. Default: use TLS. Can be used " ++
                 "to override VAULTENV_NO_CONNECT_TLS.")
        )
      )
    <*>
      ( flag (esNoValidateCerts envSwitches) True
        (  long "no-validate-certs"
        <> help ("Don't validate TLS certificates when connecting to Vault. Default: " ++
                "validate certs. Also configurable via VAULTENV_NO_VALIDATE_CERTS."))
      <|> flag (esNoValidateCerts envSwitches) False
        (  long "validate-certs"
        <> help ("Always validate TLS certificates when connecting to Vault. Default: " ++
                 "validate certs. Can be used to override VAULTENV_NO_CONNECT_TLS.")
        )
      )
    <*>
      ( flag (esNoInheritEnv envSwitches) True
        (  long "no-inherit-env"
        <> help ("Don't merge the parent environment with the secrets file. Default: " ++
                "merge environments. Also configurable via VAULTENV_NO_INHERIT_ENV."))
      <|> flag (esNoInheritEnv envSwitches) False
        (  long "inherit-env"
        <> help ("Always merge the parent environment with the secrets file. Default: " ++
                 "merge environments. Can be used to override VAULTENV_NO_INHERIT_ENV.")
        )
      )
    <*> (MilliSeconds <$> option auto
            (  long "retry-base-delay-milliseconds"
            <> metavar "MILLISECONDS"
            <> lookupFromEnvWithDefault "VAULTENV_RETRY_BASE_DELAY_MS" 40
            <> help ("Base delay for vault connection retrying. Defaults to 40ms. " ++
                     "Also configurable via VAULTENV_RETRY_BASE_DELAY_MS.")))
    <*> option auto
      (  long "retry-attempts"
      <> metavar "NUM"
      <> lookupFromEnvWithDefault "VAULTENV_RETRY_ATTEMPTS" 9
      <> help "Maximum number of vault connection retries. Defaults to 9")
  where
    lookupFromEnv key = foldMap value (lookup key environment)
    lookupFromEnvWithDefault key defVal = maybe (value defVal) value (readFromEnvironment key)

    readFromEnvironment :: Read a => String -> Maybe a
    readFromEnvironment var = lookup var environment >>= Read.readMaybe

-- | Add metadata to the `options` parser so it can be used with execParser.
optionsInfo :: EnvSwitches -> [EnvVar] -> ParserInfo Options
optionsInfo envSwitches localEnvVars =
  info
    (optionsParser envSwitches localEnvVars <**> helper)
    (fullDesc <> header "vaultenv - run programs with secrets from HashiCorp Vault")

-- | Parses behaviour switches from a list of environment variables. If an
-- environment variable corresponding to the flag is set to @"true"@ or
-- @"false"@, we use that as the default on the corresponding CLI option.
--
-- If these variables aren't present, we default to @False@. We print an error
-- if they're set to anything else than @"true"@ or @"false"@.
parseEnvSwitches :: [EnvVar] -> EnvSwitches
parseEnvSwitches vars
  = EnvSwitches
  { esNoConnectTls = envSwitch "VAULTENV_NO_CONNECT_TLS"
  , esNoValidateCerts = envSwitch "VAULTENV_NO_VALIDATE_CERTS"
  , esNoInheritEnv = envSwitch "VAULTENV_NO_INHERIT_ENV"
  }
  where
    envSwitch key =
      case lookup key vars of
        Just "true" -> True
        Just "false" -> False
        Nothing -> False
        _ -> errorWithoutStackTrace $ "[ERROR]: Invalid value for environment variable " ++ key

-- | Retry configuration to use for network requests to Vault.
-- We use a limited exponential backoff with the policy
-- fullJitterBackoff that comes with the Retry package.
vaultRetryPolicy :: (MonadIO m) => Options -> Retry.RetryPolicyM m
vaultRetryPolicy opts = Retry.fullJitterBackoff (unMilliSeconds (oRetryBaseDelay opts) * 1000)
                     <> Retry.limitRetries (oRetryAttempts opts)

--
-- IO
--

main :: IO ()
main = do
  localEnvVars <- getEnvironment

  cliAndEnvOptions <- execParser (optionsInfo (parseEnvSwitches localEnvVars) localEnvVars)

  print cliAndEnvOptions

  httpManager <- getHttpManager cliAndEnvOptions

  let context = Context { cLocalEnvVars = localEnvVars
                        , cCliOptions = cliAndEnvOptions
                        , cHttpManager = httpManager
                        }

  runExceptT (vaultEnv context) >>= \case
    Left err -> hPutStrLn stderr (vaultErrorLogMessage err)
    Right newEnv -> runCommand cliAndEnvOptions newEnv

-- | This function returns either a manager for plain HTTP or
-- for HTTPS connections. If TLS is wanted, we also check if the
-- user specified an option to disable the certificate check.
getHttpManager :: Options -> IO Manager
getHttpManager opts = newManager managerSettings
  where
    managerSettings = if oNoConnectTls opts
                      then defaultManagerSettings
                      else mkManagerSettings tlsSettings Nothing
    tlsSettings = TLSSettingsSimple
                { settingDisableCertificateValidation = oNoValidateCerts opts
                , settingDisableSession = False
                , settingUseServerName = True
                }

-- | Main logic of our application. Reads a list of secrets, fetches
-- each of them from Vault, checks for duplicates, and then yields
-- the list of environment variables to make available to the process
-- we want to run eventually. It either scrubs the environment that
-- already existed or keeps it.
--
-- Signals failure through a value of type VaultError, but can also
-- throw HTTP exceptions.
vaultEnv :: Context -> ExceptT VaultError IO [EnvVar]
vaultEnv context = do
  secrets <- readSecretList (oSecretFile . cCliOptions $ context)
  secretEnv <- requestSecrets context secrets
  checkNoDuplicates (buildEnv secretEnv)
    where
      checkNoDuplicates :: MonadError VaultError m => [EnvVar] -> m [EnvVar]
      checkNoDuplicates e =
        either (throwError . DuplicateVar) (return . const e) $ dups (map fst e)

      -- We need to check duplicates in the environment and fail if
      -- there are any. `dups` runs in O(n^2),
      -- but this shouldn't matter for our small lists.
      --
      -- Equality is determined on the first element of the env var
      -- tuples.
      dups :: Eq a => [a] -> Either a ()
      dups [] = Right ()
      dups (x:xs) | isDup x xs = Left x
                  | otherwise = dups xs

      isDup x = foldr (\y acc -> acc || x == y) False

      buildEnv :: [EnvVar] -> [EnvVar]
      buildEnv secretsEnv =
        if (oNoInheritEnv . cCliOptions $ context)
        then secretsEnv
        else secretsEnv ++ (cLocalEnvVars context)


parseSecret :: String -> Either String Secret
parseSecret line =
  let
    (name, pathAndKey) = case findIndex (== '=') line of
      Just index -> cutAt index line
      Nothing -> ("", line)
  in do
    (path, key) <- case findIndex (== '#') pathAndKey of
      Just index -> Right (cutAt index pathAndKey)
      Nothing -> Left $ "Secret path '" ++ pathAndKey ++ "' does not contain '#' separator."
    let
      varName = if name == ""
        then varNameFromKey path key
        else name
    pure Secret { sPath = path
                , sKey = key
                , sVarName = varName
                }


readSecretList :: (MonadError VaultError m, MonadIO m) => FilePath -> m [Secret]
readSecretList fname = do
  mfile <- liftIO $ safeReadFile
  maybe (throwError $ IOError fname) parseSecrets mfile
  where
    parseSecrets file =
      let
        esecrets = traverse parseSecret . lines $ file
      in
        either (throwError . ParseError) return esecrets

    safeReadFile =
      Exception.catch (Just <$> readFile fname)
        ((\_ -> return Nothing) :: Exception.IOException -> IO (Maybe String))


runCommand :: Options -> [EnvVar] -> IO a
runCommand options env =
  let
    command = oCmd options
    searchPath = False
    args = oArgs options
    env' = Just env
  in
    -- `executeFile` calls one of the syscalls in the execv* family, which
    -- replaces the current process with `command`. It does not return.
    executeFile command searchPath args env'

-- | Request all the data associated with a secret from the vault.
requestSecret :: Context -> String -> IO (Either VaultError VaultData)
requestSecret context secretPath =
  let
    cliOptions = cCliOptions context
    requestPath = "/v1/secret/" <> secretPath
    request = setRequestManager (cHttpManager context)
            $ setRequestHeader "x-vault-token" [SBS.pack (oVaultToken cliOptions)]
            $ setRequestPath (SBS.pack requestPath)
            $ setRequestPort (oVaultPort cliOptions)
            $ setRequestHost (SBS.pack (oVaultHost cliOptions))
            $ setRequestSecure (not $ oNoConnectTls cliOptions)
            $ defaultRequest

    shouldRetry = const $ return . isLeft
    retryAction _retryStatus = doRequest secretPath request
  in
    Retry.retrying (vaultRetryPolicy cliOptions) shouldRetry retryAction

-- | Request all the supplied secrets from the vault, but just once, even if
-- multiple keys are specified for a single secret. This is an optimization in
-- order to avoid unnecessary round trips and DNS requets.
requestSecrets :: Context -> [Secret] -> (ExceptT VaultError IO) [EnvVar]
requestSecrets context secrets = do
  let secretPaths = Foldable.foldMap (\x -> Map.singleton x x) $ fmap sPath secrets
  secretDataOrErr <- liftIO $ Async.mapConcurrently (requestSecret context) secretPaths
  either throwError return $ sequence secretDataOrErr >>= lookupSecrets secrets

-- | Look for the requested keys in the secret data that has been previously fetched.
lookupSecrets :: [Secret] -> Map.Map String VaultData -> Either VaultError [EnvVar]
lookupSecrets secrets vaultData = forM secrets $ \secret ->
  let secretData = Map.lookup (sPath secret) vaultData
      secretValue = secretData >>= Map.lookup (sKey secret)
      toEnvVar val = (sVarName secret, val)
  in maybe (Left $ KeyNotFound secret) (Right . toEnvVar) $ secretValue

-- | Send a request for secrets to the vault and parse the response.
doRequest :: String -> Request -> IO (Either VaultError VaultData)
doRequest secretPath request = do
  respOrEx <- Exception.try . httpLBS $ request :: IO (Either HttpException (Response LBS.ByteString))
  return $ Bifunctor.first exToErr respOrEx >>= parseResponse secretPath
  where
    exToErr :: HttpException -> VaultError
    exToErr e@(HttpExceptionRequest _ _) = ServerUnreachable e
    exToErr (InvalidUrlException _ _) = InvalidUrl secretPath

--
-- HTTP response handling
--

parseResponse :: String -> Response LBS.ByteString -> Either VaultError VaultData
parseResponse secretPath response =
  let
    responseBody = getResponseBody response
    statusCode = getResponseStatusCode response
  in case statusCode of
    200 -> Right $ parseSuccessResponse responseBody
    403 -> Left Forbidden
    404 -> Left $ SecretNotFound secretPath
    500 -> Left $ ServerError responseBody
    503 -> Left $ ServerUnavailable responseBody
    _   -> Left $ Unspecified statusCode responseBody


parseSuccessResponse :: LBS.ByteString -> VaultData
parseSuccessResponse responseBody =
  let
    getter = Lens.key "data" . reindexed Text.unpack Lens.members . Lens._String . to Text.unpack
  in
    Lens.toMapOf getter responseBody

--
-- Utility functions
--

vaultErrorLogMessage :: VaultError -> String
vaultErrorLogMessage vaultError =
  let
    description = case vaultError of
      (SecretNotFound secretPath) ->
        "Secret not found: " <> secretPath
      (IOError fp) ->
        "An I/O error happened while opening: " <> fp
      (ParseError fp) ->
        "File " <> fp <> " could not be parsed"
      (KeyNotFound secret) ->
        "Key " <> (sKey secret) <> " not found for path " <> (sPath secret)
      (DuplicateVar varName) ->
        "Found duplicate environment variable \"" ++ varName ++ "\""
      (BadRequest resp) ->
        "Made a bad request: " <> (LBS.unpack resp)
      (Forbidden) ->
        "Invalid Vault token"
      (InvalidUrl secretPath) ->
        "Secret " <> secretPath <> " contains characters that are illegal in URLs"
      (ServerError resp) ->
        "Internal Vault error: " <> (LBS.unpack resp)
      (ServerUnavailable resp) ->
        "Vault is unavailable for requests. It can be sealed, " <>
        "under maintenance or enduring heavy load: " <> (LBS.unpack resp)
      (ServerUnreachable exception) ->
        "ServerUnreachable error: " <> show exception
      (Unspecified status resp) ->
        "Received an error that I don't know about (" <> show status
        <> "): " <> (LBS.unpack resp)
  in
    "[ERROR] " <> description


-- | Convert a secret name into the name of the environment variable that it
-- will be available under.
varNameFromKey :: String -> String -> String
varNameFromKey path key = fmap format (path ++ "_" ++ key)
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore

-- | Like @splitAt@, but also removes the character at the split position.
cutAt :: Int -> [a] -> ([a], [a])
cutAt index xs =
  let
    (first, second) = splitAt index xs
  in
    (first, drop 1 second)

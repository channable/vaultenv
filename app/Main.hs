{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative    ((<|>))
import Control.Exception      (catch)
import Control.Monad          (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON, (.:))
import Data.Aeson.Types       (parseMaybe)
import Data.Bifunctor         (first)
import Data.HashMap.Strict    (HashMap, lookupDefault, mapMaybe)
import Data.List              (nubBy)
import Data.Text              (Text, pack, unpack)
import Network.Connection     (TLSSettings(..))
import Network.HTTP.Client    (defaultManagerSettings)
import Network.HTTP.Conduit   (Manager, newManager, mkManagerSettings)
import Network.HTTP.Simple    (HttpException(..), Request, Response,
                               defaultRequest, setRequestHeader, setRequestPort,
                               setRequestPath, setRequestHost, setRequestManager,
                               setRequestSecure, httpLBS, getResponseBody,
                               getResponseStatusCode)
import System.Environment     (getEnvironment)
import System.Posix.Process   (executeFile)

import qualified Control.Concurrent.Async   as Async
import qualified Control.Retry              as Retry
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy       as LBS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.Map                   as Map
import qualified Data.HashMap.Strict        as HM
import qualified System.Exit                as Exit

import Config (Options(..), parseOptions, unMilliSeconds,
               LogLevel(..), readConfigFromEnvFiles, getOptionsValue,
               Validated, Completed)
import SecretsFile (Secret(..), SFError(..), readSecretsFile)

-- | Make a HTTP URL path from a secret. This is the path that Vault expects.
secretRequestPath :: MountInfo -> Secret -> String
secretRequestPath (MountInfo mountInfo) secret = "/v1/" <> sMount secret <> foo <> sPath secret
  where
    foo = case lookupDefault KV1 (pack $ sMount secret <> "/") mountInfo of
      KV1 -> "/"
      KV2 -> "/data/"

type EnvVar = (String, String)

data MountInfo = MountInfo (HashMap Text EngineType)
  deriving (Show)

data Context
  = Context
  { cLocalEnvVars :: [EnvVar]
  , cCliOptions :: Options Validated Completed
  , cHttpManager :: Manager
  }

-- | The different types of Engine that Vautlenv supports
data EngineType = KV1 | KV2
  deriving (Show)

data VaultData = VaultData (HashMap String String)

-- Vault has two versions of the Key/Value backend. We try to parse both
-- response formats here and return the one which we can parse correctly.
--
-- This means there is some inference going on by vaultenv, but since
-- we can find out which format we're parsing unambiguously, we just go
-- with the inference instead of making the user specify this. The benefit
-- here is that users can upgrade the version of the KV secret engine
-- without having to change their config files.
--
-- KV version 1 looks like this:
--
-- @
-- {
--   "auth": null,
--   "data": {
--     "foo": "bar"
--   },
--   "lease_duration": 2764800,
--   "lease_id": "",
--   "renewable": false
-- }
-- @
--
-- And version 2 looks like this:
--
-- @
-- {
--   "data": {
--     "data": {
--       "foo": "bar"
--     },
--     "metadata": {
--       "created_time": "2018-03-22T02:24:06.945319214Z",
--       "deletion_time": "",
--       "destroyed": false,
--       "version": 1
--     }
--   },
-- }
-- @
instance FromJSON VaultData where
  parseJSON =
    let
      onlyStrings (Aeson.String t) = Just (unpack t)
      onlyStrings _ = Nothing
      parseV1 obj = do
        keyValuePairs <- obj .: "data"
        (VaultData . (mapMaybe onlyStrings)) <$> Aeson.parseJSON keyValuePairs
      parseV2 obj = do
        nested <- obj .: "data"
        flip (Aeson.withObject "nested") nested $ \obj' -> do
          keyValuePairs <- obj' .: "data"
          (VaultData . (mapMaybe onlyStrings)) <$> Aeson.parseJSON keyValuePairs
    in Aeson.withObject "object" $ \obj -> parseV1 obj <|> parseV2 obj

-- Parses a very mixed type of response that Vault gives back when you
-- request /sys/mounts. It has a garbage format. We primarily care about
-- this information:
--
-- {
--   "secret/": {
--     "options": {
--       "version": "1"
--     },
--     "type": "kv"
--   },
-- }
--
-- But there is a whole load of other stuff in the top level object.
-- Integers, dates, strings, null values. Get the stuff we need
-- and get out.
instance FromJSON MountInfo where
  parseJSON =
    let
      getType = Aeson.withObject "MountSpec" $ \o ->
        o .: "type" >>= (Aeson.withText "mount type" $ (\case
          "kv" -> do
            options <- o .: "options"
            Aeson.withObject "Options" (\opts -> do
              version <- opts .: "version"
              case version of
                Aeson.String "1" -> pure KV1
                Aeson.String "2" -> pure KV2
                _ -> fail "unknown version number") options
          _ -> fail "expected a KV type"))
    in
      Aeson.withObject "MountResp" $ \obj ->
        pure $ MountInfo (mapMaybe (\v -> parseMaybe getType v) obj)

-- | Error modes of this program.
--
-- Every part of the program that can fail has an error type. These can bubble
-- up the call stack and end up as a value of this type. We then have a single
-- function which is responsible for printing an error message and exiting.
data VaultError
  = SecretNotFound String
  | SecretFileError SFError
  | KeyNotFound Secret
  | BadRequest LBS.ByteString
  | Forbidden
  | BadJSONResp String
  | ServerError LBS.ByteString
  | ServerUnavailable LBS.ByteString
  | ServerUnreachable HttpException
  | InvalidUrl String
  | DuplicateVar String
  | Unspecified Int LBS.ByteString
  deriving Show

-- | "Handle" a HttpException by wrapping it in a Left VaultError.
-- We also edit the Request contained in the exception to remove the
-- Vault token, as it would otherwise get printed to stderr if we error
-- out.
httpErrorHandler :: HttpException -> IO (Either VaultError b)
httpErrorHandler (e :: HttpException) = case e of
  (HttpExceptionRequest request reason) ->
    let sanitizedRequest = sanitizeRequest request
    in pure $ Left $ ServerUnreachable (HttpExceptionRequest sanitizedRequest reason)
  (InvalidUrlException url _reason) -> pure $ Left $ InvalidUrl url

  where
    sanitizeRequest :: Request -> Request
    sanitizeRequest = setRequestHeader "x-vault-token" ["**removed**"]

-- | Retry configuration to use for network requests to Vault.
-- We use a limited exponential backoff with the policy
-- fullJitterBackoff that comes with the Retry package.
vaultRetryPolicy :: (MonadIO m) => Options Validated Completed -> Retry.RetryPolicyM m
vaultRetryPolicy opts = Retry.fullJitterBackoff (unMilliSeconds
                          (getOptionsValue oRetryBaseDelay opts) * 1000
                        )
                     <> Retry.limitRetries (
                          getOptionsValue oRetryAttempts opts
                        )

-- | Perform the given action according to our retry policy.
doWithRetries :: Retry.RetryPolicyM IO -> (Retry.RetryStatus -> IO (Either VaultError a)) -> IO (Either VaultError a)
doWithRetries retryPolicy = Retry.retrying retryPolicy isRetryableFailure
  where
    -- | Indicator function for retrying to retry on VaultErrors (Lefts) that
    -- shouldRetry thinks we should retry on. Needs to be in IO because the
    -- actions to perform are in IO as well.
    isRetryableFailure :: Retry.RetryStatus -> Either VaultError a -> IO Bool
    isRetryableFailure _retryStatus (Right _) = pure False
    isRetryableFailure _retryStatus (Left err) = pure $ case err of
      ServerError _ -> True
      ServerUnavailable _ -> True
      ServerUnreachable _ -> True
      Unspecified _ _ -> True
      BadJSONResp _ -> True

      -- Errors where we don't retry
      BadRequest _ -> False
      Forbidden -> False
      InvalidUrl _ -> False
      SecretNotFound _ -> False

      -- Errors that cannot occur at this point, but we list for
      -- exhaustiveness checking.
      KeyNotFound _ -> False
      DuplicateVar _ -> False
      SecretFileError _ -> False

--
-- IO
--

main :: IO ()
main = do
  localEnvVars <- getEnvironment
  envFileSettings <- readConfigFromEnvFiles

  cliAndEnvAndEnvFileOptions <- parseOptions localEnvVars envFileSettings

  let envAndEnvFileConfig = nubBy (\(x, _) (y, _) -> x == y)
                                  (localEnvVars ++ concat (reverse envFileSettings))

  if getOptionsValue oLogLevel cliAndEnvAndEnvFileOptions <= Info
    then print cliAndEnvAndEnvFileOptions
    else pure ()

  httpManager <- getHttpManager cliAndEnvAndEnvFileOptions

  let context = Context { cLocalEnvVars = envAndEnvFileConfig
                        , cCliOptions = cliAndEnvAndEnvFileOptions
                        , cHttpManager = httpManager
                        }

  vaultEnv context >>= \case
    Left err -> Exit.die (vaultErrorLogMessage err)
    Right newEnv -> runCommand cliAndEnvAndEnvFileOptions newEnv


-- | This function returns either a manager for plain HTTP or
-- for HTTPS connections. If TLS is wanted, we also check if the
-- user specified an option to disable the certificate check.
getHttpManager :: Options Validated Completed -> IO Manager
getHttpManager opts = newManager managerSettings
  where
    managerSettings = if getOptionsValue oConnectTls opts
                      then mkManagerSettings tlsSettings Nothing
                      else defaultManagerSettings
    tlsSettings = TLSSettingsSimple
                { settingDisableCertificateValidation =
                      not $ getOptionsValue oValidateCerts opts
                , settingDisableSession = False
                , settingUseServerName = True
                }

-- | Main logic of our application.
--
-- We first retrieve the mount information from Vault, as this is needed to
-- construct the URLs of the secrets to fetch.
-- With this information we fetch the secrets from Vault, check for duplicates,
-- and then yield the list of environment variables to make available to the
-- process we want to run eventually.
-- Based on the settings in the context we either scrub the environment that
-- already existed or keep it (applying the inheritance blacklist if it exists).
--
-- Signals failure through a value of type VaultError.
vaultEnv :: Context -> IO (Either VaultError [EnvVar])
vaultEnv context =
  doWithRetries retryPolicy getMountInfo >>= \case
    Left vaultError -> pure $ Left vaultError
    Right mountInfo ->
      readSecretsFile secretFile >>= \case
        Left sfError -> pure $ Left $ SecretFileError sfError
        Right secrets ->
          requestSecrets context mountInfo secrets >>= \case
            Left vaultError -> pure $ Left vaultError
            Right secretEnv -> pure $ checkNoDuplicates (buildEnv secretEnv)
    where
      retryPolicy = vaultRetryPolicy (cCliOptions context)

      getMountInfo :: Retry.RetryStatus -> IO (Either VaultError MountInfo)
      getMountInfo _retryStatus = catch (requestMountInfo context) httpErrorHandler

      secretFile = getOptionsValue oSecretFile (cCliOptions context)

      -- | Check that the given list of EnvVars contains no duplicate
      -- variables, return a DuplicateVar error if it does.
      checkNoDuplicates :: [EnvVar] -> Either VaultError [EnvVar]
      checkNoDuplicates vars = case dups (map fst vars) of
        Right () -> Right vars
        Left var -> Left $ DuplicateVar var

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

      -- | Build the resulting environment for the process to start, given the
      -- list of environment variables that were retrieved from Vault.  Return
      -- either only the retrieved secrets (if --no-inherit-env is used), or
      -- merge the retrieved variables with the environment where Vaultenv was
      -- called and apply the blacklist.
      buildEnv :: [EnvVar] -> [EnvVar]
      buildEnv secretsEnv =
        if getOptionsValue oInheritEnv . cCliOptions $ context
        then removeBlacklistedVars $ secretsEnv ++ cLocalEnvVars context
        else secretsEnv

        where
          inheritEnvBlacklist = getOptionsValue oInheritEnvBlacklist . cCliOptions $ context
          removeBlacklistedVars = filter (not . flip elem inheritEnvBlacklist . fst)


runCommand :: Options Validated Completed -> [EnvVar] -> IO a
runCommand options env =
  let
    command = getOptionsValue oCmd options
    searchPath = getOptionsValue oUsePath options
    args = getOptionsValue oArgs options
    env' = Just env
  in
    -- `executeFile` calls one of the syscalls in the execv* family, which
    -- replaces the current process with `command`. It does not return.
    executeFile command searchPath args env'


-- | Add Vault authentication token to a request, if set in the options. If
-- not, the request is returned unmodified.
addVaultToken :: Options Validated Completed -> Request -> Request
addVaultToken options request =
  let
    optToken = oVaultToken options
  in
    case optToken of
      Just token -> setRequestHeader "x-vault-token" [SBS.pack token] request
      Nothing -> request


-- | Look up what mounts are available and what type they have.
requestMountInfo :: Context -> IO (Either VaultError MountInfo)
requestMountInfo context =
  let
    cliOptions = cCliOptions context
    request
        = setRequestManager
              (cHttpManager context)
        $ addVaultToken cliOptions
        $ setRequestHeader "x-vault-request" ["true"]
        $ setRequestPath
              (SBS.pack "/v1/sys/mounts")
        $ setRequestPort
              (getOptionsValue oVaultPort cliOptions)
        $ setRequestHost
              (SBS.pack (getOptionsValue oVaultHost cliOptions))
        $ setRequestSecure
              (getOptionsValue  oConnectTls cliOptions)
        $ defaultRequest
  in do
    -- 'httpLBS' throws an IO Exception ('HttpException') if it fails to complete the request.
    -- We intentionally don't capture this here, but handle it with the retries in 'vaultEnv' instead.
    resp <- httpLBS request
    let decodeResult = Aeson.eitherDecode' (getResponseBody resp) :: Either String MountInfo

    case decodeResult of
      Left errorMsg -> pure $ Left $ BadJSONResp errorMsg
      Right result -> pure $ Right result

-- | Request all the data associated with a secret from the vault.
--
-- This function automatically retries the request if it fails according to the
-- retryPolicy set in the given context.
requestSecret :: Context -> String -> IO (Either VaultError VaultData)
requestSecret context secretPath =
  let
    cliOptions = cCliOptions context
    retryPolicy = vaultRetryPolicy (cCliOptions context)
    request
        = setRequestManager
            (cHttpManager context)
        $ addVaultToken     cliOptions
        $ setRequestHeader "x-vault-request" ["true"]
        $ setRequestPath    (SBS.pack secretPath)
        $ setRequestPort    (getOptionsValue oVaultPort cliOptions)
        $ setRequestHost    (SBS.pack (getOptionsValue oVaultHost cliOptions))
        $ setRequestSecure  (getOptionsValue oConnectTls cliOptions)
        $ defaultRequest

    getSecret :: Retry.RetryStatus -> IO (Either VaultError VaultData)
    getSecret _retryStatus = catch (doRequest secretPath request) httpErrorHandler

  in
    doWithRetries retryPolicy getSecret

-- | Request all the supplied secrets from the vault, but just once, even if
-- multiple keys are specified for a single secret. This is an optimization in
-- order to avoid unnecessary round trips and DNS requests.
requestSecrets :: Context -> MountInfo -> [Secret] -> IO (Either VaultError [EnvVar])
requestSecrets context mountInfo secrets = do
  let secretPaths = Foldable.foldMap (\x -> Map.singleton x x) $ fmap (secretRequestPath mountInfo) secrets
  secretData <- liftIO (Async.mapConcurrently (requestSecret context) secretPaths)
  pure $ sequence secretData >>= lookupSecrets mountInfo secrets

-- | Look for the requested keys in the secret data that has been previously fetched.
lookupSecrets :: MountInfo -> [Secret] -> Map.Map String VaultData -> Either VaultError [EnvVar]
lookupSecrets mountInfo secrets vaultData = forM secrets $ \secret ->
  let secretData = Map.lookup (secretRequestPath mountInfo secret) vaultData
      secretValue = secretData >>= (\(VaultData vd) -> HM.lookup (sKey secret) vd)
      toEnvVar val = (sVarName secret, val)
  in maybe (Left $ KeyNotFound secret) (Right . toEnvVar) $ secretValue

-- | Send a request for secrets to the vault and parse the response.
doRequest :: String -> Request -> IO (Either VaultError VaultData)
doRequest secretPath request = do
  -- As in 'requestMountInfo': 'httpLBS' throws a 'HttpException' in IO if it
  -- fails to complete the request, this is handled in 'vaultEnv' by the retry logic.
  resp <- httpLBS request
  pure $ parseResponse secretPath resp

--
-- HTTP response handling
--

parseResponse :: String -> Response LBS.ByteString -> Either VaultError VaultData
parseResponse secretPath response =
  let
    responseBody = getResponseBody response
    statusCode = getResponseStatusCode response
  in case statusCode of
    200 -> parseSuccessResponse responseBody
    400 -> Left $ BadRequest responseBody
    403 -> Left Forbidden
    404 -> Left $ SecretNotFound secretPath
    500 -> Left $ ServerError responseBody
    503 -> Left $ ServerUnavailable responseBody
    _   -> Left $ Unspecified statusCode responseBody


parseSuccessResponse :: LBS.ByteString -> Either VaultError VaultData
parseSuccessResponse responseBody = first BadJSONResp $ Aeson.eitherDecode' responseBody

--
-- Utility functions
--

vaultErrorLogMessage :: VaultError -> String
vaultErrorLogMessage vaultError =
  let
    description = case vaultError of
      SecretNotFound secretPath ->
        "Secret not found: " <> secretPath
      SecretFileError sfe -> show sfe
      KeyNotFound secret ->
        "Key " <> (sKey secret) <> " not found for path " <> (sPath secret)
      DuplicateVar varName ->
        "Found duplicate environment variable \"" ++ varName ++ "\""
      BadRequest resp ->
        "Made a bad request: " <> (LBS.unpack resp)
      Forbidden ->
        "Invalid Vault token"
      InvalidUrl secretPath ->
        "Secret " <> secretPath <> " contains characters that are illegal in URLs"
      BadJSONResp msg ->
        "Received bad JSON from Vault: " <> msg
      ServerError resp ->
        "Internal Vault error: " <> (LBS.unpack resp)
      ServerUnavailable resp ->
        "Vault is unavailable for requests. It can be sealed, " <>
        "under maintenance or enduring heavy load: " <> (LBS.unpack resp)
      ServerUnreachable exception ->
        "ServerUnreachable error: " <> show exception
      Unspecified status resp ->
        "Received an error that I don't know about (" <> show status
        <> "): " <> (LBS.unpack resp)
  in
    "[ERROR] " <> description

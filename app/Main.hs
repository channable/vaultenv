{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative    ((<|>))
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception      (Handler (..), bracket_, catch, catches)
import Control.Monad          (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON, (.:))
import Data.Aeson.Types       (parseMaybe)
import Data.Bifunctor         (first)
import Data.HashMap.Strict    (HashMap)
import Data.List              (nubBy)
import Data.Text              (Text, unpack)
import Network.HTTP.Client    (ManagerSettings (managerConnCount))
import Network.HTTP.Conduit   (Manager, newManager)
import Network.HTTP.Simple    (HttpException(..), Request, Response,
                               defaultRequest, setRequestBodyJSON, setRequestHeader,
                               setRequestMethod, setRequestPort,
                               setRequestPath, setRequestHost, setRequestManager,
                               setRequestSecure, httpLBS, getResponseBody,
                               getResponseStatusCode)
import Network.HTTP.Client.OpenSSL (defaultMakeContext, defaultOpenSSLSettings,
                                     opensslManagerSettings, osslSettingsVerifyMode)
import OpenSSL.Session        (contextSetDefaultVerifyPaths,
                               VerificationMode (VerifyNone, VerifyPeer),
                               vpFailIfNoPeerCert, vpClientOnce, vpCallback)
import System.Environment     (getEnvironment)
import System.IO              (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix.Process   (executeFile)

import qualified Control.Concurrent.Async   as Async
import qualified Control.Retry              as Retry
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy       as LBS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Foldable              as Foldable
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map                   as Map
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text
import qualified System.Exit                as Exit

import Config (AuthMethod (..), Options(..), parseOptions, unMilliSeconds,
               LogLevel(..), readConfigFromEnvFiles, getOptionsValue,
               Validated, Completed, DuplicateVariableBehavior (..))
import KeyMap (KeyMap)
import SecretsFile (Secret(..), SFError(..), readSecretsFile)
import Response (ClientToken (..))

import qualified KeyMap as KM

-- | Make a HTTP URL path from a secret. This is the path that Vault expects.
secretRequestPath :: MountInfo -> Secret -> String
secretRequestPath (MountInfo mountInfo) secret = "/v1/" <> sMount secret <> foo <> sPath secret
  where
    foo = case KM.lookupDefault KV1 (KM.fromString $ sMount secret <> "/") mountInfo of
      KV1 -> "/"
      KV2 -> "/data/"

type EnvVar = (String, String)

data MountInfo = MountInfo (KeyMap EngineType)
  deriving (Show)

data Context
  = Context
  { cLocalEnvVars :: [EnvVar]
  , cCliOptions :: Options Validated Completed
  , cHttpManager :: Manager
  , cExtraEnvVars :: [EnvVar]
  -- ^ Variables we want to inject into the command's environment that were not
  -- in the local environment when vaultenv was called, nor fetched via vault
  }

-- | The different types of Engine that Vautlenv supports
data EngineType = KV1 | KV2
  deriving (Show)

data VaultData = VaultData (HashMap String Aeson.Value)

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
      parseV1 obj = do
        keyValuePairs <- obj .: "data"
        VaultData <$> Aeson.parseJSON keyValuePairs
      parseV2 obj = do
        nested <- obj .: "data"
        flip (Aeson.withObject "nested") nested $ \obj' -> do
          keyValuePairs <- obj' .: "data"
          VaultData <$> Aeson.parseJSON keyValuePairs
    in Aeson.withObject "VaultData" $ \obj -> parseV2 obj <|> parseV1 obj

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
        pure $ MountInfo (KM.mapMaybe (\v -> parseMaybe getType v) obj)

-- | Error modes of this program.
--
-- Every part of the program that can fail has an error type. These can bubble
-- up the call stack and end up as a value of this type. We then have a single
-- function which is responsible for printing an error message and exiting.
data VaultError
  = SecretNotFound String
  | SecretFileError SFError
  | KeyNotFound Secret
  | WrongType Secret
  | BadRequest LBS.ByteString
  | Forbidden
  | BadJSONResp LBS.ByteString String -- Body and error
  | ServerError LBS.ByteString
  | ServerUnavailable LBS.ByteString
  | ServerUnreachable HttpException
  | InvalidUrl String
  | DuplicateVar String
  | Unspecified Int LBS.ByteString
  | KubernetesJwtInvalidUtf8
  | KubernetesJwtFailedRead
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
      BadJSONResp _ _ -> True

      -- Errors where we don't retry
      BadRequest _ -> False
      Forbidden -> False
      InvalidUrl _ -> False
      SecretNotFound _ -> False
      KubernetesJwtInvalidUtf8 -> False
      KubernetesJwtFailedRead -> False

      -- Errors that cannot occur at this point, but we list for
      -- exhaustiveness checking.
      KeyNotFound _ -> False
      DuplicateVar _ -> False
      SecretFileError _ -> False
      WrongType _ -> False

--
-- IO
--

main :: IO ()
main = do
  -- When the runtime detects that stdout is not connected to a console, it
  -- defaults to block buffering instead of line buffering. When running under
  -- systemd or a container manager, this prevents log messages from showing up
  -- until the buffer is flushed, and it breaks interleaving of stdout and
  -- stderr print. Therefore, explicitly select line buffering, to enforce a
  -- flush after every newline.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

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
                        , cExtraEnvVars = []
                        }

  vaultEnv context >>= \case
    Left err -> Exit.die (vaultErrorLogMessage err)
    Right newEnv -> runCommand cliAndEnvAndEnvFileOptions newEnv


-- | This function returns either a manager for plain HTTP or
-- for HTTPS connections. If TLS is wanted, we also check if the
-- user specified an option to disable the certificate check.
getHttpManager :: Options Validated Completed -> IO Manager
getHttpManager opts = newManager $ applyConfig basicManagerSettings
  where
    maxConnections = getOptionsValue oMaxConcurrentRequests opts
    applyConfig settings = settings
      -- Allow the manager to keep as many connections live as were requested.
      -- Unless we use the unlimited flag, in that case, use the default value.
      { managerConnCount = if maxConnections > 0 then maxConnections else managerConnCount settings
      }
    basicManagerSettings =
      (opensslManagerSettings makeContext)
        { managerConnCount = maxConnections }
    makeContext = do
      context <- defaultMakeContext opensslSettings
      contextSetDefaultVerifyPaths context
      pure context
    opensslSettings = defaultOpenSSLSettings
      { osslSettingsVerifyMode =
            if not $ getOptionsValue oValidateCerts opts
            then VerifyNone
            else VerifyPeer
                  {
                    vpFailIfNoPeerCert = True,
                    vpClientOnce = True,
                    vpCallback = Nothing
                  }
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
vaultEnv originalContext =
  readSecretsFile secretFile >>= \case
    Left sfError -> pure $ Left $ SecretFileError sfError
    Right secrets ->
      doWithRetries retryPolicy (authenticate originalContext) >>= \case
        Left vaultError -> pure $ Left vaultError
        Right authenticatedContext ->
          doWithRetries retryPolicy (getMountInfo authenticatedContext) >>= \case
            Left vaultError -> pure $ Left vaultError
            Right mountInfo ->
              requestSecrets authenticatedContext mountInfo secrets >>= \case
                Left vaultError -> pure $ Left vaultError
                Right secretEnv -> pure $ handleDuplicates $
                  buildEnv (cExtraEnvVars authenticatedContext ++ secretEnv)
    where
      retryPolicy = vaultRetryPolicy (cCliOptions originalContext)

      authenticate :: Context -> Retry.RetryStatus -> IO (Either VaultError Context)
      authenticate context _retryStatus = case oAuthMethod (cCliOptions context) of
        -- If we have a token already, or if we should not authenticate at all,
        -- then there is nothing to be done here. But if Kubernetes or Github
        -- auth is enabled, then we do that here and then fill out the token.
        AuthVaultToken _ -> pure $ Right context
        AuthNone -> pure $ Right context
        AuthGitHub token -> handleVaultAuthResponse context (requestGitHubVaultToken context token)
        AuthKubernetes role -> handleVaultAuthResponse context (requestKubernetesVaultToken context role)

      handleVaultAuthResponse :: Context -> IO (Either VaultError ClientToken) -> IO (Either VaultError Context)
      handleVaultAuthResponse context f =
        catch f httpErrorHandler >>= \case
          Left vaultError -> pure $ Left vaultError
          Right (ClientToken token) -> do
            when (getOptionsValue oLogLevel (cCliOptions context) <= Info) $
              putStrLn "[INFO] Authentication successful, we have a VAULT_TOKEN now."
            pure $ Right context
              { cCliOptions = (cCliOptions context)
                { oAuthMethod = AuthVaultToken token
                }
              , cExtraEnvVars = [("VAULT_TOKEN", unpack token)]
              }

      getMountInfo :: Context -> Retry.RetryStatus -> IO (Either VaultError MountInfo)
      getMountInfo context _retryStatus = catch (requestMountInfo context) httpErrorHandler

      secretFile = getOptionsValue oSecretFile (cCliOptions originalContext)

      allowDuplicates = getOptionsValue oDuplicateVariablesBehavior (cCliOptions originalContext)

      -- Handle duplicates baseup
      handleDuplicates = case allowDuplicates of
        DuplicateError -> checkNoDuplicates
        DuplicateKeep -> Right . preferLast
        DuplicateOverwrite -> Right . preferFirst

      -- | Check that the given list of EnvVars contains no duplicate
      -- variables, return a DuplicateVar error if it does.
      checkNoDuplicates :: [EnvVar] -> Either VaultError [EnvVar]
      checkNoDuplicates vars = case dups (map fst vars) of
        Right () -> Right vars
        Left var -> Left $ DuplicateVar var

      -- Prefer the last element in the list with environment variables. This
      -- is a somewhat inefficient way, but the list here is small, so performance shouldn't
      -- be a problem.
      preferLast :: [EnvVar] -> [EnvVar]
      preferLast = reverse . preferFirst . reverse

      preferFirst :: [EnvVar] -> [EnvVar]
      preferFirst = nubBy (\(x, _) (y, _) -> x == y)

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
        if getOptionsValue oInheritEnv . cCliOptions $ originalContext
        then removeBlacklistedVars $ secretsEnv ++ cLocalEnvVars originalContext
        else secretsEnv

        where
          inheritEnvBlacklist = getOptionsValue oInheritEnvBlacklist . cCliOptions $ originalContext
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
addVaultToken options request = case oAuthMethod options of
  AuthVaultToken token -> setRequestHeader "x-vault-token" [Text.encodeUtf8 token] request
  AuthGitHub _ -> error "GitHub auth method should have been resolved to token by now."
  AuthKubernetes _ -> error "Kubernetes auth method should have been resolved to token by now."
  AuthNone -> request

unauthenticatedVaultRequest :: Context -> String -> Request
unauthenticatedVaultRequest context path =
  let
    cliOptions = cCliOptions context
  in
    setRequestManager (cHttpManager context)
    $ setRequestHeader "x-vault-request" ["true"]
    $ setRequestPath (SBS.pack path)
    $ setRequestPort (getOptionsValue oVaultPort cliOptions)
    $ setRequestHost (SBS.pack (getOptionsValue oVaultHost cliOptions))
    $ setRequestSecure (getOptionsValue  oConnectTls cliOptions)
    $ defaultRequest

kubernetesJwtPath :: FilePath
kubernetesJwtPath = "/var/run/secrets/kubernetes.io/serviceaccount/token"

-- | Read the contents of `/var/run/secrets/kubernetes.io/serviceaccount/token`.
readKubernetesJwt :: IO (Either VaultError Text)
readKubernetesJwt =
  let
    readJwtFile = do
      contentsUtf8 <- ByteString.readFile kubernetesJwtPath
      pure $ Right $ Text.decodeUtf8With Text.strictDecode contentsUtf8
  in
    readJwtFile `catches`
      [ Handler $ \(_ :: Text.UnicodeException) -> pure $ Left KubernetesJwtInvalidUtf8
      , Handler $ \(_ :: IOError) -> pure $ Left KubernetesJwtFailedRead
      ]

-- | Authenticate using GitHub auth, see https://www.vaultproject.io/docs/auth/github.
requestGitHubVaultToken :: Context -> Text -> IO (Either VaultError ClientToken)
requestGitHubVaultToken context ghtoken = let
  bodyJson = Aeson.Object $ KM.singleton "token" (Aeson.String ghtoken)
  request = setRequestBodyJSON bodyJson
    $ setRequestMethod "POST"
    $ unauthenticatedVaultRequest context "/v1/auth/github/login"
  in decodeVaultAuthResponse <$> httpLBS request

-- | Authenticate using Kubernetes auth, see https://www.vaultproject.io/docs/auth/kubernetes.
requestKubernetesVaultToken :: Context -> Text -> IO (Either VaultError ClientToken)
requestKubernetesVaultToken context role = do
  when (getOptionsValue oLogLevel (cCliOptions context) <= Info) $
    putStrLn "[INFO] Authenticating with Kubernetes ..."
  jwtResult <- readKubernetesJwt
  case jwtResult of
    Left err -> pure $ Left err
    Right jwt ->
      let
        bodyJson = Aeson.Object $ KM.fromList
          [ ("jwt", Aeson.String jwt)
          , ("role", Aeson.String role)
          ]
        request =
          setRequestBodyJSON bodyJson
          $ setRequestMethod "POST"
          $ unauthenticatedVaultRequest context "/v1/auth/kubernetes/login"
      in decodeVaultAuthResponse <$> httpLBS request

decodeVaultAuthResponse :: Response LBS.ByteString -> Either VaultError ClientToken
decodeVaultAuthResponse response =
  case getResponseStatusCode response of
    200 -> case Aeson.eitherDecode' (getResponseBody response) of
      Left err    -> Left $ BadJSONResp (getResponseBody response) err
      Right token -> Right token
    _notOk -> Left $ ServerError $ getResponseBody response

-- | Look up what mounts are available and what type they have.
requestMountInfo :: Context -> IO (Either VaultError MountInfo)
requestMountInfo context =
  let
    cliOptions = cCliOptions context
    request = addVaultToken cliOptions $ unauthenticatedVaultRequest context "/v1/sys/mounts"
  in do
    -- 'httpLBS' throws an IO Exception ('HttpException') if it fails to complete the request.
    -- We intentionally don't capture this here, but handle it with the retries in 'vaultEnv' instead.
    resp <- httpLBS request
    let decodeResult = Aeson.eitherDecode' (getResponseBody resp) :: Either String MountInfo

    case decodeResult of
      Left errorMsg -> pure $ Left $ BadJSONResp (getResponseBody resp) errorMsg
      Right result -> pure $ Right result

-- | Request all the data associated with a secret from the vault.
--
-- This function automatically retries the request if it fails according to the
-- retryPolicy set in the given context.
requestSecret :: Context -> String -> IO (Either VaultError VaultData)
requestSecret context secretPath =
  let
    cliOptions = cCliOptions context
    retryPolicy = vaultRetryPolicy cliOptions
    request = addVaultToken cliOptions $ unauthenticatedVaultRequest context secretPath

    getSecret :: Retry.RetryStatus -> IO (Either VaultError VaultData)
    getSecret _retryStatus = catch (doRequest secretPath request) httpErrorHandler

  in do
    when (getOptionsValue oLogLevel (cCliOptions context) <= Info) $
      putStrLn $ "[INFO] Getting " <> secretPath <> " ..."
    doWithRetries retryPolicy getSecret

-- | Request all the supplied secrets from the vault, but just once, even if
-- multiple keys are specified for a single secret. This is an optimization in
-- order to avoid unnecessary round trips and DNS requests.
requestSecrets :: Context -> MountInfo -> [Secret] -> IO (Either VaultError [EnvVar])
requestSecrets context mountInfo secrets = do
  let
    secretPaths = Foldable.foldMap (\x -> Map.singleton x x) $ fmap (secretRequestPath mountInfo) secrets
    concurrentRequests = getOptionsValue oMaxConcurrentRequests (cCliOptions context)

  -- Limit the number of concurrent requests with a semaphore
  requestSemaphore <- newQSem concurrentRequests
  let
    withSemaphore
      | concurrentRequests == 0 = id
      | otherwise = bracket_ (waitQSem requestSemaphore) (signalQSem requestSemaphore)

  secretData <- liftIO (Async.mapConcurrently (withSemaphore . requestSecret context) secretPaths)
  pure $ sequence secretData >>= lookupSecrets mountInfo secrets

-- | Look for the requested keys in the secret data that has been previously fetched.
lookupSecrets :: MountInfo -> [Secret] -> Map.Map String VaultData -> Either VaultError [EnvVar]
lookupSecrets mountInfo secrets vaultData = forM secrets $ \secret ->
  let secretData = Map.lookup (secretRequestPath mountInfo secret) vaultData
      secretValue = secretData >>= (\(VaultData vd) -> HashMap.lookup (sKey secret) vd)
      toEnvVar (Aeson.String val) = Right (sVarName secret, unpack val)
      toEnvVar _                  = Left (WrongType secret)
  in maybe (Left $ KeyNotFound secret) toEnvVar $ secretValue

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
parseSuccessResponse responseBody = first
  (BadJSONResp responseBody)
  (Aeson.eitherDecode' responseBody)

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
      WrongType secret ->
        "Key " <> (sKey secret) <> " in path " <> (sPath secret) <> " is not a String"
      DuplicateVar varName ->
        "Found duplicate environment variable \"" ++ varName ++ "\""
      BadRequest resp ->
        "Made a bad request: " <> (LBS.unpack resp)
      Forbidden ->
        "Forbidden: The provided token is not valid or expired. If you're not \
        \explictly passing a token, check if the configured token in \
        \`~/.config/vaultenv/vaultenv.conf` is still valid"
      InvalidUrl secretPath ->
        "Secret " <> secretPath <> " contains characters that are illegal in URLs"
      BadJSONResp body msg ->
        "Received bad JSON from Vault: " <> msg <> ". Response was: " <> (LBS.unpack body)
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
      KubernetesJwtFailedRead ->
        "Failed to read '" <> kubernetesJwtPath <> "'."
      KubernetesJwtInvalidUtf8 ->
        "Contents of '" <> kubernetesJwtPath <> "' is not valid UTF-8."
  in
    "[ERROR] " <> description

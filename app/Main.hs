{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad          (forM)
import Control.Lens           (reindexed, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor         (first)
import Data.List              (nubBy)
import Data.Monoid            ((<>))
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
import Control.Monad.Except   (ExceptT, MonadError, runExceptT, mapExceptT, throwError)

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
import qualified System.Exit                as Exit

import Config (Options(..), parseOptionsFromEnvAndCli, unMilliSeconds,
               LogLevel(..), readConfigFromEnvFiles)
import SecretsFile (Secret(..), SFError(..), readSecretList)

-- | Make a HTTP URL path from a secret. This is the path that Vault expects.
secretRequestPath :: Secret -> String
secretRequestPath secret = "/v1/" <> sMount secret <> "/" <> sPath secret

type EnvVar = (String, String)

data Context
  = Context
  { cLocalEnvVars :: [EnvVar]
  , cCliOptions :: Options
  , cHttpManager :: Manager
  }

type VaultData = Map.Map String String

-- | Error modes of this program.
--
-- Every part of the program that can fail has an error type. These can bubble
-- up the call stack and end up as a value of this type. We then have a single
-- function which is responsible for printing an error message and exiting.
data VaultError
  = SecretNotFound    String
  | SecretFileError   SFError
  | KeyNotFound       Secret
  | BadRequest        LBS.ByteString
  | Forbidden
  | ServerError       LBS.ByteString
  | ServerUnavailable LBS.ByteString
  | ServerUnreachable HttpException
  | InvalidUrl        String
  | DuplicateVar      String
  | Unspecified       Int LBS.ByteString

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
  envFileSettings <- readConfigFromEnvFiles

  -- Deduplicate, give precedence to set env vars over .env files
  let envAndEnvFileConfig = nubBy (\(x, _) (y, _) -> x == y) (localEnvVars ++ envFileSettings)

  cliAndEnvAndEnvFileOptions <- parseOptionsFromEnvAndCli envAndEnvFileConfig

  if (oLogLevel cliAndEnvAndEnvFileOptions) <= Info
    then print cliAndEnvAndEnvFileOptions
    else pure ()

  httpManager <- getHttpManager cliAndEnvAndEnvFileOptions

  let context = Context { cLocalEnvVars = envAndEnvFileConfig
                        , cCliOptions = cliAndEnvAndEnvFileOptions
                        , cHttpManager = httpManager
                        }

  runExceptT (vaultEnv context) >>= \case
    Left err -> Exit.die (vaultErrorLogMessage err)
    Right newEnv -> runCommand cliAndEnvAndEnvFileOptions newEnv

-- | This function returns either a manager for plain HTTP or
-- for HTTPS connections. If TLS is wanted, we also check if the
-- user specified an option to disable the certificate check.
getHttpManager :: Options -> IO Manager
getHttpManager opts = newManager managerSettings
  where
    managerSettings = if oConnectTls opts
                      then mkManagerSettings tlsSettings Nothing
                      else defaultManagerSettings
    tlsSettings = TLSSettingsSimple
                { settingDisableCertificateValidation = not $ oValidateCerts opts
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
  secrets <- mapExceptT (fmap $ first SecretFileError) $ readSecretList secretFile
  secretEnv <- requestSecrets context secrets
  checkNoDuplicates (buildEnv secretEnv)
    where
      secretFile = oSecretFile (cCliOptions context)
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
        if (oInheritEnv . cCliOptions $ context)
        then secretsEnv ++ (cLocalEnvVars context)
        else secretsEnv


runCommand :: Options -> [EnvVar] -> IO a
runCommand options env =
  let
    command = oCmd options
    searchPath = oUsePath options
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
    request = setRequestManager (cHttpManager context)
            $ setRequestHeader "x-vault-token" [SBS.pack (oVaultToken cliOptions)]
            $ setRequestPath (SBS.pack secretPath)
            $ setRequestPort (oVaultPort cliOptions)
            $ setRequestHost (SBS.pack (oVaultHost cliOptions))
            $ setRequestSecure (oConnectTls cliOptions)
            $ defaultRequest

    -- Only retry on connection related failures
    shouldRetry _retryStatus (Right _) = return False
    shouldRetry _retryStatus (Left e) =
      return $ case e of
        ServerError _ -> True
        ServerUnavailable _ -> True
        ServerUnreachable _ -> True
        Unspecified _ _ -> True

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

    retryAction _retryStatus = doRequest secretPath request
  in
    Retry.retrying (vaultRetryPolicy cliOptions) shouldRetry retryAction

-- | Request all the supplied secrets from the vault, but just once, even if
-- multiple keys are specified for a single secret. This is an optimization in
-- order to avoid unnecessary round trips and DNS requets.
requestSecrets :: Context -> [Secret] -> (ExceptT VaultError IO) [EnvVar]
requestSecrets context secrets = do
  let secretPaths = Foldable.foldMap (\x -> Map.singleton x x) $ fmap secretRequestPath secrets
  secretDataOrErr <- liftIO $ Async.mapConcurrently (requestSecret context) secretPaths
  either throwError return $ sequence secretDataOrErr >>= lookupSecrets secrets

-- | Look for the requested keys in the secret data that has been previously fetched.
lookupSecrets :: [Secret] -> Map.Map String VaultData -> Either VaultError [EnvVar]
lookupSecrets secrets vaultData = forM secrets $ \secret ->
  let secretData = Map.lookup (secretRequestPath secret) vaultData
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

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens           (preview)
import Control.Monad.IO.Class (MonadIO)
import Data.Char
import Data.Either            (isLeft)
import Data.List              (findIndex)
import Data.Monoid            ((<>))
import Network.HTTP.Simple
import Options.Applicative    hiding (Parser, command)
import System.Environment
import System.Posix.Process
import System.IO              (stderr, hPutStrLn)

import qualified Control.Concurrent.Async   as Async
import qualified Control.Exception          as Exception
import qualified Control.Retry              as Retry
import qualified Data.Aeson.Lens            as Lens (key, _String)
import qualified Data.Bifunctor             as Bifunctor
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy       as LBS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import qualified Options.Applicative        as O

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
  , oConnectInsecure :: Bool
  , oInheritEnvOff   :: Bool
  } deriving (Eq, Show)

data Secret = Secret
  { sPath    :: String
  , sKey     :: String
  , sVarName :: String
  } deriving (Eq, Show)

type EnvVar = (String, String)

data VaultError
  = SecretNotFound    Secret
  | KeyNotFound       Secret
  | BadRequest        LBS.ByteString
  | Forbidden
  | ServerError       LBS.ByteString
  | ServerUnavailable LBS.ByteString
  | ServerUnreachable
  | InvalidUrl        Secret
  | DuplicateVar      String
  | Unspecified       Int LBS.ByteString

--
-- Argument parsing
--

optionsParser :: O.Parser Options
optionsParser = Options
       <$> strOption
           (  long "host"
           <> metavar "HOST"
           <> value "localhost"
           <> help "Vault host, either an IP address or DNS name, defaults to localhost" )
       <*> option auto
           (  long "port"
           <> metavar "PORT"
           <> value 8200
           <> help "Vault port, defaults to 8200" )
       <*> strOption
           (  long "token"
           <> metavar "TOKEN"
           <> help "token to authenticate to Vault with")
       <*> strOption
           (  long "secrets-file"
           <> metavar "FILENAME"
           <> help "config file specifying which secrets to request" )
       <*> argument str
           (  metavar "CMD"
           <> help "command to run after fetching secrets")
       <*> many (argument str
           (  metavar "ARGS..."
           <> help "arguments to pass to CMD, defaults to nothing"))
       <*> switch
           (  long "no-connect-tls"
           <> help "don't use TLS when connecting to Vault (default: use TLS)")
       <*> switch
           (  long "no-inherit-env"
           <> help "don't merge the parent environment with the secrets file")

-- Adds metadata to the `options` parser so it can be used with
-- execParser.
optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    (fullDesc <> header "vaultenv - run programs with secrets from HashiCorp Vault")

-- | Retry configuration to use for network requests to Vault.
-- We use a limited exponential backoff with the policy
-- fullJitterBackoff that comes with the Retry package.
vaultRetryPolicy :: (MonadIO m) => Retry.RetryPolicyM m
vaultRetryPolicy =
  let
    -- Try at most 10 times in total
    maxRetries = 9
    -- The base delay is 40 milliseconds because, in testing,
    -- we found out that fetching 50 secrets takes roughly
    -- 200 milliseconds.
    baseDelayMicroSeconds = 40000
  in Retry.fullJitterBackoff baseDelayMicroSeconds
  <> Retry.limitRetries maxRetries

--
-- IO
--

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser optionsInfo

  secretsOrError <- readSecretList (oSecretFile opts)
  case secretsOrError of
    Left err -> hPutStrLn stderr err
    Right ss -> do
      newEnvOrErrors <- Async.mapConcurrently (requestSecret opts) ss
      case sequence newEnvOrErrors of
        -- We need to check duplicates in the environment and fail if
        -- there are any. `dups` runs in O(n^2),
        -- but this shouldn't matter for our small lists.
        --
        -- Equality is determined on the first element of the env var
        -- tuples.
        Right e ->
          let
            newEnv = if oInheritEnvOff opts then e else e ++ env
          in case checkNoDuplicates newEnv of
            Left varName -> hPutStrLn stderr $ vaultErrorLogMessage (DuplicateVar varName)
            Right () -> runCommand opts newEnv
        Left err -> hPutStrLn stderr (vaultErrorLogMessage err)
    where
      checkNoDuplicates e =
        let
          keys = map fst e
        in
          dups keys

      dups :: Eq a => [a] -> Either a ()
      dups [] = Right ()
      dups (x:xs) | isDup x xs = Left x
                  | otherwise = dups xs

      isDup x = foldr (\y acc -> acc || x == y) False


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


readSecretList :: FilePath -> IO (Either String [Secret])
readSecretList fname = fmap (sequence . fmap parseSecret . lines) (readFile fname)


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


requestSecret :: Options -> Secret -> IO (Either VaultError EnvVar)
requestSecret opts secret =
  let
    requestPath = "/v1/secret/" <> (sPath secret)
    request = setRequestHeader "x-vault-token" [SBS.pack (oVaultToken opts)]
            $ setRequestPath (SBS.pack requestPath)
            $ setRequestPort (oVaultPort opts)
            $ setRequestHost (SBS.pack (oVaultHost opts))
            $ setRequestSecure (not $ oConnectInsecure opts)
            $ defaultRequest

    shouldRetry = const $ return . isLeft
    retryAction _retryStatus = doRequest secret request
  in
    Retry.retrying vaultRetryPolicy shouldRetry retryAction


doRequest :: Secret -> Request -> IO (Either VaultError EnvVar)
doRequest secret request = do
  respOrEx <- Exception.try . httpLBS $ request :: IO (Either HttpException (Response LBS.ByteString))
  let envVarOrErr = Bifunctor.first exToErr respOrEx >>= (parseResponse secret)

  return envVarOrErr

  where
    exToErr (HttpExceptionRequest _ _) = ServerUnreachable
    exToErr (InvalidUrlException _ _) = InvalidUrl secret

--
-- HTTP response handling
--

parseResponse :: Secret -> Response LBS.ByteString -> Either VaultError EnvVar
parseResponse secret response =
  let
    responseBody = getResponseBody response
    statusCode = getResponseStatusCode response
  in case statusCode of
    200 -> parseSuccessResponse secret responseBody
    403 -> Left Forbidden
    404 -> Left $ SecretNotFound secret
    500 -> Left $ ServerError responseBody
    503 -> Left $ ServerUnavailable responseBody
    _   -> Left $ Unspecified statusCode responseBody


parseSuccessResponse :: Secret -> LBS.ByteString -> Either VaultError EnvVar
parseSuccessResponse secret responseBody =
  let
    secretKey = Text.pack (sKey secret)
    getter = Lens.key "data" . Lens.key secretKey . Lens._String
    toEnvVar secretValue = (sVarName secret, Text.unpack secretValue)
  in
    maybeToEither (KeyNotFound secret) $ fmap toEnvVar (preview getter responseBody)

--
-- Utility functions
--

vaultErrorLogMessage :: VaultError -> String
vaultErrorLogMessage vaultError =
  let
    description = case vaultError of
      (SecretNotFound secret) ->
        "Secret not found: " <> sPath secret
      (KeyNotFound secret) ->
        "Key " <> (sKey secret) <> " not found for path " <> (sPath secret)
      (DuplicateVar varName) ->
        "Found duplicate environment variable \"" ++ varName ++ "\""
      (BadRequest resp) ->
        "Made a bad request: " <> (LBS.unpack resp)
      (Forbidden) ->
        "Invalid Vault token"
      (InvalidUrl secret) ->
        "Secret " <> (sPath secret) <> " contains characters that are illegal in URLs"
      (ServerError resp) ->
        "Internal Vault error: " <> (LBS.unpack resp)
      (ServerUnavailable resp) ->
        "Vault is unavailable for requests. It can be sealed, " <>
        "under maintenance or enduring heavy load: " <> (LBS.unpack resp)
      (ServerUnreachable) ->
        "Network trouble. Host can be unreachable, requests may be " <>
        "timing out or DNS not working."
      (Unspecified status resp) ->
        "Received an error that I don't know about (" <> show status
        <> "): " <> (LBS.unpack resp)
  in
    "[ERROR] " <> description


-- Converts a secret name into the name of the environment variable that it
-- will be available under.
varNameFromKey :: String -> String -> String
varNameFromKey path key = fmap format (path ++ "_" ++ key)
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore


maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e


-- Like splitAt, but also removes the character at the split position.
cutAt :: Int -> [a] -> ([a], [a])
cutAt index xs =
  let
    (first, second) = splitAt index xs
  in
    (first, drop 1 second)

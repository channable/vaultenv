{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent   (threadDelay)
import Control.Lens         (preview)
import Data.Char
import Data.List            (findIndex, nubBy)
import Data.Semigroup       ((<>))
import Network.HTTP.Simple
import Options.Applicative  hiding (Parser, command)
import System.Environment
import System.Posix.Process
import System.Random        (getStdRandom, randomR)

import qualified Control.Concurrent.Async   as Async
import qualified Control.Exception          as Exception
import qualified Data.Aeson.Lens            as Lens (key, _String)
import qualified Data.ByteString.Char8      as SBS
import qualified Data.ByteString.Lazy       as LBS hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import qualified Options.Applicative        as O

--
-- Datatypes
--

data Options = Options
  { oVaultHost   :: String
  , oVaultPort   :: Int
  , oVaultToken  :: String
  , oSecretFile  :: FilePath
  , oCmd         :: String
  , oArgs        :: [String]
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


-- Adds metadata to the `options` parser so it can be used with
-- execParser.
optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    (fullDesc <> header "vaultenv - run programs with secrets from HashiCorp Vault")

--
-- IO
--

main :: IO ()
main = do
  env <- getEnvironment
  opts <- execParser optionsInfo

  secretsOrError <- readSecretList (oSecretFile opts)
  let
    secrets = case secretsOrError of
      Right ss -> ss
      Left err -> errorWithoutStackTrace err

  newEnvOrErrors <- Async.mapConcurrently (requestSecret opts) secrets

  let
    newEnv = case sequence newEnvOrErrors of
      -- We need to eliminate duplicates in the environment and keep
      -- the first occurrence. `nubBy` (from Data.List) runs in O(n^2),
      -- but this shouldn't matter for our small lists.
      --
      -- Equality is determined on the first element of the env var
      -- tuples.
      Right e -> nubBy (\(a,_) (b,_) -> a == b) (e ++ env)
      Left err -> errorWithoutStackTrace (vaultErrorLogMessage err)

  runCommand opts newEnv


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
            $ defaultRequest

  in do
    responseOrErr <- httpWithRetry 3 request
    case responseOrErr of
      (Left _) -> pure $ Left ServerUnreachable
      (Right response) -> pure $ parseResponse secret response


httpWithRetry :: Int -> Request -> IO (Either (HttpException) (Response LBS.ByteString))
httpWithRetry 1 request = Exception.try $ httpLBS request
httpWithRetry triesLeft request =
  let
    retry = do
      -- Get a random jitter between 0 and 100 ms (inclusive).
      jitter <- getStdRandom (randomR (0, 100))
      let
        delayMilliseconds = 50 + jitter
        delayMicroseconds = 1000 * delayMilliseconds
      -- Wait a bit before retrying, at least 50 ms, at most 150 ms.
      -- TODO: Exponential backoff.
      threadDelay delayMicroseconds
      httpWithRetry (triesLeft - 1) request

  in do
    responseOrError <- Exception.try $ httpLBS request
    case responseOrError of
      Right response ->
        case (getResponseStatusCode response) of
          500 -> retry -- Internal Server Error
          503 -> retry -- Service Unavailable
          504 -> retry -- Gateway Timeout
          _   -> pure responseOrError
      Left (HttpExceptionRequest _ _) -> retry
      Left a -> Exception.throw a


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
      (BadRequest resp) ->
        "Made a bad request: " <> (LBS.unpack resp)
      (Forbidden) ->
        "Invalid Vault token"
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

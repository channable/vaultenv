{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.Async as Async
import           Data.Char
import           Data.List (findIndex, nubBy)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.JsonStream.Parser hiding (value)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Semigroup ((<>))
import           Network.HTTP.Simple
import           Options.Applicative hiding (Parser, command)
import qualified Options.Applicative as O
import           System.Environment
import           System.Posix.Process

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
  = SecretNotFound Secret
  | KeyNotFound    Secret
  | BadRequest     LBS.ByteString
  | Forbidden
  | Internal       LBS.ByteString
  | Maintenance    LBS.ByteString
  | Unspecified    LBS.ByteString

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

  responses <- Async.mapConcurrently (requestSecret opts) secrets

  let
    newEnvOrError = mapM (uncurry parseResponse) responses
    newEnv = case newEnvOrError of
      -- We need to eliminate duplicates in the environment and keep
      -- the first occurrence. `nubBy` (from Data.List) runs in O(n^2),
      -- but this shouldn't matter for our small lists.
      --
      -- Equality is determined on the first element of the env var
      -- tuples.
      Right e -> nubBy (\(a,_) (b,_) -> a == b) (e ++ env)
      Left err -> errorWithoutStackTrace err

  runCommand opts newEnv

-- Like splitAt, but also removes the character at the split position.
cutAt :: Int -> [a] -> ([a], [a])
cutAt index xs =
  let
    (first, second) = splitAt index xs
  in
    (first, drop 1 second)

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


requestSecret :: Options -> Secret -> IO (Secret, Response LBS.ByteString)
requestSecret opts secret = do
  let request = setRequestHeader "x-vault-token" [SBS.pack (oVaultToken opts)]
              $ setRequestPath (requestPath secret)
              $ setRequestPort (oVaultPort opts)
              $ setRequestHost (SBS.pack (oVaultHost opts))
              $ defaultRequest

  response <- httpLBS request
  pure (secret, response)

  where
    requestPath :: Secret -> SBS.ByteString
    requestPath s = "/v1/secret/" `SBS.append` (SBS.pack (sPath s))

--
-- HTTP response handling
--

parseResponse :: Secret -> Response LBS.ByteString -> Either String EnvVar
parseResponse secret response =
  case (getResponseStatusCode response) of
    403 -> Left $ "[ERROR] Vault token is invalid"
    404 -> Left $ "[ERROR] Secret not found: " ++ path
    _   -> fmap (\c -> (sVarName secret, Text.unpack c)) content
  where
    body = getResponseBody response
    key = sKey secret
    path = sPath secret
    at = "data" .:? (Text.pack key) .: string
    getContent json = parseLazyByteString at json :: [Maybe Text]
    -- This call to head should never fail: we will always get a response with
    -- a "data" key from Vault
    content = maybeToEither ("[ERROR] Key '" ++ key ++ "' not found in: " ++ path) $ head (getContent body)


-- Converts a secret into the name of the environment variable
varNameFromKey :: String -> String -> String
varNameFromKey path key = fmap format (path ++ "_" ++ key)
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore

--
-- Utilitiy functions
--

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing  = Left e

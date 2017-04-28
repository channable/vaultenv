{-# LANGUAGE OverloadedStrings #-}

import           Data.Char
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.JsonStream.Parser hiding (value)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Semigroup ((<>))
import           Network.HTTP.Simple
import           Options.Applicative hiding (Parser, command)
import qualified Options.Applicative as O
import           System.Environment
import           System.Exit
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
  { sPath :: SBS.ByteString
  , sKey  :: SBS.ByteString
  } deriving (Eq, Show)

type EnvVar = (String, String)

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

  secrets <- readSecretList (oSecretFile opts)
  responses <- mapM (requestSecret opts) secrets

  let newEnv = mapM (uncurry parseResponse) responses

  case newEnv of
    Right e -> runCommand opts (e ++ env)
    Left err -> exitWithError err


readSecretList :: FilePath -> IO [Secret]
readSecretList f = fmap ((map toSecret) . SBS.lines) (SBS.readFile f)
  where
    toSecret bs =
      let (p:k:[]) = SBS.split '#' bs
      in  Secret { sPath = p, sKey = k }


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
    requestPath s = "/v1/secret/" `SBS.append` (sPath s)

--
-- HTTP response handling
--

parseResponse :: Secret -> Response LBS.ByteString -> Either String EnvVar
parseResponse secret response =
  case (getResponseStatusCode response) of
    403 -> Left $ "[ERROR] Vault token is invalid"
    404 -> Left $ "[ERROR] Secret not found: " ++ path
    _   -> fmap (\c -> (envVarName secret, T.unpack c)) content
  where
    body = getResponseBody response
    key = E.decodeUtf8 (sKey secret)
    path = SBS.unpack (sPath secret)
    getContent json = parseLazyByteString ("data" .:? key .: string) json :: [Maybe T.Text]
    -- This call to head should never fail: we will always get a response with
    -- a "data" key from Vault
    content = maybeToEither ("[ERROR] Key '" ++ T.unpack key ++ "' not found in: " ++ path) $ head (getContent body)


-- Converts a secret into the name of the environment variable
envVarName :: Secret -> String
envVarName s = SBS.unpack $ SBS.intercalate "_" [SBS.map format (sPath s), SBS.map format (sKey s)]
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


exitWithError :: String -> IO a
exitWithError err = do
  putStrLn err
  exitWith (ExitFailure 1)

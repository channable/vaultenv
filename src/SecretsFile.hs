{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module SecretsFile where

import Data.Char (toUpper)
import Data.List (intercalate)
import Control.Applicative.Combinators (some, (<|>), optional)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Control.Exception (try)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

data Secret = Secret
  { sMount :: String
  , sPath :: String
  , sKey :: String
  , sVarName :: String
  } deriving (Eq, Show)

data SFVersion
  = V1
  | V2
  deriving (Show)

type Parser = MP.Parsec Void String

data SFError = IOError | ParseError (MP.ParseError (MP.Token String) Void)

instance Show SFError where
  show sfErr =
    case sfErr of
      IOError -> "could not read file"
      ParseError pe -> MP.parseErrorPretty pe

-- | Helper for ExceptT stuff.
readSecretList :: (MonadError SFError m, MonadIO m) => FilePath -> m [Secret]
readSecretList fp = liftEither =<< (liftIO $ readSecretsFile fp)

-- | Read a lit of secrets from a file
readSecretsFile :: FilePath -> IO (Either SFError [Secret])
readSecretsFile fp = do
  contents <- safeReadFile fp
  case contents of
    Just c -> do
      let parseResult = parseSecretsFile fp c
      case parseResult of
        Right res -> pure $ Right res
        Left err -> pure $ Left (ParseError err)
    Nothing -> pure $ Left IOError

-- | Read a file, catching all IOError exceptions.
safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile fp = do
  (contentsOrErr :: Either IOError String) <- (try . readFile) fp
  case contentsOrErr of
    Right contents -> pure $ Just contents
    Left _ -> pure $ Nothing

-- | Parse a String as a SecretsFile.
parseSecretsFile :: FilePath -> String -> Either (MP.ParseError (MP.Token String) Void) [Secret]
parseSecretsFile = MP.parse secretsFileP

-- | SpaceConsumer parser, which is responsible for stripping all whitespace.
--
-- The name is short, because we need to use it in a lot of places. This is the
-- suggested/idiomatic name in Megaparsec.
sc :: Parser ()
sc = MPL.space MPC.space1 lineComment blockComment
  where
    lineComment = MP.empty
    blockComment = MP.empty

-- | Helper which consumes all whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme sc

-- | Helper which looks for a string and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = MPL.symbol sc

-- | Top level parser of the secrets file
--
-- Parses the magic version number and dispatches to the Mount block based
-- parser or the list based parser based on that.
secretsFileP :: Parser [Secret]
secretsFileP = do
  _ <- sc
  _ <- symbol "VERSION"
  version <- versionP
  case version of
    V1 -> some (secretP version "secret")
    V2 -> concat <$> some secretBlockP

-- | Parse the file version
versionP :: Parser SFVersion
versionP = V1 <$ symbol "1"
       <|> V2 <$ symbol "2"

-- | Parse a secret block
--
-- Exclusive to V2 of the format. A secret block consists of a line describing
-- the mount location followed by secret specifications.
secretBlockP :: Parser [Secret]
secretBlockP = do
  _ <- symbol "MOUNT"
  mountPath <- lexeme (some MPC.alphaNumChar)
  some (MP.try (lexeme (secretP V2 mountPath)))

-- | Parse a secret specification line
--
-- The version of the fileformat we're parsing determines the way we report
-- variable information. For V2, the mount point is part of the variable name,
-- to allow for disambiguation. For V1, this is not needed.
secretP :: SFVersion -> String -> Parser Secret
secretP version mount = do
  varName <- optional $ MP.try secretVarP
  path <- some MPC.alphaNumChar
  _ <- symbol "#"
  key <- some MPC.alphaNumChar
  _ <- symbol "\n"

  pure Secret { sMount = mount
              , sPath = path
              , sKey = key
              , sVarName = maybe (getVarName version mount path key) id varName
              }

secretVarP :: Parser String
secretVarP = do
  var <- some MPC.alphaNumChar
  _ <- symbol "="
  pure var

-- | Convert a secret name into the name of the environment variable that it
-- will be available under.
getVarName :: SFVersion -> String -> String -> String -> String
getVarName version mount path key = fmap format $ intercalate "_" components
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore
        components = case version of
          V1 -> [path, key]
          V2 -> [mount, path, key]

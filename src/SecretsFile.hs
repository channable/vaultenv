{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : SecretsFile
Description : Parser for the vaultenv secret file format

Contains a Megaparsec parser for the config format used by vaultenv to specify
secrets. We support two versions of the format. This parser accepts both
versions of the format in use. Version one of the format is a list of secrets,
which vaultenv will fetch from a generic backend mounted at @secret/@.

Version 2 allows users to specify mountpoints. Both versions of the format
allow users to specify which keys are fetched from Vault, as well as specify
the name of the environment variable under which secrets will be made availabe.

The entrypoints here are the @readSecretsFile@ and @readSecretList@ functions.

If you are user, please see the README for more information.
-}
module SecretsFile where

import Control.Applicative.Combinators (some, option, optional)
import Control.Exception (try, displayException)
import Control.Monad.Except (MonadError, MonadIO, liftEither, liftIO)
import Data.Char (toUpper, isSpace, isControl)
import Data.Functor (void)
import Data.List (intercalate)
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

-- | Error modes of this module.
--
-- We either get IO errors because we cannot open the secrets file, or we
-- cannot parse it.
data SFError = IOErr IOError | ParseErr (MP.ParseError (MP.Token String) Void)

instance Show SFError where
  show sfErr = case sfErr of
    IOErr ioErr -> displayException ioErr
    ParseErr pe -> MP.parseErrorPretty pe

-- | Helper for ExceptT stuff that we use in app/Main.hs
readSecretList :: (MonadError SFError m, MonadIO m) => FilePath -> m [Secret]
readSecretList fp = liftEither =<< (liftIO $ readSecretsFile fp)

-- | Read a list of secrets from a file
readSecretsFile :: FilePath -> IO (Either SFError [Secret])
readSecretsFile fp = do
  contentsOrErr <- safeReadFile fp
  case contentsOrErr of
    Right c -> do
      let parseResult = parseSecretsFile fp c
      case parseResult of
        Right res -> pure $ Right res
        Left err -> pure $ Left (ParseErr err)
    Left err -> pure $ Left (IOErr err)

-- | Read a file, catching all IOError exceptions.
safeReadFile :: FilePath -> IO (Either IOError String)
safeReadFile fp = (try . readFile) fp

-- | Parse a String as a SecretsFile.
parseSecretsFile :: FilePath -> String -> Either (MP.ParseError (MP.Token String) Void) [Secret]
parseSecretsFile = MP.parse secretsFileP

-- | SpaceConsumer parser, which is responsible for stripping all whitespace.
--
-- Sometimes, we require explicit newlines, therefore, we don't handle those
-- here. @isSpace@ works on any unicode whitespace character. Megaparsec comes
-- with some helpers that would make this better, but here we need to roll our
-- own whitespace parser, because we want to preserve newlines.
whitespace :: Parser ()
whitespace = MPL.space whitespaceChars lineComment blockComment
  where
    whitespaceChars = void $ MP.takeWhile1P (Just "whitespace") (\c -> isSpace c && c /= '\n')
    lineComment = MP.empty
    blockComment = MP.empty

-- | Parses one or multiple newlines separated by whitespace.
newlines :: Parser ()
newlines = void $ some $ lexeme $ MPC.char '\n'

-- | Helper which consumes all whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme whitespace

-- | Helper which looks for a string and consumes trailing whitespace.
symbol :: String -> Parser String
symbol = MPL.symbol whitespace

-- | Top level parser of the secrets file
--
-- Parses the magic version number and dispatches to the Mount block based
-- parser or the list based parser based on that.
secretsFileP :: Parser [Secret]
secretsFileP = do
  _ <- optional newlines
  _ <- whitespace
  version <- versionP
  case version of
    V1 -> some (secretP version "secret")
    V2 -> concat <$> some secretBlockP

-- | Parse the file version
--
-- We need @MP.try@ because we need to backtrack after reading VERSION. (As
-- some secrets could very well start with that path.
versionP :: Parser SFVersion
versionP = option V1 $ MP.try $ do
  _ <- symbol "VERSION"
  _ <- symbol "2"
  _ <- newlines
  pure V2

-- | Parse a secret block
--
-- Exclusive to V2 of the format. A secret block consists of a line describing
-- the mount location followed by secret specifications.
secretBlockP :: Parser [Secret]
secretBlockP = do
  _ <- symbol "MOUNT"
  mountPath <- lexeme pathComponentP
  _ <- newlines
  some (MP.try (lexeme (secretP V2 mountPath)))

-- | Parses legal Vault path components.
--
-- A Vault path allows a surprising amount of characters. Spaces, quotes and
-- whatnot are all allowed. We don't want to complicate the parser and
-- the format by specifying escaping for all kinds of things, so we impose the
-- following restrictions:
--
--  - We don't support mounts, paths and keys with whitespace in them.
--  - We don't support control characters (vault doesn't either)
--  - All other characters except @=@ and @#@ are allowed. Supporting paths
--    with these characters in them would lead to ambiguities when parsing
--    paths such as:
--
--        FOO=foo=bar/baz#quix
--
--    and
--
--        foo#bar/baz#quix
--
-- If this is undesired, have a compelling usecase, and a good proposal for
-- supporting this, please open a ticket.
pathComponentP :: Parser String
pathComponentP = MP.takeWhile1P (Just "path component") isAllowed
  where isAllowed c = not (isSpace c) && c /= '#' && c /= '=' && not (isControl c)

-- | Parse a secret specification line
--
-- The version of the fileformat we're parsing determines the way we report
-- variable information. For V2, the mount point is part of the variable name,
-- to allow for disambiguation. For V1, this is not needed.
secretP :: SFVersion -> String -> Parser Secret
secretP version mount = do
  secret <- lexeme $ do
    varName <- optional $ MP.try secretVarP
    path <- pathComponentP
    _ <- symbol "#"
    key <- pathComponentP

    pure Secret { sMount = mount
                , sPath = path
                , sKey = key
                , sVarName = maybe (getVarName version mount path key) id varName
                }
  _ <- newlines
  pure secret

-- | Parses a secret variable.
--
-- We're restrictrive in the characters we allow in environment variables. We
-- don't allow special characters or whitespace. Environment variables have to
-- start with a letter or underscore which can be followed by letters
-- underscores and digits. This is similar to what Zsh and Bash allow in their
-- `export` statements. Even though the Unix process environment is technically
-- just a string and you can put all kinds of things in there, most programs
-- and standard libraries don't seem to support this.
--
-- Please open a ticket if you require looser restrictions.
secretVarP :: Parser String
secretVarP = do
  -- Environment variables have to start with a letter or underscore and can be
  -- followed by letters, underscores and digits.
  varStart <- MPC.oneOf asciiLettersUnderscore
  varRest <- MP.many $ MPC.oneOf (asciiLettersUnderscore ++ digits)
  _ <- symbol "="
  pure (varStart:varRest)

-- | Helper list for ASCII chars plus the underscore
asciiLettersUnderscore :: [MP.Token String]
asciiLettersUnderscore = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

-- | Helper list for ASCII digits
digits :: [MP.Token String]
digits = ['0'..'9']

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

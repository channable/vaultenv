{-# LANGUAGE FlexibleContexts #-}

module SecretsFile where

import Data.Char              (toUpper)
import Data.Monoid ((<>))
import Data.List              (intercalate)
import Control.Monad.Except   (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Control.Exception          as Exception

import Text.ParserCombinators.ReadP

data Secret = Secret
  { sMount   :: String
  , sPath    :: String
  , sKey     :: String
  , sVarName :: String
  } deriving (Eq, Show)

data SecretFileErr
  = IOError           FilePath
  | ParseError        FilePath

instance Show SecretFileErr where
  show err =
    case err of
      (IOError fp) ->
        "An I/O error happened while opening: " <> fp
      (ParseError fp) ->
        "Could not parse: " <> fp

data SecretFileVersion = V1 | V2

runParser :: MonadError SecretFileErr m => FilePath -> ReadP a -> String -> m a
runParser fileName parser contents =
  let result = (readP_to_S parser) contents
  in case result of
    [] -> throwError $ ParseError fileName
    _ -> pure . fst . head $ result

secretFileP :: ReadP [Secret]
secretFileP = do
  fileVersion <- option V1 (formatVersionP <* skipSpaces)
  case fileVersion of
    V1 -> many ((secretP fileVersion "secret") <* skipSpaces) <* eof
    V2 -> concat <$> many (secretBlockP <* skipSpaces) <* eof

formatVersionP :: ReadP SecretFileVersion
formatVersionP = const V2 <$> string "VERSION 2"

secretBlockP :: ReadP [Secret]
secretBlockP = do
  mountPath <- string "MOUNT " *> manyTill get (char '\n')
  many (secretP V2 mountPath)

secretP :: SecretFileVersion -> String -> ReadP Secret
secretP version mount = do
  varName <- option Nothing $ Just <$> manyTill get (char '=')
  (path, key) <- (,) <$> manyTill get (char '#') <*> manyTill get (char '\n')

  pure Secret { sMount = mount
              , sPath = path
              , sKey = key
              , sVarName = maybe (getVarName version mount path key) id varName
              }

-- | Convert a secret name into the name of the environment variable that it
-- will be available under.
getVarName :: SecretFileVersion -> String -> String -> String -> String
getVarName version mount path key = fmap format $ intercalate "_" components
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore
        components = case version of
          V1 -> [path, key]
          V2 -> [mount, path, key]

readSecretList :: (MonadError SecretFileErr m, MonadIO m) => FilePath -> m [Secret]
readSecretList fileName = do
  contents <- liftIO $ safeReadFile
  res <- maybe (throwError $ IOError fileName) (runParser fileName secretFileP) contents
  liftIO $ print res
  pure res
  where
    safeReadFile =
      Exception.catch (Just <$> readFile fileName)
        ((\_ -> return Nothing) :: Exception.IOException -> IO (Maybe String))


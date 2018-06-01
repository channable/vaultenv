{-# LANGUAGE FlexibleContexts #-}

module SecretsFile where

import Data.Char              (toUpper)
import Data.Monoid ((<>))
import Data.List              (findIndex)
import Control.Monad.Except   (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Control.Exception          as Exception

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

readSecretList :: (MonadError SecretFileErr m, MonadIO m) => FilePath -> m [Secret]
readSecretList fname = do
  mfile <- liftIO $ safeReadFile
  maybe (throwError $ IOError fname) parseSecrets mfile
  where
    parseSecrets file =
      let
        esecrets = traverse parseSecret . lines $ file
      in
        either (throwError . ParseError) return esecrets

    safeReadFile =
      Exception.catch (Just <$> readFile fname)
        ((\_ -> return Nothing) :: Exception.IOException -> IO (Maybe String))

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
    pure Secret { sMount = "secret"
                , sPath = path
                , sKey = key
                , sVarName = varName
                }

-- | Convert a secret name into the name of the environment variable that it
-- will be available under.
varNameFromKey :: String -> String -> String
varNameFromKey path key = fmap format (path ++ "_" ++ key)
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore

-- | Like @splitAt@, but also removes the character at the split position.
cutAt :: Int -> [a] -> ([a], [a])
cutAt index xs =
  let
    (first, second) = splitAt index xs
  in
    (first, drop 1 second)

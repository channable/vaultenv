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

import Control.Exception (try, displayException)
import Data.Char (toUpper, isSpace)
import Data.List (elemIndex, intercalate, isPrefixOf)

data Secret = Secret
  { sMount :: String
  , sPath :: String
  , sKey :: String
  , sVarName :: String
  } deriving (Eq, Show)

-- | Read a file, catching all IOError exceptions.
safeReadFile :: FilePath -> IO (Either IOError String)
safeReadFile fp = (try . readFile) fp

-- | Read a list of secrets from a file
readSecretsFile :: FilePath -> IO (Either String [Secret])
readSecretsFile fp = do
  contentsOrErr <- safeReadFile fp
  case contentsOrErr of
    Right str -> pure $ parseSecretsFile str
    Left err -> pure $ Left $ displayException err

parseSecretsFile :: String -> Either String [Secret]
parseSecretsFile str =
  let
    dataLines = id
      -- Drop blank lines.
      $ filter (not . all isSpace)
      -- Drop comment lines that start with #.
      $ filter (\line -> not $ "#" `isPrefixOf` line)
      $ lines str
  in
    case dataLines of
      [] -> Left "Secrets file must not be empty."
      "VERSION 2" : linesV2 -> parseSecretsV2 linesV2
      linesV1 -> mapM parseSecretV1 linesV1

-- | V1 secrets use mount "secret" and have no prefix in the default variable name.
parseSecretV1 :: String -> Either String Secret
parseSecretV1 = parseSecret "secret" []

parseSecretsV2 :: [String] -> Either String [Secret]
parseSecretsV2 fileLines = go Nothing [] fileLines
  where
    -- We build the list in reverse while we parse the file, so reverse it
    -- at the end, to avoid being accidentally quadratic.
    go _mountOpt acc [] = Right $ reverse acc
    go mountOpt acc (line : more) = case (mountOpt, words line) of
      (_, ["MOUNT", mount]) -> go (Just mount) acc more
      (Nothing, _) -> Left $ "Expected a 'MOUNT <mount>' line before secret definition."
      (Just mount, _) -> do
        secret <- parseSecret mount [mount] line
        go mountOpt (secret : acc) more

parseSecret :: String -> [String] -> String -> Either String Secret
parseSecret mount mountPrefix = parseVar
  where
    parseVar line = case elemIndex '=' line of
      Nothing -> parsePath line
      Just i -> do
        secret <- parsePath $ drop (i + 1) line
        let varName = take i line
        validateVariableName varName
        pure $ secret { sVarName = varName }

    parsePath line = case elemIndex '#' line of
      Nothing -> Left $ "Line must contain a '#' to indicate the key, on line: " ++ line
      Just i -> do
        let
          key = drop (i + 1) line
          path = take i line
        validateKey key
        pure $ Secret
          { sKey = key
          , sPath = path
          , sMount = mount
          , sVarName = getVarName mountPrefix path key
          }

validateKey :: String -> Either String ()
validateKey key = if '#' `elem` key
  then Left $ "Key must not contain '#': " ++ key
  else Right ()

validateVariableName :: String -> Either String ()
validateVariableName name = case name of
  ch : _ | ch `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_']) -> Right ()
  [] -> Left "Variable name must not be empty."
  _ -> Left $ "Variable name must start with _ or ASCII letter: " ++ name

-- | Convert a secret name into the name of the environment variable that it
-- will be available under.
getVarName :: [String] -> String -> String -> String
getVarName mountPrefix path key = fmap format $ intercalate "_" components
  where underscore '/' = '_'
        underscore '-' = '_'
        underscore c   = c
        format         = toUpper . underscore
        components     = mountPrefix ++ [path, key]

{-# LANGUAGE OverloadedStrings #-}

-- | Types to hold and json-decode responses from Vault.
module Response
  ( ClientToken (..)
  ) where

import Data.Aeson (FromJSON, (.:))
import Data.Text (Text)

import qualified Data.Aeson as Aeson

-- | The "client_token" field from an /auth/kubernetes/login or
--   /auth/github/login response.
newtype ClientToken = ClientToken Text deriving (Eq, Show)

instance FromJSON ClientToken where
  parseJSON = Aeson.withObject "AuthResponse" $ \obj -> do
    auth <- obj .: "auth"
    clientToken <- auth .: "client_token"
    pure $ ClientToken clientToken

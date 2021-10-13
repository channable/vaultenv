{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ResponseSpec where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString

import Response (ClientToken (..))

-- Example response from /v1/auth/kubernetes/login
-- Secrets are from a test setup, these are not real secrets.
loginResponse :: LazyByteString.ByteString
loginResponse = 
  -- Replace single quotes (U+0027) with double quotes (U+0022), so we can write
  -- the json string here as a string literal without too many escapes.
  LazyByteString.map (\case
    0x27 -> 0x22
    x    -> x
  ) "{'request_id':'43cfeccb-ebd6-aa9f-bcd0-e1c184b625ae','lease_id':'','renewable':false,'lease_duration':0,'data':null,'wrap_info':null,'warnings':null,'auth':{'client_token':'s.J1hknwSvoU5iDB7QfAJ2G15V','accessor':'u3D5ESJ0OzenuqwUdSX87VTZ','policies':['default','testapp-kv-ro'],'token_policies':['default','testapp-kv-ro'],'metadata':{'role':'vaultenv-test-role','service_account_name':'test-vault-auth','service_account_namespace':'default','service_account_secret_name':'','service_account_uid':'0036f224-8b4e-4bd2-b298-560b7b39c011'},'lease_duration':86400,'renewable':true,'entity_id':'349d417f-d5d9-2ae5-a042-b801848334a7','token_type':'service','orphan':true}}"

spec :: SpecWith ()
spec = do
  describe "Response.ClientToken" $ do
    it "can be parsed from json" $ do
      Aeson.eitherDecode loginResponse `shouldBe` Right (ClientToken "s.J1hknwSvoU5iDB7QfAJ2G15V")

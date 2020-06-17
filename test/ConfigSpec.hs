{-# LANGUAGE ScopedTypeVariables #-}
module ConfigSpec where

import Data.Either(isRight, isLeft)
import Data.Maybe(fromJust)
import Network.URI (URI(..), parseURI, isUnescapedInURIComponent)
import Test.Hspec
import Test.QuickCheck

import Config

getHost
  :: Either OptionsError (ValidScheme, String, Int)
  -> Either OptionsError String
getHost = fmap $ \(_, host, _) -> host

getScheme
  :: Either OptionsError (ValidScheme, String, Int)
  -> Either OptionsError ValidScheme
getScheme = fmap $ \(scheme, _, _) -> scheme

getPort
  :: Either OptionsError (ValidScheme, String, Int)
  -> Either OptionsError Int
getPort = fmap $ \(_, _, port) -> port

-- Generates strings that are valid as host parts of a URI
genHost :: Gen String
genHost = arbitrary `suchThat` all isUnescapedInURIComponent

-- Valid hosts and ports
genHostPort :: Gen (String, Int)
genHostPort = (,) <$> genHost <*> (arbitrary `suchThat` (>=0))

spec :: SpecWith ()
spec =
  describe "Split addr" $ do
    it "should accept http schemes " $
      getScheme (splitAddress $ fromJust $ parseURI "http://localhost:80")
        `shouldBe` Right HTTP
    it "should accept https schemes " $
      getScheme (splitAddress $ fromJust $ parseURI "https://localhost:80")
        `shouldBe` Right HTTPS
    it "should reject any other scheme" $
      property $ \scheme ->
        let uri = parseURI $ scheme ++ "://localhost:80"
        in scheme `notElem` ["http", "https"]
        ==> maybe True (isLeft . splitAddress) uri
    it "should split any valid addr properly" $
      forAll genHostPort (\(host, port) ->
        let
          httpAddr :: Maybe URI
          httpAddr = parseURI $ "http://" ++ host ++ ":" ++ show port

          httpsAddr :: Maybe URI
          httpsAddr = parseURI $ "https://" ++ host ++ ":" ++ show port
        in
               (splitAddress <$> httpAddr `shouldBe` Just (Right (HTTP, host, port)))
          .&&. (splitAddress <$> httpsAddr `shouldBe` Just (Right (HTTPS, host, port)))
      )
    it "should parse addr without explicit port" $
      forAll genHost (\host ->
        let
          httpAddr :: Maybe URI
          httpAddr = parseURI $ "http://" ++ host

          httpsAddr :: Maybe URI
          httpsAddr = parseURI $ "https://" ++ host
        in      (splitAddress <$> httpAddr `shouldBe` Just (Right (HTTP, host, 80)))
           .&&. (splitAddress <$> httpsAddr `shouldBe` Just (Right (HTTPS, host, 443)))
      )
    it "should accept the default configuration" $
      isRight (validateCopyAddr "" $ castOptions defaultOptions)
    it "should reject mismatched port" $
      let
        options = defaultOptions{
          oVaultPort = Just 1234
        }
      in isLeft (validateCopyAddr "" $ castOptions options)
    it "should reject mismatched host" $
      let
        options = defaultOptions{
          oVaultHost = Just "invalid_host"
        }
      in isLeft (validateCopyAddr "" $ castOptions options)
    it "should reject mismatched TLS" $
      let
        options = defaultOptions{
          oConnectTls = Just False
        }
      in isLeft (validateCopyAddr "" $ castOptions options)
    it "should reject invalid schemes" $
      let
        options = defaultOptions{
          oVaultAddr = parseURI "ftp://localhost:8200"
        }
      in isLeft (validateCopyAddr "" $ castOptions options)
    it "should accept URLs with trailing slash " $
        let
          options = defaultOptions{
            oVaultAddr = parseURI "https://localhost:8200/"
          }
        in isRight (validateCopyAddr "" $ castOptions options)
    it "should reject URLs with non-empty paths" $
      let
        options = defaultOptions{
          oVaultAddr = parseURI "https://localhost:8200/foo"
        }
      in isLeft (validateCopyAddr "" $ castOptions options)

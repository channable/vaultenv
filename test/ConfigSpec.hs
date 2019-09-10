module ConfigSpec where

import Test.Hspec
import Test.QuickCheck

import Config

getHost :: (Maybe String, String, String) -> String
getHost (_, host, _) = host

getScheme :: (Maybe String, String, String) -> Maybe String
getScheme (mScheme, _, _) = mScheme

getPort :: (Maybe String, String, String) -> String
getPort (_, _, sPort) = sPort

spec :: SpecWith ()
spec =
    describe "Split addr" $ do
        it "should accept http schemes "
            (
                getScheme (splitAddress "http://localhost:80") `shouldBe` Just "http://"
            )
        it "should accept https schemes "
            (
                getScheme (splitAddress "https://localhost:80") `shouldBe` Just "https://"
            )
        it "should reject any other scheme"
            (
                property $ \scheme -> scheme `notElem` ["http", "https"]
                    ==> getScheme (splitAddress $ scheme ++ "://localhost:80") `shouldBe` Nothing
            )
        it "should parse any host"
            (
                property (\host ->
                    let
                        addr :: String
                        addr = "http://" ++ (host :: String) ++ ":80"
                    in getHost (splitAddress addr) `shouldBe` host
                )

            )
        it "should parse any port"
            (
                property (\port ->
                    let
                        addr :: String
                        addr = "http://localhost:" ++ port
                    in ':' `notElem` port ==> getPort (splitAddress addr) `shouldBe` port
                )
            )
        it "should parse any addr"
            (
                property (\host port ->
                    let
                        addr :: String
                        addr = "http://" ++ host ++ ":" ++ port
                    in ':' `notElem` port ==> splitAddress addr `shouldBe` (Just "http://", host, port)
                )
            )

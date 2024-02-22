module SecretFileSpec where

import Test.Hspec

import Data.Either (isRight, isLeft)


import qualified SecretsFile
import qualified System.Directory as Dir

spec :: SpecWith ()
spec = do
  describe "SecretFile.readSecretList" $ do
    it "parses all golden tests succesfully" $ do
      goldenTestContents <- Dir.listDirectory "test/golden"
      let goldenTestFiles = map ("test/golden/" <>) goldenTestContents
      parseResults <- mapM SecretsFile.readSecretsFile goldenTestFiles
      parseResults `shouldSatisfy` (all isRight)

    it "rejects all invalid examples succesfully" $ do
      invalidTestContents <- Dir.listDirectory "test/invalid"
      let invalidTestFiles = map ("test/invalid/" <>) invalidTestContents
      parseResults <- mapM SecretsFile.readSecretsFile invalidTestFiles
      parseResults `shouldSatisfy` (all isLeft)

    it "parses all v2-comments.secrets as expected" $ do
      -- This is a regression test, Vaultenv 0.16.0 used to drop the second
      -- secret.
      Right parseResults <- SecretsFile.readSecretsFile "test/golden/v2-comments.secrets"
      parseResults `shouldBe`
        [ SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "devices/frobnicator"
          , SecretsFile.sKey = "PASSWORD"
          , SecretsFile.sVarName = "FROBNICATOR_PASSWORD"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "devices/widgets/turboencabulator"
          , SecretsFile.sKey = "PIN"
          , SecretsFile.sVarName = "WIDGET_PIN"
          }
        ]

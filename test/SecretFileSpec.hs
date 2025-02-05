module SecretFileSpec where

import Test.Hspec

import Data.Either (isRight, isLeft)
import Control.Monad (forM_)

import qualified SecretsFile
import qualified System.Directory as Dir

spec :: SpecWith ()
spec = do
  describe "SecretFile.readSecretList" $ do
    goldenTestContents <- runIO $ Dir.listDirectory "test/golden"
    let goldenTestFiles = map ("test/golden/" <>) goldenTestContents
    forM_ goldenTestFiles $ \fname ->
      it ("parses " ++ fname ++ " correctly") $ do
        parseResult <- SecretsFile.readSecretsFile fname
        parseResult `shouldSatisfy` isRight

    invalidTestContents <- runIO $ Dir.listDirectory "test/invalid"
    let invalidTestFiles = map ("test/invalid/" <>) invalidTestContents
    forM_ invalidTestFiles $ \fname ->
      it ("rejects " ++ fname) $ do
        parseResult <- SecretsFile.readSecretsFile fname
        parseResult `shouldSatisfy` isLeft

    it "parses v1.secrets as expected" $ do
      Right parseResults <- SecretsFile.readSecretsFile "test/golden/v1.secrets"
      parseResults `shouldBe`
        [ SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo"
          , SecretsFile.sKey = "bar"
          , SecretsFile.sVarName = "FOO_BAR"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/bar"
          , SecretsFile.sKey = "baz"
          , SecretsFile.sVarName = "FOO_BAR_BAZ"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "bar"
          , SecretsFile.sKey = "baz"
          , SecretsFile.sVarName = "FOO"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/baz"
          , SecretsFile.sKey = "quix"
          , SecretsFile.sVarName = "BAR"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/single"
          , SecretsFile.sKey = "underscore"
          , SecretsFile.sVarName = "single_underscore"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/double"
          , SecretsFile.sKey = "underscore"
          , SecretsFile.sVarName = "double__underscore"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/double"
          , SecretsFile.sKey = "underscore"
          , SecretsFile.sVarName = "_leading_underscore"
          }
        ]

    it "parses v2.secrets as expected" $ do
      Right parseResults <- SecretsFile.readSecretsFile "test/golden/v2.secrets"
      parseResults `shouldBe`
        [ SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo"
          , SecretsFile.sKey = "bar"
          , SecretsFile.sVarName = "SECRET_FOO_BAR"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "secret"
          , SecretsFile.sPath = "foo/baz"
          , SecretsFile.sKey = "bar"
          , SecretsFile.sVarName = "BAR"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "otherthing"
          , SecretsFile.sPath = "foo"
          , SecretsFile.sKey = "bar"
          , SecretsFile.sVarName = "OTHERTHING_FOO_BAR"
          }
        , SecretsFile.Secret
          { SecretsFile.sMount = "otherthing"
          , SecretsFile.sPath = "foo/baz"
          , SecretsFile.sKey = "bar"
          , SecretsFile.sVarName = "BAR"
          }
        ]

    it "parses v2-comments.secrets as expected" $ do
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

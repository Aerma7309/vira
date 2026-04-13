{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.ConfigurationSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
import Language.Haskell.Hint.Nix (ghcPackagePath)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Paths_vira (getDataFileName)
import System.Directory (listDirectory)
import Test.Hspec
import Vira.CI.Configuration
import Vira.CI.Context (CIMode (..), ViraContext (..))
import Vira.CI.Pipeline.Implementation (defaultPipeline)
import Vira.CI.Pipeline.Type (BuildStage (..), Flake (..), HttpMethod (..), NixConfig (..), PostBuildStage (..), SignoffStage (..), ViraPipeline (..), WebhookConfig (..), validateNixOptions)
import Vira.State.Type (Branch (..))
import Prelude hiding (id)

-- Test data
testBranchStaging :: Branch
testBranchStaging =
  Branch
    { repoName = RepoName "test-repo"
    , branchName = BranchName "staging"
    , headCommit =
        Commit
          { id = CommitID "abc123"
          , message = "Test commit"
          , date = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
          , author = "Test Author"
          , authorEmail = "test@example.com"
          }
    , deleted = False
    }

testContextStaging :: ViraContext
testContextStaging =
  ViraContext
    { branch = testBranchStaging.branchName
    , ciMode = FullBuild
    , commitId = testBranchStaging.headCommit.id
    , cloneUrl = Just "https://example.com/test-repo.git"
    , repoDir = "/tmp/test-repo"
    }

spec :: Spec
spec = describe "Vira.CI.Configuration" $ do
  describe "applyConfig" $ do
    it "applies valid config correctly" $ do
      configPath <- getDataFileName "test/sample-configs/simple-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.signoff.enable `shouldBe` True
          let (rootFlake :| _) = pipeline.build.flakes
          rootFlake.path `shouldBe` "."
          rootFlake.overrideInputs `shouldBe` [("local", "github:boolean-option/false")]
        Left err -> expectationFailure $ "Config application failed: " <> show err

    it "applies nix options correctly" $ do
      configPath <- getDataFileName "test/sample-configs/nix-options-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      result <- applyConfig configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.nix.options
            `shouldBe` ( [ ("sandbox", "relaxed")
                         , ("cores", "4")
                         , ("max-jobs", "2")
                         , ("allow-import-from-derivation", "true")
                         ] ::
                           [(Text, Text)]
                       )
        Left err -> expectationFailure $ "Config application failed: " <> show err

    it "applies webhook config correctly" $ do
      -- The Nix package DB baked into the binary may not have WebhookConfig yet
      -- (it predates this branch). Run the interpreter with both the Nix DB
      -- (for external deps like Relude) and the cabal in-place DB (for our
      -- locally-modified vira-ci-types) so all modules are visible.
      configPath <- getDataFileName "test/sample-configs/webhook-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      ghcDirs <- listDirectory "dist-newstyle/packagedb"
      let inPlaceDb = case filter ("ghc-" `isPrefixOf`) ghcDirs of
            (d : _) -> "dist-newstyle/packagedb/" <> d
            [] -> "dist-newstyle/packagedb"
          runner =
            unsafeRunInterpreterWithArgs
              [ "-package-db"
              , ghcPackagePath -- Nix DB: Relude, GHC.Records.Compat, etc.
              , "-package-db"
              , inPlaceDb -- in-place DB: local vira-ci-types with WebhookConfig
              ]
      result <- applyConfigWithRunner runner configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          let hooks = pipeline.postBuild.webhooks
          length hooks `shouldBe` 1
          case hooks of
            [hook] -> do
              hook.webhookUrl `shouldBe` "https://example.com/notify?branch=$VIRA_BRANCH&commit=$VIRA_COMMIT_ID"
              hook.method `shouldBe` GET
              hook.headers `shouldBe` []
              hook.body `shouldBe` Nothing
            _ -> expectationFailure $ "Expected exactly 1 webhook, got " <> show (length hooks)
        Left err -> expectationFailure $ "Config application failed: " <> show err

  describe "validateNixOptions" $ do
    it "accepts all whitelisted options" $ do
      let opts = [("sandbox", "relaxed"), ("cores", "4"), ("max-jobs", "2"), ("allow-import-from-derivation", "true")]
      validateNixOptions opts `shouldBe` []

    it "rejects disallowed options" $ do
      let opts = [("sandbox", "relaxed"), ("access-tokens", "secret")]
      validateNixOptions opts `shouldBe` ["access-tokens"]

    it "returns empty for empty options" $ do
      validateNixOptions [] `shouldBe` []

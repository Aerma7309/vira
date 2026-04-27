{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Pipeline.ImplementationSpec (spec) where

import Data.List (lookup)
import Data.Map qualified as Map (empty)
import Effectful.Git (BranchName (..), CommitID (..))
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import Test.Hspec
import Vira.App.Type qualified as App (HooksConfig)
import Vira.CI.Context (CIMode (..), ViraContext (..))
import Vira.CI.Pipeline.Implementation (defaultPipeline, hookEnvVars, repoNameFromCloneUrl, runHook)
import Vira.CI.Pipeline.Type (Hooks (..), ViraPipeline (..))
import Prelude hiding (id)

-- Test data
testContext :: ViraContext
testContext =
  ViraContext
    { branch = BranchName "main"
    , ciMode = FullBuild
    , commitId = CommitID "deadbeef"
    , cloneUrl = Just "https://example.com/test-repo.git"
    , repoDir = "/tmp"
    }

spec :: Spec
spec = describe "Vira.CI.Pipeline.Implementation" $ do
  describe "hookEnvVars" $ do
    it "sets VIRA_BRANCH from context" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_BRANCH" vars `shouldBe` Just "main"

    it "sets VIRA_COMMIT_ID from context" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_COMMIT_ID" vars `shouldBe` Just "deadbeef"

    it "sets VIRA_REPO derived from cloneUrl" $ do
      let vars = hookEnvVars testContext
      lookup "VIRA_REPO" vars `shouldBe` Just "test-repo"

  describe "repoNameFromCloneUrl" $ do
    it "returns 'unknown' for Nothing" $ do
      repoNameFromCloneUrl Nothing `shouldBe` "unknown"

    it "strips .git suffix from HTTPS URL" $ do
      repoNameFromCloneUrl (Just "https://example.com/test-repo.git") `shouldBe` "test-repo"

    it "returns last path component when no .git suffix" $ do
      repoNameFromCloneUrl (Just "https://example.com/my-project") `shouldBe` "my-project"

    it "handles SSH URL format" $ do
      repoNameFromCloneUrl (Just "git@github.com:user/vira.git") `shouldBe` "vira"

    it "returns 'unknown' for edge case of only .git" $ do
      repoNameFromCloneUrl (Just "https://example.com/.git") `shouldBe` "unknown"

  describe "runHook" $ do
    it "returns Left when hook name not found in config" $ do
      let emptyConfig = Map.empty :: App.HooksConfig
      result <- runHook emptyConfig "nonexistent" [] "/tmp"
      result `shouldBe` Left "Hook 'nonexistent' not found in operator configuration"

    it "returns Right () for successful hook command" $ do
      let config = one ("success-hook", "true") :: App.HooksConfig
      result <- runHook config "success-hook" [] "/tmp"
      result `shouldBe` Right ()

    it "returns Left with exit code for failing hook command" $ do
      let config = one ("fail-hook", "false") :: App.HooksConfig
      result <- runHook config "fail-hook" [] "/tmp"
      result `shouldBe` Left "Hook command exited with code 1"

    it "passes environment variables to hook command" $ do
      -- Write VIRA_BRANCH value to a temp file via the hook, then read it back
      (tmpPath, tmpHandle) <- openTempFile "/tmp" "vira-hook-test"
      hClose tmpHandle
      let envVar = ("VIRA_BRANCH", "staging")
          hookCmd = "echo $VIRA_BRANCH > " <> toText tmpPath
          config = one ("env-hook", hookCmd) :: App.HooksConfig
      result <- runHook config "env-hook" [envVar] "/tmp"
      result `shouldBe` Right ()
      content <- readFileBS tmpPath
      decodeUtf8 content `shouldContain` "staging"
      removeFile tmpPath

  describe "defaultPipeline" $ do
    it "has no onSuccess hook by default" $ do
      defaultPipeline.hooks.onSuccess `shouldBe` Nothing

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Pipeline.ImplementationSpec (spec) where

import Control.Exception (finally)
import Data.List (lookup)
import Data.Map qualified as Map (empty)
import Data.Text qualified as T
import Effectful.Git (BranchName (..), CommitID (..))
import LogSink (Sink (..))
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import Test.Hspec
import Vira.App.Type qualified as App (HooksConfig)
import Vira.CI.Context (CIMode (..), ViraContext (..), repoNameFromCloneUrl)
import Vira.CI.Pipeline.Implementation (defaultPipeline, hookEnvVars, runHook)
import Vira.CI.Pipeline.Type (Hooks (..), ViraPipeline (..))
import Prelude hiding (id)

-- | Sink that throws away anything written, for tests that don't inspect subprocess output.
discardSink :: Sink Text
discardSink = Sink {sinkWrite = const pass, sinkFlush = pass, sinkClose = pass}

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

    it "omits VIRA_REPO when cloneUrl is absent" $ do
      let vars = hookEnvVars testContext {cloneUrl = Nothing}
      lookup "VIRA_REPO" vars `shouldBe` Nothing

  describe "repoNameFromCloneUrl" $ do
    it "returns Nothing for Nothing" $ do
      repoNameFromCloneUrl Nothing `shouldBe` Nothing

    it "strips .git suffix from HTTPS URL" $ do
      repoNameFromCloneUrl (Just "https://example.com/test-repo.git") `shouldBe` Just "test-repo"

    it "returns last path component when no .git suffix" $ do
      repoNameFromCloneUrl (Just "https://example.com/my-project") `shouldBe` Just "my-project"

    it "handles SSH URL format" $ do
      repoNameFromCloneUrl (Just "git@github.com:user/vira.git") `shouldBe` Just "vira"

    it "returns Nothing for edge case of only .git" $ do
      repoNameFromCloneUrl (Just "https://example.com/.git") `shouldBe` Nothing

  describe "runHook" $ do
    it "returns Left when hook name not found in config" $ do
      let emptyConfig = Map.empty :: App.HooksConfig
      result <- runHook emptyConfig "nonexistent" [] "/tmp" discardSink Nothing
      result `shouldBe` Left "Hook 'nonexistent' not found in operator configuration"

    it "returns Right () for successful hook command" $ do
      let config = one ("success-hook", "true") :: App.HooksConfig
      result <- runHook config "success-hook" [] "/tmp" discardSink Nothing
      result `shouldBe` Right ()

    it "returns Left with exit code for failing hook command" $ do
      let config = one ("fail-hook", "false") :: App.HooksConfig
      result <- runHook config "fail-hook" [] "/tmp" discardSink Nothing
      result `shouldBe` Left "Hook command exited with code 1"

    it "passes environment variables to hook command" $ do
      -- Write VIRA_BRANCH value to a temp file via the hook, then read it back
      (tmpPath, tmpHandle) <- openTempFile "/tmp" "vira-hook-test"
      hClose tmpHandle
      let envVar = ("VIRA_BRANCH", "staging")
          hookCmd = "echo $VIRA_BRANCH > " <> toText tmpPath
          config = one ("env-hook", hookCmd) :: App.HooksConfig
      result <- runHook config "env-hook" [envVar] "/tmp" discardSink Nothing
      result `shouldBe` Right ()
      content <- readFileBS tmpPath
      decodeUtf8 content `shouldContain` "staging"
      removeFile tmpPath

    it "times out when hook command runs longer than the limit" $ do
      let config = one ("sleep-hook", "sleep 5") :: App.HooksConfig
      result <- runHook config "sleep-hook" [] "/tmp" discardSink (Just 200_000) -- 0.2s
      case result of
        Left msg -> T.isInfixOf "timed out" msg `shouldBe` True
        Right () -> expectationFailure "Expected timeout, hook returned Right ()"

    it "hook envVars override the inherited environment" $ do
      let varName = "VIRA_TEST_PARENT_OVERRIDE_XYZ"
      setEnv varName "from-parent"
      (tmpPath, tmpHandle) <- openTempFile "/tmp" "vira-hook-test"
      hClose tmpHandle
      let hookCmd = "echo $" <> toText varName <> " > " <> toText tmpPath
          config = one ("override-hook", hookCmd) :: App.HooksConfig
      ( do
          result <- runHook config "override-hook" [(toText varName, "from-hook")] "/tmp" discardSink Nothing
          result `shouldBe` Right ()
          content <- readFileBS tmpPath
          decodeUtf8 content `shouldContain` "from-hook"
        )
        `finally` (removeFile tmpPath >> unsetEnv varName)

  describe "defaultPipeline" $ do
    it "has no onSuccess hook by default" $ do
      defaultPipeline.hooks.onSuccess `shouldBe` Nothing

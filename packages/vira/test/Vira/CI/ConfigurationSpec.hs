{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.ConfigurationSpec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Effectful.Git (BranchName (..), Commit (..), CommitID (..), RepoName (..))
import Language.Haskell.Hint.Nix (ghcLibDir, runInterpreterWithNixPackageDb)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgsLibdir)
import Paths_vira (getDataFileName)
import System.Environment (getExecutablePath)
import System.FilePath (joinPath, splitDirectories)
import Test.Hspec
import Vira.CI.Configuration
import Vira.CI.Context (CIMode (..), ViraContext (..))
import Vira.CI.Pipeline.Implementation (defaultPipeline)
import Vira.CI.Pipeline.Type (BuildStage (..), Flake (..), Hooks (..), NixConfig (..), SignoffStage (..), ViraPipeline (..), validateNixOptions)
import Vira.State.Type (Branch (..))
import Prelude hiding (id)

{- | Locate the cabal in-place package DB by walking up from the test binary.
The binary is at dist-newstyle/build/.../vira-tests and the package DB
is at dist-newstyle/packagedb/ghc-<version>.
Returns 'Nothing' when running in a Nix build (no dist-newstyle in path).
-}
getCabalInplacePkgDb :: IO (Maybe FilePath)
getCabalInplacePkgDb = do
  exePath <- getExecutablePath
  let dirs = splitDirectories exePath
      -- The path contains "ghc-<version>" as a directory name under dist-newstyle
      beforeDistNewstyle = takeWhile (/= "dist-newstyle") dirs
      -- Find the GHC version directory (matches "ghc-*")
      afterDistNewstyle = drop 1 (dropWhile (/= "dist-newstyle") dirs)
  case filter ("ghc-" `isPrefixOf`) afterDistNewstyle of
    (ghcDir : _) -> pure $ Just $ joinPath (beforeDistNewstyle ++ ["dist-newstyle", "packagedb", ghcDir])
    [] -> pure Nothing

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

    it "applies hooks configuration correctly" $ do
      -- The Nix package DB may not include the latest vira-ci-types with Hooks,
      -- so we try the cabal in-place package DB first; fall back to the Nix
      -- package DB when running in a Nix build (no dist-newstyle in path).
      configPath <- getDataFileName "test/sample-configs/hooks-example.hs"
      configCode <- decodeUtf8 <$> readFileBS configPath
      mCabalPkgDb <- getCabalInplacePkgDb
      let runner = case mCabalPkgDb of
            Just cabalPkgDb -> unsafeRunInterpreterWithArgsLibdir ["-package-db", cabalPkgDb] ghcLibDir
            Nothing -> runInterpreterWithNixPackageDb
      result <- applyConfigWithRunner runner configCode testContextStaging defaultPipeline
      case result of
        Right pipeline -> do
          pipeline.hooks.onSuccess `shouldBe` Just "notify-jenkins"
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

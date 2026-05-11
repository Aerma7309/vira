{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- @id@ from "Prelude" is hidden by the @Commit (id)@ import,
-- so passing the identity function inline requires a lambda.
{-# HLINT ignore "Use id" #-}

module Vira.CI.Pipeline.Implementation (
  runPipeline,

  -- * Used in tests
  defaultPipeline,
  hookEnvVars,
  runHook,
) where

import Prelude hiding (asks, id)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception qualified as CE
import Data.Aeson (eitherDecodeFileStrict)
import Data.Map qualified as Map
import DevourFlake (DevourFlakeArgs (..), devourFlake, prefetchFlakeInputs)
import DevourFlake.Result (DevourFlakeResult (..), SystemOutputs (..), extractSystems)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Environment (Environment)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Platform (detectPlatform)
import Effectful.Git.Types (Commit (id))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import LogSink (Sink (..))
import LogSink.Handle (drainHandleWith)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (System (..))
import System.Process (CreateProcess (..), StdStream (CreatePipe), interruptProcessGroupOf, proc, waitForProcess, withCreateProcess)
import System.Timeout qualified as Timeout
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (CIMode (..), ViraContext (..), repoNameFromCloneUrl)
import Vira.CI.Error (ConfigurationError (..), PipelineError (..), pipelineToolError)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess)
import Vira.CI.Pipeline.Signoff qualified as Signoff
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), Hooks (..), NixConfig (..), SignoffStage (..), ViraPipeline (..), allowedNixOptions, validateNixOptions)
import Vira.Environment.Tool.Tools.Attic qualified as AtticTool
import Vira.Environment.Tool.Type.ToolData (status)
import Vira.Environment.Tool.Type.Tools (attic)
import Vira.State.Type (Branch (..), Repo (..))

-- | Run the unified Pipeline effect
runPipeline ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  PipelineEnv ->
  Eff (Pipeline : ER.Reader PipelineEnv : es) a ->
  Eff es a
runPipeline env program =
  ER.runReader env $
    interpret
      ( \_ -> \case
          Clone repo branch workspacePath -> cloneImpl repo branch workspacePath
          LoadConfig -> loadConfigImpl
          Build pipeline -> buildImpl pipeline
          Cache pipeline buildResults -> cacheImpl pipeline buildResults
          Signoff pipeline buildResults -> signoffImpl pipeline buildResults
          PostBuild pipeline -> postBuildImpl pipeline
      )
      program

-- | Implementation: Clone repository
cloneImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  Repo ->
  Branch ->
  FilePath ->
  Eff es FilePath
cloneImpl repo branch workspacePath = do
  let projectDirName = "project"
  cloneProc <-
    Git.cloneAtCommit
      repo.cloneUrl
      branch.headCommit.id
      projectDirName

  logPipeline Info $ "Cloning repository at commit " <> toText branch.headCommit.id

  runProcess workspacePath cloneProc

  let clonedDir = workspacePath </> projectDirName
  logPipeline Info $ "Repository cloned to " <> toText clonedDir
  pure clonedDir

-- | Implementation: Load vira.hs configuration
loadConfigImpl ::
  ( FileSystem :> es
  , IOE :> es
  , Log (RichMessage IO) :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  Eff es ViraPipeline
loadConfigImpl = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
      viraConfigPath = repoDir </> "vira.hs"
  doesFileExist viraConfigPath >>= \case
    True -> do
      logPipeline Info "Found vira.hs configuration file, applying customizations..."
      content <- liftIO $ decodeUtf8 <$> readFileBS viraConfigPath
      Configuration.applyConfig content env.viraContext defaultPipeline >>= \case
        Left err -> throwError $ PipelineConfigurationError $ InterpreterError err
        Right p -> do
          logPipeline Info "Successfully applied vira.hs configuration"
          pure $ patchPipelineForCli env.viraContext p
    False -> do
      logPipeline Info "No vira.hs found - using default pipeline"
      pure $ patchPipelineForCli env.viraContext defaultPipeline
  where
    patchPipelineForCli :: ViraContext -> ViraPipeline -> ViraPipeline
    patchPipelineForCli ctx pipeline = case ctx.ciMode of
      FullBuild -> pipeline
      LocalBuild ->
        pipeline
          { build = BuildStage {flakes = pipeline.build.flakes, systems = []}
          }
      BuildOnly ->
        pipeline
          { signoff = pipeline.signoff {enable = False}
          , cache = CacheStage {url = Nothing}
          , build = BuildStage {flakes = pipeline.build.flakes, systems = []}
          }

-- | Implementation: Build flakes
buildImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  Eff es (NonEmpty BuildResult)
buildImpl pipeline = do
  logPipeline Info $ "Building " <> show (length pipeline.build.flakes) <> " flakes"
  -- Validate nix options against whitelist
  case validateNixOptions pipeline.nix.options of
    [] -> pass
    bad -> throwError $ PipelineConfigurationError $ MalformedConfig $ "Disallowed nix options: " <> show bad <> ". Allowed: " <> show allowedNixOptions
  -- Build each flake sequentially and return BuildResult for each
  forM pipeline.build.flakes $ \flake ->
    buildFlake pipeline.build.systems pipeline.nix flake

-- | Pretty-print DevourFlakeResult in a concise format
prettyDevourResult :: FilePath -> DevourFlakeResult -> Text
prettyDevourResult flakePath (DevourFlakeResult systems) =
  renderStrict $
    layoutPretty defaultLayoutOptions $
      vsep
        [ "Build outputs for" <+> pretty flakePath <> ":"
        , indent 2 $ vsep $ map prettySystem (Map.toList systems)
        ]
  where
    prettySystem :: (System, SystemOutputs) -> Doc ann
    prettySystem (System sys, SystemOutputs {byName}) =
      pretty sys
        <> ":"
        <+> pretty (Map.size byName)
        <+> "packages"
        <+> parens (hsep $ punctuate comma $ map pretty $ take 5 $ Map.keys byName)
        <> if Map.size byName > 5 then ", ..." else mempty

-- | Build a single flake
buildFlake ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  [System] ->
  NixConfig ->
  Flake ->
  Eff es BuildResult
buildFlake systems nixCfg (Flake flakePath overrideInputs) = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
  let args =
        DevourFlakeArgs
          { flakePath = flakePath
          , systems
          , outLink = Just (flakePath </> "result")
          , overrideInputs = overrideInputs
          , nixOptions = nixCfg.options
          }

  -- Prefetch flake inputs before building (for devourFlakePath and target flake)
  logPipeline Info "Prefetching flake inputs"
  runProcess repoDir $ proc nix $ prefetchFlakeInputs args

  -- Run build process from working directory
  logPipeline Info $ "Building flake at " <> toText flakePath
  runProcess repoDir $ proc nix $ devourFlake args

  -- Return relative path to result symlink (relative to repo root)
  let resultPath = flakePath </> "result"
  logPipeline Info $ "Build succeeded, result at " <> toText resultPath

  -- Parse the JSON result
  devourResult <- liftIO $ eitherDecodeFileStrict $ repoDir </> resultPath
  case devourResult of
    Left err ->
      throwError $ DevourFlakeMalformedOutput resultPath err
    Right parsed -> do
      logPipeline Info $ prettyDevourResult flakePath parsed
      pure $ BuildResult flakePath resultPath parsed

-- | Implementation: Push to cache
cacheImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
cacheImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let repoDir = env.viraContext.repoDir
  case pipeline.cache.url of
    Nothing -> do
      logPipeline Warning "Cache disabled, skipping"
    Just urlText -> do
      logPipeline Info $ "Pushing " <> show (length buildResults) <> " build results to cache"

      -- Parse cache URL
      (serverEndpoint, cacheName) <- case Attic.Url.parseCacheUrl urlText of
        Left err -> throwError $ parseErrorToPipelineError urlText err
        Right result -> pure result

      -- Get attic server info (token validated by lookupEndpointWithToken)
      server <- case do
        atticConfig <- env.tools.attic.status
        -- Get server name for endpoint (only if it has a token)
        serverName <-
          lookupEndpointWithToken atticConfig serverEndpoint
            & maybeToRight (AtticTool.MissingEndpoint serverEndpoint)
        -- Create server (token already validated by lookupEndpointWithToken)
        pure $ AtticServer serverName serverEndpoint of
        Left err -> throwError $ atticErrorToPipelineError urlText serverEndpoint err
        Right result -> pure result

      -- Push to cache - paths are relative to repoDir
      let pathsToPush = fmap (.resultPath) buildResults
      logPipeline Info $ "Pushing " <> show (length pathsToPush) <> " result files: " <> show (toList pathsToPush)
      let pushProc = Attic.atticPushProcess server cacheName pathsToPush
      runProcess repoDir pushProc
      logPipeline Info "Cache push succeeded"
  where
    parseErrorToPipelineError :: Text -> Attic.Url.ParseError -> PipelineError
    parseErrorToPipelineError url err =
      PipelineConfigurationError $
        MalformedConfig $
          "Invalid cache URL '" <> url <> "': " <> show err

    atticErrorToPipelineError :: Text -> AtticServerEndpoint -> AtticTool.ConfigError -> PipelineError
    atticErrorToPipelineError url _endpoint err =
      let suggestion = AtticTool.configErrorToSuggestion err
          msg = "Attic configuration error for cache URL '" <> url <> "': " <> show err
       in pipelineToolError msg (Just suggestion)

-- | Implementation: Create signoff (one per system)
signoffImpl ::
  ( Concurrent :> es
  , Process :> es
  , Log (RichMessage IO) :> es
  , IOE :> es
  , FileSystem :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
signoffImpl pipeline buildResults = do
  env <- ER.ask @PipelineEnv
  let commitId = env.viraContext.commitId
      mCloneUrl = env.viraContext.cloneUrl
      repoDir = env.viraContext.repoDir
  if pipeline.signoff.enable
    then do
      case mCloneUrl of
        Nothing ->
          throwError $
            pipelineToolError
              ("Signoff enabled but no remote URL is available. Add an 'origin' remote or disable signoff." :: Text)
              (Nothing :: Maybe Text)
        Just cloneUrl -> do
          -- Extract unique systems from all build results
          let systems = extractSystems $ fmap (.devourResult) (toList buildResults)
              signoffNames = fmap (\system -> "vira/" <> toString system) (toList systems)
          case nonEmpty signoffNames of
            Nothing -> throwError $ DevourFlakeMalformedOutput "build results" "No systems found in build results"
            Just names -> do
              -- Detect platform based on clone URL
              case detectPlatform cloneUrl of
                Nothing ->
                  throwError $
                    pipelineToolError
                      ("Signoff enabled but could not detect platform from clone URL: " <> cloneUrl <> ". Must be GitHub or Bitbucket.")
                      (Nothing :: Maybe Text)
                Just platform -> do
                  Signoff.performSignoff commitId platform repoDir names
    else
      logPipeline Warning "Signoff disabled, skipping"

-- | Environment variables passed to hooks (only values from ViraContext or derived from it)
hookEnvVars :: ViraContext -> [(Text, Text)]
hookEnvVars ctx =
  [ ("VIRA_BRANCH", toText ctx.branch)
  , ("VIRA_COMMIT_ID", toText ctx.commitId)
  ]
    <> maybe [] (\name -> [("VIRA_REPO", name)]) (repoNameFromCloneUrl ctx.cloneUrl)

{- | Execute a named hook command with environment variables.

Subprocess stdout/stderr are streamed line by line to @sink@. The hook
is killed (process group) if it runs longer than @timeoutMicros@.
Returns @Left@ with a diagnostic when the hook is missing, exits
non-zero, or times out.
-}
runHook ::
  HooksConfig ->
  -- | Hook name
  Text ->
  -- | Environment variables to set; override any same-named inherited vars
  [(Text, Text)] ->
  -- | Working directory
  FilePath ->
  -- | Sink for subprocess output
  Sink Text ->
  -- | Timeout in microseconds; 'Nothing' disables the timeout
  Maybe Int ->
  IO (Either Text ())
runHook hooksConfig hookName envVars workDir sink mTimeoutMicros =
  case Map.lookup hookName hooksConfig of
    Nothing -> pure $ Left $ "Hook '" <> hookName <> "' not found in operator configuration"
    Just cmd -> doRun cmd
  where
    doRun cmd = do
      -- VIRA_* values take precedence over any same-named inherited vars,
      -- and Map.union deduplicates so execve receives a single value per key.
      currentEnv <- getEnvironment
      let processEnv =
            Map.toList $
              Map.fromList (map (bimap toString toString) envVars)
                `Map.union` Map.fromList currentEnv
          cp =
            (proc "sh" ["-c", toString cmd])
              { env = Just processEnv
              , cwd = Just workDir
              , std_out = CreatePipe
              , std_err = CreatePipe
              , create_group = True
              }
      withCreateProcess cp $ \_ mStdoutH mStderrH ph -> do
        let stdoutH = fromMaybe (error "Expected stdout handle") mStdoutH
            stderrH = fromMaybe (error "Expected stderr handle") mStderrH
        withAsync (drainHandleWith (\x -> x) stdoutH sink) $ \stdoutAsync ->
          withAsync (drainHandleWith (\x -> x) stderrH sink) $ \stderrAsync -> do
            mExit <- case mTimeoutMicros of
              Nothing -> Just <$> waitForProcess ph
              Just t -> Timeout.timeout t (waitForProcess ph)
            case mExit of
              Nothing -> do
                interruptProcessGroupOf ph `CE.catch` \(_ :: SomeException) -> pass
                _ <- waitForProcess ph
                wait stdoutAsync
                wait stderrAsync
                let secs = maybe 0 (`div` 1_000_000) mTimeoutMicros
                pure $ Left $ "Hook command timed out after " <> show secs <> "s"
              Just ExitSuccess -> do
                wait stdoutAsync
                wait stderrAsync
                pure $ Right ()
              Just (ExitFailure code) -> do
                wait stdoutAsync
                wait stderrAsync
                pure $ Left $ "Hook command exited with code " <> show code

-- | Implementation: Execute post-build hooks
postBuildImpl ::
  ( Log (RichMessage IO) :> es
  , IOE :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  ViraPipeline ->
  Eff es ()
postBuildImpl pipeline = do
  env <- ER.ask @PipelineEnv
  case pipeline.hooks.onSuccess of
    Nothing -> logPipeline Info "No post-build hook configured, skipping"
    Just hookName -> executeHook env hookName

-- | Execute a hook: look up its command, run it, and handle the result
executeHook ::
  ( Log (RichMessage IO) :> es
  , IOE :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  ) =>
  PipelineEnv ->
  Text ->
  Eff es ()
executeHook env hookName = do
  let ctx = env.viraContext
      envVars = hookEnvVars ctx
  logPipeline Info $ "Running post-build hook: " <> hookName
  result <- liftIO $ runHook env.availableHooks hookName envVars ctx.repoDir env.logSink (Just hookTimeoutMicros)
  case result of
    Left err ->
      throwError $
        pipelineToolError
          ("Post-build hook '" <> hookName <> "' failed: " <> err)
          (Nothing :: Maybe Text)
    Right () ->
      logPipeline Info $ "Post-build hook '" <> hookName <> "' succeeded"

-- | Maximum hook runtime before vira kills the subprocess (5 minutes).
hookTimeoutMicros :: Int
hookTimeoutMicros = 300 * 1_000_000

-- | Default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage {flakes = one defaultFlake, systems = []}
    , nix = NixConfig {options = []}
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    , hooks = Hooks {onSuccess = Nothing}
    }
  where
    defaultFlake = Flake "." mempty

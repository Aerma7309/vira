{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vira.CI.Pipeline.Implementation (
  runPipeline,

  -- * Used in tests
  defaultPipeline,
  checkDomain,
  isLoopbackHost,
  isIpLiteral,
  sanitiseHeaderName,
  sanitiseHeaderValue,
) where

import Prelude hiding (asks, id)

import Attic qualified
import Attic.Config (lookupEndpointWithToken)
import Attic.Types (AtticServer (..), AtticServerEndpoint)
import Attic.Url qualified
import Colog (Severity (..))
import Colog.Message (RichMessage)
import Control.Exception (try)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (splitOn, strip)
import Data.Text qualified as T
import DevourFlake (DevourFlakeArgs (..), devourFlake, prefetchFlakeInputs)
import DevourFlake.Result (DevourFlakeResult (..), SystemOutputs (..), extractSystems)
import Effectful
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext (..))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Environment (Environment, getEnvironment)
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, doesFileExist)
import Effectful.Git.Command.Clone qualified as Git
import Effectful.Git.Platform (detectPlatform)
import Effectful.Git.Types (Commit (id))
import Effectful.Process (Process)
import Effectful.Reader.Static qualified as ER
import Network.HTTP.Req (
  HttpException,
  NoReqBody (..),
  ReqBodyBs (..),
  defaultHttpConfig,
  header,
  ignoreResponse,
  req,
  responseTimeout,
  runReq,
  useURI,
 )
import Network.HTTP.Req qualified as Req
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>))
import System.Nix.Core (nix)
import System.Nix.System (System (..))
import System.Process (proc)
import Text.URI (Authority (..), URI, mkURI, unRText, uriAuthority, uriScheme)
import Vira.CI.Configuration qualified as Configuration
import Vira.CI.Context (CIMode (..), ViraContext (..))
import Vira.CI.Error (ConfigurationError (..), PipelineError (..), pipelineToolError)
import Vira.CI.Pipeline.Effect
import Vira.CI.Pipeline.Process (runProcess)
import Vira.CI.Pipeline.Signoff qualified as Signoff
import Vira.CI.Pipeline.Type (BuildStage (..), CacheStage (..), Flake (..), HttpMethod (..), NixConfig (..), PostBuildStage (..), SignoffStage (..), ViraPipeline (..), WebhookConfig (..), allowedNixOptions, substituteVars, validateNixOptions)
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
          PostBuild pipeline buildResults -> postBuildImpl pipeline buildResults
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
          , -- Don't fire webhooks when only building (webhooks are side effects)
            postBuild = PostBuildStage {webhooks = []}
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

-- | Implementation: Fire post-build webhooks
postBuildImpl ::
  ( Log (RichMessage IO) :> es
  , IOE :> es
  , ER.Reader LogContext :> es
  , ER.Reader PipelineEnv :> es
  , Error PipelineError :> es
  , Environment :> es
  ) =>
  ViraPipeline ->
  NonEmpty BuildResult ->
  Eff es ()
postBuildImpl pipeline _buildResults = do
  env <- ER.ask @PipelineEnv
  let ctx = env.viraContext
      hooks = pipeline.postBuild.webhooks
  if null hooks
    then logPipeline Info "No post-build webhooks configured, skipping"
    else do
      let viraBindings =
            [ ("VIRA_BRANCH", toText ctx.branch)
            , ("VIRA_COMMIT_ID", toText ctx.commitId)
            , ("VIRA_CLONE_URL", maybe "" identity ctx.cloneUrl)
            , ("VIRA_REPO_DIR", toText ctx.repoDir)
            , ("VIRA_ONLY_BUILD", if ctx.ciMode == BuildOnly then "true" else "false")
            ]
      machineEnv <- getEnvironment
      let allowedEnvNames =
            Set.fromList $
              maybe [] ((map strip . splitOn ",") . toText) $
                lookup "VIRA_WEBHOOK_ALLOWED_ENV" machineEnv
          allowedEnvBindings =
            [ (toText k, toText v)
            | (k, v) <- machineEnv
            , Set.member (toText k) allowedEnvNames
            ]
          -- viraBindings last so $VIRA_* cannot be shadowed by the env allowlist
          allBindings = allowedEnvBindings <> viraBindings
          allowedDomains =
            fmap (Set.fromList . filter (not . T.null) . map strip . splitOn ",") $
              toText <$> lookup "VIRA_WEBHOOK_ALLOWED_DOMAINS" machineEnv
      when (isNothing allowedDomains) $
        throwError $
          pipelineToolError
            ( "VIRA_WEBHOOK_ALLOWED_DOMAINS is not set on the CI machine; webhooks are disabled by default. "
                <> "Set it to a comma-separated list of allowed domains to enable post-build webhooks." ::
                Text
            )
            (Nothing :: Maybe Text)
      forM_ (zip [1 :: Int ..] hooks) $ \(idx, hook) -> do
        let safeUrl = redactSecrets allBindings hook.webhookUrl
            label = "webhook #" <> show idx <> " (" <> safeUrl <> ")"
        logPipeline Info $ "Firing post-build " <> label
        result <- liftIO $ fireWebhook allBindings allowedDomains hook
        case result of
          Left err ->
            throwError $
              pipelineToolError
                ("Post-build " <> label <> " failed: " <> err)
                (Nothing :: Maybe Text)
          Right () ->
            logPipeline Info $ "Post-build " <> label <> " succeeded"

{- | Validate a resolved webhook URL against the domain allowlist.

Returns @Right uri@ if the request may proceed, @Left errMsg@ otherwise.

  * @Nothing@ allowlist — @VIRA_WEBHOOK_ALLOWED_DOMAINS@ unset; all requests blocked (deny-by-default).
  * @Just domains@ — host must appear in the set; empty set blocks everything.

Only HTTPS is permitted. Loopback addresses and IP literals are always rejected
(SSRF prevention). @templateUrl@ is used in error messages to avoid leaking
substituted secrets.
-}
checkDomain :: Maybe (Set.Set Text) -> Text -> Text -> Either Text URI
checkDomain Nothing _resolvedUrl _templateUrl =
  Left "VIRA_WEBHOOK_ALLOWED_DOMAINS is not set; webhooks are disabled by default. Set it on the CI machine to enable webhooks."
checkDomain (Just allowedDomains) resolvedUrl templateUrl =
  case mkURI resolvedUrl of
    Nothing -> Left $ "Invalid webhook URL (could not parse): " <> templateUrl
    Just uri -> do
      let scheme = fmap unRText (uriScheme uri)
      case scheme of
        Just "https" -> Right ()
        Just s -> Left $ "Webhook URL scheme '" <> s <> "' is not allowed; only https is permitted (template: " <> templateUrl <> ")"
        Nothing -> Left $ "Webhook URL has no scheme (template: " <> templateUrl <> ")"
      case uriAuthority uri of
        Right auth ->
          let host = unRText (authHost auth)
           in if isLoopbackHost host
                then Left $ "Webhook URL host is a loopback address and cannot be used as a webhook target (template: " <> templateUrl <> ")"
                else
                  if isIpLiteral host
                    then Left $ "Webhook URL host is an IP address literal; use a hostname from VIRA_WEBHOOK_ALLOWED_DOMAINS instead (template: " <> templateUrl <> ")"
                    else
                      if Set.member host allowedDomains
                        then Right uri
                        else Left $ "Webhook URL host '" <> host <> "' is not in VIRA_WEBHOOK_ALLOWED_DOMAINS (template: " <> templateUrl <> ")"
        _ -> Left $ "Webhook URL has no host (template: " <> templateUrl <> ")"

{- | Return @True@ if the host is a loopback address (@localhost@, @127.x@,
@::1@, @::ffff:127.x@, @0.0.0.0@). Always rejected to prevent SSRF.
-}
isLoopbackHost :: Text -> Bool
isLoopbackHost host =
  host == "localhost"
    || host == "::1"
    || host == "0.0.0.0"
    || T.isPrefixOf "127." host
    || T.isPrefixOf "::ffff:127." host

{- | Return @True@ if the host looks like an IP address literal (IPv4, IPv6, or
bare colon-notation). Rejected to prevent allowlisting of internal addresses.
-}
isIpLiteral :: Text -> Bool
isIpLiteral host =
  (T.isPrefixOf "[" host && T.isSuffixOf "]" host)
    || (T.all (\c -> c == '.' || isDigit c) host && T.any (== '.') host)
    || T.any (== ':') host

-- | Sanitise an HTTP header name (RFC 7230 token characters only). Prevents header injection.
sanitiseHeaderName :: Text -> Text
sanitiseHeaderName = T.filter isHeaderTokenChar
  where
    isHeaderTokenChar :: Char -> Bool
    isHeaderTokenChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` ("!#$%&'*+-.^_`|~" :: String)

-- | Sanitise an HTTP header value by stripping @\\r@, @\\n@, @\\0@.
sanitiseHeaderValue :: Text -> Text
sanitiseHeaderValue = T.filter (\c -> c /= '\r' && c /= '\n' && c /= '\0')

{- | Redact substituted variable values from an error message before logging.

Values with ≥ 4 characters are replaced with @***@. Short values are skipped
to avoid corrupting normal diagnostic text.
-}
redactSecrets :: [(Text, Text)] -> Text -> Text
redactSecrets bindings msg =
  foldr redactOne msg secretValues
  where
    secretValues :: [Text]
    secretValues = filter (\v -> T.length v >= 4) (map snd bindings)

    redactOne :: Text -> Text -> Text
    redactOne secret = T.replace secret "***"

{- | Fire a single webhook. Performs variable substitution, validates the URL,
disables redirects, and redacts secrets from any error messages returned.
-}
fireWebhook :: [(Text, Text)] -> Maybe (Set.Set Text) -> WebhookConfig -> IO (Either Text ())
fireWebhook bindings allowedDomains hook = do
  let resolvedUrl = substituteVars bindings hook.webhookUrl
      resolvedHeaders = map (\(k, v) -> (sanitiseHeaderName k, sanitiseHeaderValue (substituteVars bindings v))) hook.headers
      resolvedBody = fmap (substituteVars bindings) hook.body
      bodyBytes = maybe "" encodeUtf8 resolvedBody
      templateUrl = hook.webhookUrl
  case checkDomain allowedDomains resolvedUrl templateUrl of
    Left err -> pure $ Left err
    Right uri ->
      case useURI uri of
        Nothing -> pure $ Left $ "Could not parse URI scheme (expected https://) for webhook (template: " <> templateUrl <> ")"
        Just (Left _) -> pure $ Left $ "HTTP scheme is not allowed; only HTTPS is permitted for webhooks (template: " <> templateUrl <> ")"
        Just (Right (httpsUrl, _)) -> do
          let noRedirectConfig = defaultHttpConfig {Req.httpConfigRedirectCount = 0}
              opts =
                responseTimeout (30 * 1_000_000)
                  <> mconcat [header (encodeUtf8 k) (encodeUtf8 v) | (k, v) <- resolvedHeaders]
          result <- try @HttpException $
            runReq noRedirectConfig $
              case hook.method of
                GET -> void $ req Req.GET httpsUrl NoReqBody ignoreResponse opts
                POST -> void $ req Req.POST httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
                PUT -> void $ req Req.PUT httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
                PATCH -> void $ req Req.PATCH httpsUrl (ReqBodyBs bodyBytes) ignoreResponse opts
          pure $ bimap (\ex -> redactSecrets bindings $ "HTTP error for webhook (template: " <> templateUrl <> "): " <> fromString (show ex)) identity result

-- | Default pipeline configuration
defaultPipeline :: ViraPipeline
defaultPipeline =
  ViraPipeline
    { build = BuildStage {flakes = one defaultFlake, systems = []}
    , nix = NixConfig {options = []}
    , cache = CacheStage Nothing
    , signoff = SignoffStage False
    , postBuild = PostBuildStage {webhooks = []}
    }
  where
    defaultFlake = Flake "." mempty

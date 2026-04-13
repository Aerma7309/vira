{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use 'fromString' from Relude" #-}
{-# HLINT ignore "Use toText" #-}
{-# HLINT ignore "Use alternative" #-}

module Vira.CI.Pipeline.Type where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import GHC.Records.Compat
import Relude (Bool (..), Char, Eq (..), FilePath, Generic, Maybe (..), NonEmpty, Show, Text, maybe, mempty, notElem, ($), (<>), (||))
import System.Nix.System (System)

-- | CI Pipeline configuration types
data ViraPipeline = ViraPipeline
  { build :: BuildStage
  , nix :: NixConfig
  , cache :: CacheStage
  , signoff :: SignoffStage
  , postBuild :: PostBuildStage
  }
  deriving stock (Generic, Show)

data BuildStage = BuildStage
  { flakes :: NonEmpty Flake
  , systems :: [System]
  }
  deriving stock (Generic, Show)

-- | Nix-level configuration (options and experimental features)
newtype NixConfig = NixConfig
  { options :: [(Text, Text)]
  {- ^ Nix @--option key value@ flags. Only whitelisted keys are allowed;
    see 'allowedNixOptions'.
  -}
  }
  deriving stock (Generic, Show)

{- | Whitelist of Nix option keys that are safe to set per-project.

Secrets (like @access-tokens@) must NOT be added here — they belong
in @nix.conf@ on the CI machine, not in @vira.hs@.
-}
allowedNixOptions :: [Text]
allowedNixOptions =
  [ "sandbox" -- e.g. "relaxed" or "false"
  , "cores" -- CPU cores per build
  , "max-jobs" -- parallel build jobs
  , "allow-import-from-derivation" -- IFD control
  ]

{- | Validate that all nix option keys are in the whitelist.
Returns a list of disallowed keys.
-}
validateNixOptions :: [(Text, Text)] -> [Text]
validateNixOptions opts =
  [k | (k, _) <- opts, k `notElem` allowedNixOptions]

-- | Configuration for building a flake at a specific path
data Flake = Flake
  { path :: FilePath
  , overrideInputs :: [(Text, Text)]
  }
  deriving stock (Generic, Show)

{- | Allows using string literals for Flake paths with optional record update

Examples:
  "." :: Flake                                    -- Simple path
  "./doc" { overrideInputs = [...] } :: Flake     -- With overrides
-}
instance IsString Flake where
  fromString s = Flake (fromString s) []

newtype SignoffStage = SignoffStage
  { enable :: Bool
  }
  deriving stock (Generic, Show)

-- TODO: Switch url type to URI from modern-uri for better type safety
newtype CacheStage = CacheStage
  { url :: Maybe Text
  }
  deriving stock (Generic, Show)

-- | HTTP method for webhook requests
data HttpMethod = GET | POST | PUT | PATCH
  deriving stock (Generic, Show, Eq)

{- | A single outbound webhook triggered after a successful build.

Variable substitution (@$VAR@) is performed on 'webhookUrl', header values,
and 'body'.  Two namespaces are available:

  * @$VIRA_BRANCH@, @$VIRA_COMMIT_ID@, @$VIRA_CLONE_URL@, @$VIRA_REPO_DIR@,
    @$VIRA_ONLY_BUILD@ — always substituted from the build context.
  * Any other @$VAR@ — substituted from the CI machine environment if @VAR@
    appears in @VIRA_WEBHOOK_ALLOWED_ENV@; otherwise replaced with empty string.
-}
data WebhookConfig = WebhookConfig
  { webhookUrl :: Text
  , method :: HttpMethod
  , headers :: [(Text, Text)]
  , body :: Maybe Text
  }
  deriving stock (Generic, Show)

{- | Smart constructor for 'WebhookConfig'.

Named-field syntax does not work inside the @hint@ interpreter ('NoFieldSelectors'),
so use this instead:

@
webhook GET "https://example.com/notify" [] Nothing
@
-}
webhook :: HttpMethod -> Text -> [(Text, Text)] -> Maybe Text -> WebhookConfig
webhook m url hdrs bd = WebhookConfig {webhookUrl = url, method = m, headers = hdrs, body = bd}

-- | Post-build stage: fire outbound webhooks after a successful pipeline run.
newtype PostBuildStage = PostBuildStage
  { webhooks :: [WebhookConfig]
  }
  deriving stock (Generic, Show)

{- | Substitute @$VAR@ placeholders in text.

Unknown keys are replaced with empty string. Variable names follow identifier
rules (@[A-Za-z_][A-Za-z0-9_]*@). A bare @$@ not followed by a valid
identifier start is kept as-is.
-}
substituteVars :: [(Text, Text)] -> Text -> Text
substituteVars bindings tmpl =
  TL.toStrict $ TB.toLazyText $ go tmpl
  where
    lookupMap :: Map.Map Text Text
    lookupMap = Map.fromList bindings

    go :: Text -> TB.Builder
    go t =
      -- T.breakOn "$" returns (before, fromDollarOnwards); rest is either empty
      -- or starts with '$', so the Just ('$', after) branch is exhaustive.
      let (lit, rest) = T.breakOn "$" t
          prefix = TB.fromText lit
       in case T.uncons rest of
            Nothing -> prefix
            Just ('$', after) ->
              case T.uncons after of
                Just (c, _)
                  | isVarStart c ->
                      let (name, tail_) = T.span isIdentChar after
                          replacement = maybe mempty TB.fromText (Map.lookup name lookupMap)
                       in prefix <> replacement <> go tail_
                -- bare '$' not followed by a valid identifier start — keep it
                _ -> prefix <> TB.singleton '$' <> go after
            -- unreachable: rest from breakOn always starts with '$' when non-empty
            Just _ -> prefix <> go rest
    isVarStart :: Char -> Bool
    isVarStart c = isAsciiUpper c || isAsciiLower c || c == '_'

    isIdentChar :: Char -> Bool
    isIdentChar c = isVarStart c || isDigit c

-- HasField instances for enabling OverloadedRecordUpdate syntax (see vira.hs)
-- NOTE: Do not forgot to fill in these instances if the types above change.
-- In future, we could generically derive them using generics-sop and the like.

instance HasField "path" Flake FilePath where
  hasField (Flake path overrideInputs) = (\x -> Flake x overrideInputs, path)

instance HasField "overrideInputs" Flake [(Text, Text)] where
  hasField (Flake path overrideInputs) = (Flake path, overrideInputs)

instance HasField "flakes" BuildStage (NonEmpty Flake) where
  hasField (BuildStage flakes systems) = (\x -> BuildStage x systems, flakes)

instance HasField "systems" BuildStage [System] where
  hasField (BuildStage flakes systems) = (BuildStage flakes, systems)

instance HasField "options" NixConfig [(Text, Text)] where
  hasField (NixConfig options) = (NixConfig, options)

instance HasField "enable" SignoffStage Bool where
  hasField (SignoffStage enable) = (SignoffStage, enable)

instance HasField "url" CacheStage (Maybe Text) where
  hasField (CacheStage url) = (CacheStage, url)

instance HasField "webhookUrl" WebhookConfig Text where
  hasField wh = (\x -> wh {webhookUrl = x}, wh.webhookUrl)

instance HasField "method" WebhookConfig HttpMethod where
  hasField wh = (\x -> wh {method = x}, wh.method)

instance HasField "headers" WebhookConfig [(Text, Text)] where
  hasField wh = (\x -> wh {headers = x}, wh.headers)

instance HasField "body" WebhookConfig (Maybe Text) where
  hasField wh = (\x -> wh {body = x}, wh.body)

instance HasField "webhooks" PostBuildStage [WebhookConfig] where
  hasField (PostBuildStage webhooks) = (PostBuildStage, webhooks)

instance HasField "build" ViraPipeline BuildStage where
  hasField (ViraPipeline build nix cache signoff postBuild) = (\x -> ViraPipeline x nix cache signoff postBuild, build)

instance HasField "nix" ViraPipeline NixConfig where
  hasField (ViraPipeline build nix cache signoff postBuild) = (\x -> ViraPipeline build x cache signoff postBuild, nix)

instance HasField "cache" ViraPipeline CacheStage where
  hasField (ViraPipeline build nix cache signoff postBuild) = (\x -> ViraPipeline build nix x signoff postBuild, cache)

instance HasField "signoff" ViraPipeline SignoffStage where
  hasField (ViraPipeline build nix cache signoff postBuild) = (\x -> ViraPipeline build nix cache x postBuild, signoff)

instance HasField "postBuild" ViraPipeline PostBuildStage where
  hasField (ViraPipeline build nix cache signoff postBuild) = (ViraPipeline build nix cache signoff, postBuild)

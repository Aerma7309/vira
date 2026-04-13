{-# LANGUAGE OverloadedStrings #-}

module Vira.CI.Configuration (
  applyConfig,
  applyConfigWithRunner,
  configInterpreter,
) where

import Language.Haskell.Hint.Nix
import Language.Haskell.Interpreter (InterpreterError, InterpreterT)
import Language.Haskell.Interpreter qualified as Hint
import Vira.CI.Context (ViraContext)
import Vira.CI.Pipeline.Type (ViraPipeline)

-- | Apply a Haskell configuration file to modify a 'ViraPipeline'
applyConfig ::
  (MonadIO m) =>
  -- | Contents of Haskell config file
  Text ->
  -- | Current 'ViraContext'
  ViraContext ->
  -- | Default 'ViraPipeline' configuration
  ViraPipeline ->
  m (Either InterpreterError ViraPipeline)
applyConfig = applyConfigWithRunner runInterpreterWithNixPackageDb

{- | Like 'applyConfig' but accepts an explicit interpreter runner.

Useful in tests where the Nix package DB may not include locally-built
packages.  Pass a runner that uses the cabal in-place package DB instead:

@
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
applyConfigWithRunner (unsafeRunInterpreterWithArgs ["-package-db", localDb])
@
-}
applyConfigWithRunner ::
  (MonadIO m) =>
  -- | Interpreter runner (e.g. 'runInterpreterWithNixPackageDb')
  (InterpreterT IO ViraPipeline -> IO (Either InterpreterError ViraPipeline)) ->
  -- | Contents of Haskell config file
  Text ->
  -- | Current 'ViraContext'
  ViraContext ->
  -- | Default 'ViraPipeline' configuration
  ViraPipeline ->
  m (Either InterpreterError ViraPipeline)
applyConfigWithRunner runner configContent ctx pipeline =
  liftIO $ runner (configInterpreter configContent ctx pipeline)

{- | The hint interpreter action that evaluates a @vira.hs@ config snippet.

Separated from the runner so callers can supply a different package DB
(e.g. the cabal in-place DB in tests) without duplicating the setup logic.
-}
configInterpreter :: Text -> ViraContext -> ViraPipeline -> InterpreterT IO ViraPipeline
configInterpreter configContent ctx pipeline = do
  Hint.set
    [ Hint.languageExtensions
        Hint.:= [ Hint.OverloadedStrings
                , Hint.OverloadedLists
                , Hint.MultiWayIf
                , Hint.UnknownExtension "OverloadedRecordDot"
                , Hint.UnknownExtension "OverloadedRecordUpdate"
                , Hint.UnknownExtension "RebindableSyntax"
                ]
    ]
  -- Import necessary modules
  Hint.setImports
    [ "Relude"
    , "GHC.Records.Compat"
    , "Vira.CI.Context"
    , "Vira.CI.Pipeline.Type"
    , "Effectful.Git"
    ]
  -- RebindableSyntax requires ifThenElse to be in scope
  let wrappedContent = "let ifThenElse :: Bool -> a -> a -> a; ifThenElse True t _ = t; ifThenElse False _ f = f in " <> configContent
  configFn <- Hint.interpret (toString wrappedContent) (Hint.as :: ViraContext -> ViraPipeline -> ViraPipeline)
  return $ configFn ctx pipeline

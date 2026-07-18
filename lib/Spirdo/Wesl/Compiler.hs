{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compiler pipeline and quasiquoter implementation.
module Spirdo.Wesl.Compiler
  ( module Spirdo.Wesl.Compiler
  , module Spirdo.Wesl.Compiler.Cache
  ) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Word (Word64)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (showFFloat)
import Spirdo.Wesl.Compiler.Cache
import Spirdo.Wesl.Emit
import Spirdo.Wesl.Parser
import Spirdo.Wesl.SourceFile
import Spirdo.Wesl.Syntax
import Spirdo.Wesl.Typecheck
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util (annotateErrorWithSource, renderErrorWithSource)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, makeAbsolute)
import System.FilePath (isAbsolute, makeRelative, normalise, takeDirectory, (<.>), (</>))

overrideValuesText :: [(String, OverrideValue)] -> [(Text, OverrideValue)]
overrideValuesText = map (first T.pack)

data CompileResult = CompileResult
  { crInterface :: !ShaderInterface
  , crSpirv :: !ByteString
  , crDiagnostics :: ![Diagnostic]
  , crSource :: !(Maybe ShaderSource)
  }

data SamplerModeProxy (mode :: SamplerBindingMode) where
  SamplerCombinedP :: SamplerModeProxy 'SamplerCombined
  SamplerSeparateP :: SamplerModeProxy 'SamplerSeparate

withSamplerModeProxy :: SamplerBindingMode -> (forall mode. SamplerModeProxy mode -> r) -> r
withSamplerModeProxy mode k =
  case mode of
    SamplerCombined -> k SamplerCombinedP
    SamplerSeparate -> k SamplerSeparateP

compileToCompiled :: CompileOptions -> CompileResult -> SomeCompiledShader
compileToCompiled opts result =
  withSamplerModeProxy opts.samplerBindingMode $ \(_ :: SamplerModeProxy mode) ->
    SomeCompiledShader (CompiledShader @mode (result.crSpirv) (result.crInterface) (result.crSource))

withCompiled ::
  CompileOptions ->
  CompileResult ->
  (forall mode iface. CompiledShader mode iface -> Either CompileError a) ->
  Either CompileError a
withCompiled opts result k =
  case compileToCompiled opts result of
    SomeCompiledShader shader -> k shader

-- | Compile WESL source to a fully prepared shader.
compile :: Source -> Either CompileError SomeShader
compile = compileWith []

-- | Compile WESL source with option overrides (inline only).
compileWith :: [Option] -> Source -> Either CompileError SomeShader
compileWith overrides src =
  let opts = applyOptions overrides defaultCompileOptions
  in case src of
      SourceInline name text -> do
        result <- first (annotateErrorWithSource (Just name) text) (compileInlineResult opts False name text)
        withCompiled opts result $ \shader -> do
          prep <- toCompileError (prepareShader shader)
          pure (SomeShader (shaderFromPrepared prep))
      SourceFile _ ->
        Left (CompileError "file inputs require compileFile/compileFileWith" Nothing Nothing)

-- | Compile WESL source with diagnostics (inline only).
compileWithDiagnostics :: [Option] -> Source -> Either CompileError (SomeShader, [Diagnostic])
compileWithDiagnostics overrides src =
  let opts = applyOptions overrides defaultCompileOptions
  in case src of
      SourceInline name text -> do
        result <- first (annotateErrorWithSource (Just name) text) (compileInlineResult opts True name text)
        withCompiled opts result $ \shader -> do
          prep <- toCompileError (prepareShader shader)
          pure (SomeShader (shaderFromPrepared prep), result.crDiagnostics)
      SourceFile _ ->
        Left (CompileError "file inputs require compileFile/compileFileWithDiagnostics" Nothing Nothing)

-- | Compile a WESL file to a fully prepared shader.
compileFile :: FilePath -> IO (Either CompileError SomeShader)
compileFile = compileFileWith []

-- | Compile a WESL file with option overrides.
compileFileWith :: [Option] -> FilePath -> IO (Either CompileError SomeShader)
compileFileWith overrides path = do
  let opts = applyOptions overrides defaultCompileOptions
  result <- compileFileResult opts False path
  pure $ do
    cr <- result
    withCompiled opts cr $ \shader -> do
      prep <- toCompileError (prepareShader shader)
      pure (SomeShader (shaderFromPrepared prep))

-- | Compile a WESL file and return diagnostics.
compileFileWithDiagnostics :: [Option] -> FilePath -> IO (Either CompileError (SomeShader, [Diagnostic]))
compileFileWithDiagnostics overrides path = do
  let opts = applyOptions overrides defaultCompileOptions
  result <- compileFileResult opts True path
  pure $ do
    cr <- result
    withCompiled opts cr $ \shader -> do
      prep <- toCompileError (prepareShader shader)
      pure (SomeShader (shaderFromPrepared prep), cr.crDiagnostics)

compileInlineResult :: CompileOptions -> Bool -> FilePath -> String -> Either CompileError CompileResult
compileInlineResult opts wantDiagnostics name src = do
  moduleAst0 <- parseModuleWith opts.enabledFeatures src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst2 <- inferOverrideTypes [] "" moduleAst1
  moduleAst2' <- lowerOverridesWith [] (overrideValuesText opts.overrideValues) moduleAst2
  moduleAst <- resolveConstExprs [] "" moduleAst2'
  unless (null moduleAst.modImports) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "" "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface opts moduleAst
  spirvBytes <- emitSpirv opts moduleAst iface
  diags <-
    if wantDiagnostics
      then collectDiagnosticsMerged opts [] moduleAst
      else Right []
  pure CompileResult
    { crInterface = iface
    , crSpirv = spirvBytes
    , crDiagnostics = diags
    , crSource = Just (ShaderSource name (T.pack src))
    }

parseModuleMap :: CompileOptions -> Map.Map FilePath Text -> Either CompileError (Map.Map FilePath ModuleAst)
parseModuleMap opts modules =
  fmap Map.fromList (mapM parseOne (Map.toList modules))
  where
    parseOne (path, src) =
      case parseModuleWith opts.enabledFeatures (T.unpack src) of
        Left err -> Left (annotateErrorWithSource (Just path) (T.unpack src) err)
        Right ast -> Right (path, ast)

validateInlineImports :: FilePath -> Imports mods -> [ModuleNode] -> Either CompileError ()
validateInlineImports rootName importSet nodes = do
  let rootDir = takeDirectory rootName
  let provided = importsNames importSet
  let usedFiles =
        [ node.mnFile
        | node <- nodes
        , normalise node.mnFile /= normalise rootName
        ]
  let used =
        map
          (normalizeModuleKey . makeRelative rootDir)
          usedFiles
  let providedSet = Set.fromList (map normalizeModuleKey provided)
  let usedSet = Set.fromList (map normalizeModuleKey used)
  let missing = Set.toList (Set.difference usedSet providedSet)
  let extra = Set.toList (Set.difference providedSet usedSet)
  case (missing, extra) of
    ([], []) -> Right ()
    _ ->
      Left
        ( CompileError
            ( "inline import mismatch"
                <> formatImportDelta "missing" missing
                <> formatImportDelta "extra" extra
            )
            Nothing
            Nothing
        )
  where
    showListComma xs = T.unpack (T.intercalate ", " (map T.pack xs))

    formatImportDelta _ [] = ""
    formatImportDelta label xs = " (" <> label <> ": " <> showListComma (sort xs) <> ")"

validateInlineImportNames :: Imports mods -> Either CompileError ()
validateInlineImportNames importSet =
  unless (null collisions) $
    Left
      ( CompileError
          ("duplicate import modules after normalization: " <> showListComma (map showCollision collisions))
          Nothing
          Nothing
      )
  where
    groups =
      Map.fromListWith (<>)
        [ (normalizeModuleKey name, [name])
        | name <- importsNames importSet
        ]
    collisions =
      [ (normalizedName, sort rawNames)
      | (normalizedName, rawNames) <- Map.toList groups
      , length rawNames > 1
      ]
    showCollision (normalizedName, rawNames) =
      normalizedName <> " (from " <> showListComma rawNames <> ")"
    showListComma names = T.unpack (T.intercalate ", " (map T.pack names))

compileInlineResultWithImports :: CompileOptions -> Bool -> FilePath -> Imports mods -> String -> Either CompileError CompileResult
compileInlineResultWithImports opts wantDiagnostics rootName importSet src = do
  validateInlineImportNames importSet
  let rootDir = takeDirectory rootName
  moduleAst0 <- parseModuleWith opts.enabledFeatures src
  let importMap = normalizeImportsForRoot rootDir (importsMap importSet)
  astMap <- parseModuleMap opts importMap
  (nodes, linked) <- resolveImportsInline opts rootName moduleAst0 astMap
  validateInlineImports rootName importSet nodes
  linked' <- resolveTypeAliases linked
  let rootPath = modulePathFromFile rootDir rootName
  linked'' <- inferOverrideTypes rootPath rootDir linked'
  lowered0 <- lowerOverridesWith rootPath (overrideValuesText opts.overrideValues) linked''
  lowered <- resolveConstExprs rootPath rootDir lowered0
  diags <-
    if wantDiagnostics
      then collectDiagnosticsMerged opts rootPath lowered
      else do
        _ <- validateConstAssertsMerged opts rootPath lowered
        pure []
  iface <- buildInterface opts lowered
  spirvBytes <- emitSpirv opts lowered iface
  pure CompileResult
    { crInterface = iface
    , crSpirv = spirvBytes
    , crDiagnostics = diags
    , crSource = Just (ShaderSource rootName (T.pack src))
    }

compileFileResult :: CompileOptions -> Bool -> FilePath -> IO (Either CompileError CompileResult)
compileFileResult opts wantDiagnostics path = do
  result <- try (compileFileResultUnchecked opts wantDiagnostics path) :: IO (Either IOException (Either CompileError CompileResult))
  pure $
    case result of
      Left ioErr ->
        Left
          ( CompileError
              ("failed to compile file: " <> path <> " (" <> show ioErr <> ")")
              Nothing
              Nothing
          )
      Right compiled -> compiled

compileFileResultUnchecked :: CompileOptions -> Bool -> FilePath -> IO (Either CompileError CompileResult)
compileFileResultUnchecked opts wantDiagnostics path =
  runExceptT $ do
    input <- ExceptT (resolveInputPath path)
    let filePath = input.riCanonical
    sourceFile <- ExceptT (timedPhase opts "read-file" (readBoundedUtf8File "input file" maxSourceUtf8Bytes filePath))
    let src = T.unpack sourceFile.text
    importRoots <- ExceptT (discoverFileImportRoots input.riSelected filePath)
    let annotate :: ExceptT CompileError IO a -> ExceptT CompileError IO a
        annotate = withExceptT annotateErr
        annotateErr err
          | " --> " `isInfixOf` err.ceMessage = err
          | otherwise =
              case (err.ceLine, err.ceColumn) of
                (Just _, Just _) -> annotateErrorWithSource (Just filePath) src err
                _ -> err
    moduleAst0 <- annotate (ExceptT (timedPhase opts "parse" (evaluate (parseModuleWith opts.enabledFeatures src))))
    -- resolveImports reads every imported module below this boundary.
    importResult <- liftIO (try (timedPhase opts "imports" (resolveImports opts importRoots filePath (T.length sourceFile.text) moduleAst0)) :: IO (Either IOException (Either CompileError ModuleAst)))
    linked <-
      case importResult of
        Left ioErr ->
          throwE
            ( CompileError
                ("failed to read an imported module while compiling " <> filePath <> " (" <> show ioErr <> ")")
                Nothing
                Nothing
            )
        Right result -> ExceptT (pure result)
    linked' <- annotate (ExceptT (timedPhase opts "type-aliases" (evaluate (resolveTypeAliases linked))))
    rootPackage <- ExceptT (pure (lookupPackageImportRoot importRoots importRoots.firCurrent))
    rootPath <- ExceptT (pure (modulePathFromPackage importRoots importRoots.firCurrent filePath))
    let rootDir = rootPackage.pirPath
    linked'' <- annotate (ExceptT (timedPhase opts "infer-overrides" (evaluate (inferOverrideTypes rootPath rootDir linked'))))
    lowered0 <- annotate (ExceptT (timedPhase opts "overrides" (evaluate (lowerOverridesWith rootPath (overrideValuesText opts.overrideValues) linked''))))
    lowered <- annotate (ExceptT (timedPhase opts "resolve-const" (evaluate (resolveConstExprs rootPath rootDir lowered0))))
    diags <-
      if wantDiagnostics
        then annotate (ExceptT (timedPhase opts "diagnostics" (evaluate (collectDiagnosticsMerged opts rootPath lowered))))
        else do
          _ <- annotate (ExceptT (timedPhase opts "const-asserts" (evaluate (validateConstAssertsMerged opts rootPath lowered))))
          pure []
    iface <- annotate (ExceptT (timedPhase opts "interface" (evaluate (buildInterface opts lowered))))
    spirvBytes <- annotate (ExceptT (timedPhase opts "emit" (evaluate (emitSpirv opts lowered iface))))
    pure CompileResult
      { crInterface = iface
      , crSpirv = spirvBytes
      , crDiagnostics = diags
      , crSource = Just (ShaderSource filePath sourceFile.text)
      }

-- Quasiquoter

-- | Quasiquoter for raw inline WESL source.
wesl :: QuasiQuoter
wesl =
  QuasiQuoter
    { quoteExp = \src -> pure (TH.LitE (TH.StringL src))
    , quotePat = const (fail "wesl: pattern context not supported")
    , quoteType = const (fail "wesl: type context not supported")
    , quoteDec = const (fail "wesl: declaration context not supported")
    }

compileInlineCachedWithImports :: CompileOptions -> FilePath -> Imports mods -> String -> Q (ByteString, ShaderInterface, Maybe ShaderSource)
compileInlineCachedWithImports opts rootName importSet src = do
  either
    (fail . renderErrorWithSource (Just rootName) src)
    pure
    (validateInlineImportNames importSet)
  let sourceInfo = Just (ShaderSource rootName (T.pack src))
  cached <- TH.runIO (timed opts "cache-read" (loadWeslCacheWithImports opts rootName importSet src))
  maybe
    compileFresh
    ( \entry@(_, iface) ->
        case validateCachedShader opts rootName entry of
          Right _ -> pure (fst entry, iface, sourceInfo)
          Left _ -> compileFresh
    )
    cached
  where
    compileFresh =
      either
        (fail . renderErrorWithSource (Just rootName) src)
        ( \result -> do
            let bytes = result.crSpirv
                iface = result.crInterface
            TH.runIO (timed opts "cache-write" (writeWeslCacheWithImports opts rootName importSet src bytes iface))
            pure (bytes, iface, result.crSource)
        )
        (compileInlineResultWithImports opts False rootName importSet src)

-- | Compile inline WESL with typed imports at compile-time.
spirv :: CompileOptions -> Imports mods -> String -> Q Exp
spirv opts importSet src = do
  (bytes, iface, sourceInfo) <- compileInlineCachedWithImports opts "<inline>" importSet src
  preparedExpWith opts bytes iface sourceInfo

validateCachedShader :: CompileOptions -> String -> (ByteString, ShaderInterface) -> Either String ()
validateCachedShader opts label (bytes, iface) =
  withSamplerModeProxy opts.samplerBindingMode $ \(_ :: SamplerModeProxy mode) ->
    case (prepareShader (CompiledShader bytes iface Nothing :: CompiledShader mode '[]) :: Either String (PreparedShader mode '[])) of
      Left err -> Left ("cached shader invalid for " <> label <> ": " <> err)
      Right _ -> Right ()

preparedExpWith :: CompileOptions -> ByteString -> ShaderInterface -> Maybe ShaderSource -> Q Exp
preparedExpWith opts bytes iface sourceInfo = do
  bytesExp <- bytesToExp bytes
  ifaceExp <- interfaceToExp iface
  let sourceExp =
        case sourceInfo of
          Nothing -> TH.ConE 'Nothing
          Just (ShaderSource name text) ->
            TH.AppE
              (TH.ConE 'Just)
              ( TH.AppE
                  (TH.AppE (TH.ConE 'ShaderSource) (TH.LitE (TH.StringL name)))
                  (TH.AppE (TH.VarE 'T.pack) (TH.LitE (TH.StringL (T.unpack text))))
              )
  ifaceTy <- either (fail . ("wesl: " <>)) pure (interfaceToType iface)
  let modeTy =
        case opts.samplerBindingMode of
          SamplerCombined -> TH.PromotedT 'SamplerCombined
          SamplerSeparate -> TH.PromotedT 'SamplerSeparate
  let compiledTy =
        TH.AppT
          (TH.AppT (TH.ConT ''CompiledShader) modeTy)
          ifaceTy
  let compiledExp =
        TH.SigE
          (TH.AppE (TH.AppE (TH.AppE (TH.ConE 'CompiledShader) bytesExp) ifaceExp) sourceExp)
          compiledTy
  let prepExp = TH.AppE (TH.VarE 'unsafePrepareInline) compiledExp
  let shaderExp = TH.AppE (TH.VarE 'shaderFromPrepared) prepExp
  pure (TH.SigE shaderExp (TH.AppT (TH.AppT (TH.ConT ''Shader) modeTy) ifaceTy))

timed :: CompileOptions -> String -> IO a -> IO a
timed opts label action =
  if not (opts.cacheVerbose)
    then action
    else do
      t0 <- getCurrentTime
      result <- action
      t1 <- getCurrentTime
      putStrLn ("[spirdo] " <> label <> ": " <> show (diffUTCTime t1 t0))
      pure result

timedPhase :: CompileOptions -> String -> IO a -> IO a
timedPhase opts label action =
  if not (opts.timingVerbose)
    then action
    else do
      t0 <- getMonotonicTimeNSec
      !result <- action
      t1 <- getMonotonicTimeNSec
      putStrLn ("[spirdo] " <> label <> ": " <> formatNs (t1 - t0))
      pure result

formatNs :: Word64 -> String
formatNs ns =
  let ms = fromIntegral ns / (1000 * 1000) :: Double
  in showFFloat (Just 3) ms "ms"

toCompileError :: Either String a -> Either CompileError a
toCompileError =
  either (\msg -> Left (CompileError msg Nothing Nothing)) Right

unsafePrepareInline :: CompiledShader mode iface -> PreparedShader mode iface
unsafePrepareInline shader =
  case prepareShader shader of
    Left err -> error ("prepareShader: " <> err)
    Right prep -> prep


-- Package metadata

-- | Package metadata parsed from @wesl.toml@.
data PackageInfo = PackageInfo
  { pkgName :: String
  , pkgVersion :: Maybe String
  , pkgRoot :: FilePath
  , pkgSourceRoot :: FilePath
  , pkgDependencies :: [PackageDependency]
  , pkgEdition :: String
  , pkgPackageManager :: Maybe String
  , pkgManifest :: FilePath
  } deriving (Eq, Show)

-- | Dependency entry from @wesl.toml@.
data PackageDependency = PackageDependency
  { depName :: String
  , depVersion :: Maybe String
  , depPackage :: Maybe String
  , depPath :: Maybe FilePath
  } deriving (Eq, Show)

data LoadedPackage = LoadedPackage
  { package :: !PackageInfo
  , manifestBytes :: !Int
  }

data TomlSection
  = TomlSectionNone
  | TomlSectionPackage
  | TomlSectionDependencies
  | TomlSectionDependency String
  | TomlSectionOther
  deriving (Eq, Show)

data ManifestState = ManifestState
  { msSection :: !TomlSection
  , msSections :: !(Set.Set String)
  , msPackageFields :: !(Map.Map String String)
  , msDependencies :: !(Map.Map String PackageDependency)
  }

emptyManifestState :: ManifestState
emptyManifestState =
  ManifestState TomlSectionNone Set.empty Map.empty Map.empty

-- | Find and parse the nearest @wesl.toml@ above a file path.
discoverPackageInfo :: FilePath -> IO (Either CompileError (Maybe LoadedPackage))
discoverPackageInfo filePath = findWeslToml (takeDirectory filePath)
  where
    findWeslToml dir = do
      let candidate = dir </> "wesl.toml"
      exists <- doesFileExist candidate
      if exists
        then fmap Just <$> loadWeslPackage candidate
        else
          let parent = takeDirectory dir
          in if parent == dir
              then pure (Right Nothing)
              else findWeslToml parent

parseWeslToml :: FilePath -> IO (Either CompileError PackageInfo)
parseWeslToml path = fmap (fmap (\loaded -> loaded.package)) (loadWeslPackage path)

loadWeslPackage :: FilePath -> IO (Either CompileError LoadedPackage)
loadWeslPackage path = runExceptT $ do
  manifest <- ExceptT (canonicalizeExistingPath "WESL manifest" path)
  manifestFile <- ExceptT (readBoundedUtf8File "WESL manifest" maxManifestUtf8Bytes manifest)
  packageInfo <- ExceptT (pure (parseWeslTomlText manifest (T.unpack manifestFile.text)))
  sourceRoot <- ExceptT (canonicalizeExistingPath "package root" packageInfo.pkgSourceRoot)
  pure
    LoadedPackage
      { package = packageInfo { pkgSourceRoot = sourceRoot }
      , manifestBytes = manifestFile.bytes
      }

parseWeslTomlText :: FilePath -> String -> Either CompileError PackageInfo
parseWeslTomlText manifest contents = do
  parsed <- foldM parseLine emptyManifestState (zip [1 :: Int ..] (lines contents))
  edition <-
    case Map.lookup "edition" parsed.msPackageFields of
      Just value -> Right value
      Nothing -> Left (manifestError manifest Nothing "missing required [package] field edition")
  let packageManager = Map.lookup "package-manager" parsed.msPackageFields
  case packageManager of
    Just value | value `notElem` ["npm", "cargo"] ->
      Left (manifestError manifest Nothing ("unsupported package-manager: " <> value))
    _ -> pure ()
  let manifestRoot = takeDirectory manifest
      source = Map.findWithDefault "./shaders" "root" parsed.msPackageFields
      name = Map.findWithDefault "wesl-package" "name" parsed.msPackageFields
      version = Map.lookup "version" parsed.msPackageFields
  pure
    PackageInfo
      { pkgName = name
      , pkgVersion = version
      , pkgRoot = manifestRoot
      , pkgSourceRoot = normalise (manifestRoot </> source)
      , pkgDependencies = Map.elems parsed.msDependencies
      , pkgEdition = edition
      , pkgPackageManager = packageManager
      , pkgManifest = manifest
      }
  where
    parseLine state (lineNumber, rawLine) =
      let line = trim (stripTomlComment rawLine)
      in case line of
          [] -> Right state
          ('[':_) -> parseSection manifest lineNumber line state
          _ ->
            case state.msSection of
              TomlSectionPackage -> parsePackageField manifest lineNumber line state
              TomlSectionDependencies -> parseDependencyField manifest lineNumber line state
              TomlSectionDependency name -> parseDependencyTableField manifest lineNumber name line state
              TomlSectionNone -> Right state
              TomlSectionOther -> Right state

parseSection :: FilePath -> Int -> String -> ManifestState -> Either CompileError ManifestState
parseSection manifest lineNumber line =
  case line of
    '[' : rest
      | not (null rest) && last rest == ']' && '[' `notElem` init rest && ']' `notElem` init rest ->
          selectSection (init rest)
    _ -> const (Left (manifestError manifest (Just lineNumber) "malformed manifest section header"))
  where
    selectSection name state =
      case name of
        "package" -> enterRelevantSection name TomlSectionPackage state
        "dependencies" -> enterRelevantSection name TomlSectionDependencies state
        _
          | "dependencies." `isPrefixOf` name ->
              enterDependencySection (drop (length ("dependencies." :: String)) name) state
          | otherwise -> Right state { msSection = TomlSectionOther }

    enterRelevantSection name section state
      | Set.member name state.msSections =
          Left (manifestError manifest (Just lineNumber) ("duplicate manifest section [" <> name <> "]"))
      | otherwise =
          Right
            state
              { msSection = section
              , msSections = Set.insert name state.msSections
              }

    enterDependencySection dependencyName state = do
      unless (validWeslName dependencyName) $
        Left (manifestError manifest (Just lineNumber) ("invalid dependency name: " <> dependencyName))
      let sectionName = "dependencies." <> dependencyName
      when (Set.member sectionName state.msSections || Map.member dependencyName state.msDependencies) $
        Left (manifestError manifest (Just lineNumber) ("duplicate dependency: " <> dependencyName))
      let dependency = PackageDependency dependencyName Nothing Nothing Nothing
      Right
        state
          { msSection = TomlSectionDependency dependencyName
          , msSections = Set.insert sectionName state.msSections
          , msDependencies = Map.insert dependencyName dependency state.msDependencies
          }

parsePackageField :: FilePath -> Int -> String -> ManifestState -> Either CompileError ManifestState
parsePackageField manifest lineNumber line state = do
  (rawKey, rawValue) <- parseAssignment manifest lineNumber line
  let key = trim rawKey
  case key of
    "root" -> recordStringField "root" rawValue
    "source_root" -> recordStringField "root" rawValue
    "edition" -> recordStringField "edition" rawValue
    "package-manager" -> recordStringField "package-manager" rawValue
    "name" -> recordStringField "name" rawValue
    "version" -> recordStringField "version" rawValue
    _ -> Right state
  where
    recordStringField key rawValue = do
      when (Map.member key state.msPackageFields) $
        Left (manifestError manifest (Just lineNumber) ("duplicate [package] field " <> key))
      value <- parseTomlString manifest lineNumber rawValue
      when (key == "root" && isManifestAbsolute value) $
        Left (manifestError manifest (Just lineNumber) ("package root must be relative: " <> value))
      Right state { msPackageFields = Map.insert key value state.msPackageFields }

parseDependencyField :: FilePath -> Int -> String -> ManifestState -> Either CompileError ManifestState
parseDependencyField manifest lineNumber line state = do
  (rawName, rawValue) <- parseAssignment manifest lineNumber line
  let name = trim rawName
  unless (validWeslName name) $
    Left (manifestError manifest (Just lineNumber) ("invalid dependency name: " <> name))
  when (Map.member name state.msDependencies) $
    Left (manifestError manifest (Just lineNumber) ("duplicate dependency: " <> name))
  fields <-
    if "{" `isPrefixOf` trim rawValue
      then parseInlineDependency manifest lineNumber rawValue
      else do
        version <- parseTomlString manifest lineNumber rawValue
        Right (Map.singleton "version" version)
  dependency <- dependencyFromFields manifest lineNumber name fields
  Right state { msDependencies = Map.insert name dependency state.msDependencies }

parseDependencyTableField :: FilePath -> Int -> String -> String -> ManifestState -> Either CompileError ManifestState
parseDependencyTableField manifest lineNumber name line state = do
  (rawKey, rawValue) <- parseAssignment manifest lineNumber line
  let key = trim rawKey
  unless (key `elem` ["package", "path", "version"]) $
    Left (manifestError manifest (Just lineNumber) ("unsupported dependency field: " <> key))
  value <- parseTomlString manifest lineNumber rawValue
  dependency <-
    case Map.lookup name state.msDependencies of
      Nothing -> Left (manifestError manifest (Just lineNumber) ("dependency table is not registered: " <> name))
      Just current -> addDependencyField manifest lineNumber key value current
  Right state { msDependencies = Map.insert name dependency state.msDependencies }

dependencyFromFields :: FilePath -> Int -> String -> Map.Map String String -> Either CompileError PackageDependency
dependencyFromFields manifest lineNumber name fields = do
  let packageName = Map.lookup "package" fields
      rawPath = Map.lookup "path" fields
      version = Map.lookup "version" fields
  when (packageName /= Nothing && rawPath /= Nothing) $
    Left (manifestError manifest (Just lineNumber) ("dependency " <> name <> " cannot specify both package and path"))
  dependencyPath <-
    case rawPath of
      Nothing -> Right Nothing
      Just value -> do
        when (isManifestAbsolute value) $
          Left (manifestError manifest (Just lineNumber) ("dependency path must be relative: " <> value))
        Right (Just (normalise (takeDirectory manifest </> value)))
  let dependency =
        PackageDependency
          { depName = name
          , depVersion = version
          , depPackage = if dependencyPath == Nothing then Just (fromMaybe name packageName) else Nothing
          , depPath = dependencyPath
          }
  Right dependency

addDependencyField :: FilePath -> Int -> String -> String -> PackageDependency -> Either CompileError PackageDependency
addDependencyField manifest lineNumber key value dependency =
  case key of
    "package" -> do
      when (dependency.depPackage /= Nothing) duplicateField
      when (dependency.depPath /= Nothing) conflictingFields
      Right dependency { depPackage = Just value }
    "path" -> do
      when (dependency.depPath /= Nothing) duplicateField
      when (dependency.depPackage /= Nothing) conflictingFields
      when (isManifestAbsolute value) $
        Left (manifestError manifest (Just lineNumber) ("dependency path must be relative: " <> value))
      Right dependency { depPath = Just (normalise (takeDirectory manifest </> value)) }
    "version" -> do
      when (dependency.depVersion /= Nothing) duplicateField
      Right dependency { depVersion = Just value }
    _ -> Left (manifestError manifest (Just lineNumber) ("unsupported dependency field: " <> key))
  where
    duplicateField = Left (manifestError manifest (Just lineNumber) ("duplicate dependency field: " <> key))
    conflictingFields =
      Left (manifestError manifest (Just lineNumber) ("dependency " <> dependency.depName <> " cannot specify both package and path"))

parseInlineDependency :: FilePath -> Int -> String -> Either CompileError (Map.Map String String)
parseInlineDependency manifest lineNumber raw =
  let value = trim raw
  in case value of
      '{' : rest | not (null rest) && last rest == '}' -> do
        let body = trim (init rest)
        if null body
          then Right Map.empty
          else foldM addField Map.empty (splitTomlComma body)
      _ -> Left (manifestError manifest (Just lineNumber) "dependency must be an inline table with package or path")
  where
    addField fields rawField = do
      (rawKey, rawValue) <- parseAssignment manifest lineNumber rawField
      let key = trim rawKey
      unless (key `elem` ["package", "path", "version"]) $
        Left (manifestError manifest (Just lineNumber) ("unsupported dependency field: " <> key))
      when (Map.member key fields) $
        Left (manifestError manifest (Just lineNumber) ("duplicate dependency field: " <> key))
      value <- parseTomlString manifest lineNumber rawValue
      Right (Map.insert key value fields)

parseAssignment :: FilePath -> Int -> String -> Either CompileError (String, String)
parseAssignment manifest lineNumber line =
  case break (== '=') line of
    (key, '=' : value)
      | not (null (trim key)) && not (null (trim value)) -> Right (key, value)
    _ -> Left (manifestError manifest (Just lineNumber) "malformed manifest assignment")

parseTomlString :: FilePath -> Int -> String -> Either CompileError String
parseTomlString manifest lineNumber raw =
  case reads (trim raw) of
    [(value, "")] -> Right value
    _ -> Left (manifestError manifest (Just lineNumber) "manifest field must be a quoted string")

splitTomlComma :: String -> [String]
splitTomlComma = go False False [] []
  where
    go _ _ current parts [] = reverse (trim (reverse current) : parts)
    go quoted escaped current parts (char:rest)
      | escaped = go quoted False (char : current) parts rest
      | quoted && char == '\\' = go quoted True (char : current) parts rest
      | char == '"' = go (not quoted) False (char : current) parts rest
      | char == ',' && not quoted = go False False [] (trim (reverse current) : parts) rest
      | otherwise = go quoted False (char : current) parts rest

stripTomlComment :: String -> String
stripTomlComment = go False False
  where
    go _ _ [] = []
    go quoted escaped (char:rest)
      | escaped = char : go quoted False rest
      | quoted && char == '\\' = char : go quoted True rest
      | char == '"' = char : go (not quoted) False rest
      | char == '#' && not quoted = []
      | otherwise = char : go quoted False rest

validWeslName :: String -> Bool
validWeslName name =
  case name of
    [] -> False
    firstChar : rest ->
      (firstChar == '_' || isAlpha firstChar)
        && all (\char -> char == '_' || isAlphaNum char) rest
        && case validateImportName (SrcPos 1 1) (T.pack name) of
          Right () -> True
          Left _ -> False

isManifestAbsolute :: FilePath -> Bool
isManifestAbsolute path =
  isAbsolute path
    || case path of
      separator : _ | separator `elem` ['/', '\\'] -> True
      drive : ':' : _ -> isAlpha drive
      _ -> False

manifestError :: FilePath -> Maybe Int -> String -> CompileError
manifestError manifest lineNumber message =
  CompileError
    ( manifest
        <> maybe "" ((":" <>) . show) lineNumber
        <> ": " <> message
    )
    Nothing
    Nothing

discoverFileImportRoots :: FilePath -> FilePath -> IO (Either CompileError FileImportRoots)
discoverFileImportRoots selectedPath filePath = runExceptT $ do
  selectedPackage <- ExceptT (discoverPackageInfo selectedPath)
  discoveredPackage <-
    case selectedPackage of
      Just package -> pure (Just package)
      Nothing
        | normalise selectedPath /= normalise filePath -> ExceptT (discoverPackageInfo filePath)
        | otherwise -> pure Nothing
  case discoveredPackage of
    Nothing -> do
      let packagePath = takeDirectory filePath
          packageRoot = PackageImportRoot packagePath PackageDirectory Nothing Map.empty
          packageRegistry = Map.singleton packagePath packageRoot
          semanticPaths = Map.singleton packagePath []
      pure (FileImportRoots packagePath packageRegistry semanticPaths)
    Just package -> do
      let packageMetadata = package.package
      packageKind <- ExceptT (packageRootKind packageMetadata.pkgSourceRoot)
      let currentRoot =
            PackageImportRoot packageMetadata.pkgSourceRoot packageKind (Just packageMetadata.pkgManifest) Map.empty
      unless (packageContains currentRoot filePath) $
        throwE
          ( CompileError
              ( "input file is outside package root: " <> filePath
                  <> " is not within " <> packageMetadata.pkgSourceRoot
                  <> " from " <> packageMetadata.pkgManifest
              )
              Nothing
              Nothing
          )
      packageRegistry <- loadPackageRegistry package
      semanticPaths <-
        ExceptT
          (pure (buildPackageSemanticPaths packageMetadata.pkgSourceRoot packageRegistry))
      pure (FileImportRoots packageMetadata.pkgSourceRoot packageRegistry semanticPaths)

maxPackageCount :: Int
maxPackageCount = 256

maxPackageDependencyDepth :: Int
maxPackageDependencyDepth = 64

maxPackageManifestUtf8Bytes :: Int
maxPackageManifestUtf8Bytes = 1024 * 1024

data PackageLoadState = PackageLoadState
  { registry :: !(Map.Map FilePath PackageImportRoot)
  , packagesByManifest :: !(Map.Map FilePath LoadedPackage)
  , manifestBySourceRoot :: !(Map.Map FilePath FilePath)
  , cumulativeManifestBytes :: !Int
  }

loadPackageRegistry :: LoadedPackage -> ExceptT CompileError IO (Map.Map FilePath PackageImportRoot)
loadPackageRegistry rootPackage = do
  when (rootPackage.manifestBytes > maxPackageManifestUtf8Bytes) $
    throwE
      ( CompileError
          ( "cumulative package manifest bytes " <> show rootPackage.manifestBytes
              <> " exceeds limit " <> show maxPackageManifestUtf8Bytes
              <> " while loading " <> rootPackage.package.pkgManifest
          )
          Nothing
          Nothing
      )
  let initialState =
        PackageLoadState
          { registry = Map.empty
          , packagesByManifest = Map.singleton rootPackage.package.pkgManifest rootPackage
          , manifestBySourceRoot =
              Map.singleton rootPackage.package.pkgSourceRoot rootPackage.package.pkgManifest
          , cumulativeManifestBytes = rootPackage.manifestBytes
          }
  finalState <- loadPackage initialState rootPackage
  case validatePackageDependencyDepth rootPackage.package.pkgSourceRoot finalState.registry of
    Left err -> throwE err
    Right () -> pure finalState.registry
  where
    loadPackage state loadedPackage = do
      if Map.member package.pkgSourceRoot state.registry
        then pure state
        else do
          (stateWithDependencies, reversedDependencies) <-
            foldM
              (loadDependency package)
              (state, [])
              package.pkgDependencies
          kind <- ExceptT (packageRootKind package.pkgSourceRoot)
          let dependencyRoots =
                Map.fromList
                  [ (T.pack dependency.depName, dependencyPackage.pkgSourceRoot)
                  | (dependency, loadedDependency) <- reverse reversedDependencies
                  , let dependencyPackage = loadedDependency.package
                  ]
              packageRoot =
                PackageImportRoot
                  package.pkgSourceRoot
                  kind
                  (Just package.pkgManifest)
                  dependencyRoots
              stateWithRoot =
                stateWithDependencies
                  { registry = Map.insert package.pkgSourceRoot packageRoot stateWithDependencies.registry
                  }
          foldM
            (\currentState (_, dependencyPackage) -> loadPackage currentState dependencyPackage)
            stateWithRoot
            (reverse reversedDependencies)
      where
        package = loadedPackage.package

    loadDependency package (state, dependencies) dependency =
      case dependency.depPath of
        Nothing ->
          throwE
            ( CompileError
                ( "package-manager dependency resolution is unsupported: " <> dependency.depName
                    <> " refers to " <> fromMaybe dependency.depName dependency.depPackage
                    <> " in " <> package.pkgManifest
                    <> maybe "" (" using " <>) package.pkgPackageManager
                )
                Nothing
                Nothing
            )
        Just dependencyPath -> do
          canonicalDirectory <- ExceptT (canonicalizeExistingPath ("path dependency " <> dependency.depName) dependencyPath)
          isDirectory <- liftIO (doesDirectoryExist canonicalDirectory)
          unless isDirectory $
            throwE
              ( CompileError
                  ( "path dependency " <> dependency.depName <> " is not a directory: " <> canonicalDirectory
                  )
                  Nothing
                  Nothing
              )
          manifest <- ExceptT (canonicalizeExistingPath "WESL manifest" (canonicalDirectory </> "wesl.toml"))
          case Map.lookup manifest state.packagesByManifest of
            Just loadedDependency -> pure (state, (dependency, loadedDependency) : dependencies)
            Nothing -> do
              let packageCount = Map.size state.packagesByManifest + 1
              when (packageCount > maxPackageCount) $
                throwE
                  ( CompileError
                      ( "package count " <> show packageCount
                          <> " exceeds limit " <> show maxPackageCount
                          <> " while loading " <> manifest
                      )
                      Nothing
                      Nothing
                  )
              loadedDependency <- ExceptT (loadWeslPackage manifest)
              let dependencyPackage = loadedDependency.package
              case Map.lookup dependencyPackage.pkgSourceRoot state.manifestBySourceRoot of
                Just registeredManifest
                  | registeredManifest /= manifest ->
                      throwE
                        ( CompileError
                            ( "package source root collision: " <> dependencyPackage.pkgSourceRoot
                                <> " is declared by both " <> registeredManifest
                                <> " and " <> manifest
                            )
                            Nothing
                            Nothing
                        )
                _ -> pure ()
              let manifestBytes = state.cumulativeManifestBytes + loadedDependency.manifestBytes
              when (manifestBytes > maxPackageManifestUtf8Bytes) $
                throwE
                  ( CompileError
                      ( "cumulative package manifest bytes " <> show manifestBytes
                          <> " exceeds limit " <> show maxPackageManifestUtf8Bytes
                          <> " while loading " <> manifest
                      )
                      Nothing
                      Nothing
                  )
              let stateWithDependency =
                    state
                      { packagesByManifest = Map.insert manifest loadedDependency state.packagesByManifest
                      , manifestBySourceRoot =
                          Map.insert dependencyPackage.pkgSourceRoot manifest state.manifestBySourceRoot
                      , cumulativeManifestBytes = manifestBytes
                      }
              pure (stateWithDependency, (dependency, loadedDependency) : dependencies)


validatePackageDependencyDepth :: FilePath -> Map.Map FilePath PackageImportRoot -> Either CompileError ()
validatePackageDependencyDepth rootPath packages = do
  rootComponent <-
    maybe
      ( Left
          ( CompileError
              ("package graph is missing its root: " <> rootPath)
              Nothing
              Nothing
          )
      )
      Right
      (Map.lookup rootPath componentByPackage)
  let rootDepth = Map.findWithDefault 1 rootComponent componentWeights
      finalDepths = relaxDepths componentCount (Map.singleton rootComponent rootDepth)
      (deepestManifest, deepestDepth) =
        foldl' selectDeeper (manifestFor rootPath, rootDepth) (Map.toList finalDepths)
  if deepestDepth <= maxPackageDependencyDepth
    then Right ()
    else
      Left
        ( CompileError
            ( "package dependency depth " <> show deepestDepth
                <> " exceeds limit " <> show maxPackageDependencyDepth
                <> " while loading " <> deepestManifest
            )
            Nothing
            Nothing
        )
  where
    adjacency =
      Map.map (Map.elems . (.pirDependencies)) packages
    components =
      zip
        [0 :: Int ..]
        ( stronglyConnComp
            [ (packagePath, packagePath, Map.findWithDefault [] packagePath adjacency)
            | packagePath <- Map.keys packages
            ]
        )
    componentPackages stronglyConnected =
      case stronglyConnected of
        AcyclicSCC packagePath -> [packagePath]
        CyclicSCC packagePaths -> packagePaths
    componentByPackage =
      Map.fromList
        [ (packagePath, componentId)
        | (componentId, stronglyConnected) <- components
        , packagePath <- componentPackages stronglyConnected
        ]
    componentWeights =
      Map.fromList
        [ (componentId, length (componentPackages stronglyConnected))
        | (componentId, stronglyConnected) <- components
        ]
    componentRepresentatives =
      Map.fromList
        [ ( componentId
          , case componentPackages stronglyConnected of
              packagePath : _ -> manifestFor packagePath
              [] -> manifestFor rootPath
          )
        | (componentId, stronglyConnected) <- components
        ]
    componentEdges =
      Map.fromList
        [ ( componentId
          , Set.toList
              ( Set.fromList
                  [ targetComponent
                  | packagePath <- componentPackages stronglyConnected
                  , targetPath <- Map.findWithDefault [] packagePath adjacency
                  , Just targetComponent <- [Map.lookup targetPath componentByPackage]
                  , targetComponent /= componentId
                  ]
              )
          )
        | (componentId, stronglyConnected) <- components
        ]
    componentCount = length components

    relaxDepths remaining depths
      | remaining <= 0 = depths
      | otherwise =
          relaxDepths
            (remaining - 1)
            (foldl' relaxComponent depths (map fst components))

    relaxComponent depths componentId =
      case Map.lookup componentId depths of
        Nothing -> depths
        Just sourceDepth ->
          foldl'
            (\currentDepths targetComponent ->
              let targetDepth =
                    sourceDepth + Map.findWithDefault 1 targetComponent componentWeights
              in Map.insertWith max targetComponent targetDepth currentDepths
            )
            depths
            (Map.findWithDefault [] componentId componentEdges)

    selectDeeper current@(_, currentDepth) (componentId, candidateDepth)
      | candidateDepth <= currentDepth = current
      | otherwise =
          ( Map.findWithDefault (manifestFor rootPath) componentId componentRepresentatives
          , candidateDepth
          )

    manifestFor packagePath =
      case Map.lookup packagePath packages >>= (.pirManifest) of
        Just manifest -> manifest
        Nothing -> packagePath

packageRootKind :: FilePath -> IO (Either CompileError PackageRootKind)
packageRootKind path = do
  isDirectory <- doesDirectoryExist path
  exists <- doesFileExist path
  pure $
    if isDirectory
      then Right PackageDirectory
      else if exists
        then Right PackageModule
        else Left (CompileError ("package root disappeared after resolution: " <> path) Nothing Nothing)

canonicalizeExistingPath :: String -> FilePath -> IO (Either CompileError FilePath)
canonicalizeExistingPath label path = do
  isDirectory <- doesDirectoryExist path
  isFile <- doesFileExist path
  if not (isDirectory || isFile)
    then pure (Left (CompileError (label <> " not found: " <> path) Nothing Nothing))
    else do
      result <- try (canonicalizePath path) :: IO (Either IOException FilePath)
      pure $
        first
          (\ioErr -> CompileError ("failed to resolve " <> label <> ": " <> path <> " (" <> show ioErr <> ")") Nothing Nothing)
          result

data ResolvedInput = ResolvedInput
  { riSelected :: !FilePath
  , riCanonical :: !FilePath
  }

resolveInputPath :: FilePath -> IO (Either CompileError ResolvedInput)
resolveInputPath path = selectInput [path, path <.> "wesl", path <.> "wgsl"]
  where
    selectInput [] = pure (Left (CompileError ("file not found: " <> path) Nothing Nothing))
    selectInput (candidate:rest) = do
      exists <- doesFileExist candidate
      if exists
        then do
          absoluteResult <- try (makeAbsolute candidate) :: IO (Either IOException FilePath)
          case absoluteResult of
            Left ioErr ->
              pure (Left (CompileError ("failed to resolve input file: " <> candidate <> " (" <> show ioErr <> ")") Nothing Nothing))
            Right selected ->
              fmap (ResolvedInput selected) <$> canonicalizeExistingPath "input file" candidate
        else selectInput rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse

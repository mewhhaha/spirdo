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
module Spirdo.Wesl.Compiler where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Bifunctor (first)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace, ord)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (showFFloat, showHex)
import Spirdo.Wesl.Emit
import Spirdo.Wesl.Parser
import Spirdo.Wesl.Syntax
import Spirdo.Wesl.Typecheck
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util (annotateErrorWithSource, renderErrorWithSource)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, isRelative, normalise, takeDirectory, (<.>), (</>))
import Text.Read (readMaybe)

overrideValuesText :: [(String, OverrideValue)] -> [(Text, OverrideValue)]
overrideValuesText = map (first T.pack)

data CompileResult = CompileResult
  { crAst :: !ModuleAst
  , crInterface :: !ShaderInterface
  , crSpirv :: !ByteString
  , crDiagnostics :: ![Diagnostic]
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
    SomeCompiledShader (CompiledShader @mode (result.crSpirv) (result.crInterface))

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
        result <- first (annotateErrorWithSource (Just name) text) (compileInlineResult opts False text)
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
        result <- first (annotateErrorWithSource (Just name) text) (compileInlineResult opts True text)
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

-- | Compile inline WESL to a prepared shader (runtime).
prepareWesl :: String -> Either CompileError SomePreparedShader
prepareWesl = prepareWeslWith defaultCompileOptions

-- | Compile inline WESL with explicit options (runtime).
prepareWeslWith :: CompileOptions -> String -> Either CompileError SomePreparedShader
prepareWeslWith opts src = do
  result <- first (annotateErrorWithSource (Just "<inline>") src) (compileInlineResult opts False src)
  withCompiled opts result $ \shader -> do
    prep <- toCompileError (prepareShader shader)
    pure (SomePreparedShader prep)

-- | Compile inline WESL with diagnostics (runtime).
prepareWeslWithDiagnostics :: CompileOptions -> String -> Either CompileError (SomePreparedShader, [Diagnostic])
prepareWeslWithDiagnostics opts src = do
  result <- first (annotateErrorWithSource (Just "<inline>") src) (compileInlineResult opts True src)
  withCompiled opts result $ \shader -> do
    prep <- toCompileError (prepareShader shader)
    pure (SomePreparedShader prep, result.crDiagnostics)

-- | Compile a WESL file (imports supported).
prepareWeslFile :: FilePath -> IO (Either CompileError SomePreparedShader)
prepareWeslFile = prepareWeslFileWith defaultCompileOptions

-- | Compile a WESL file with explicit options.
prepareWeslFileWith :: CompileOptions -> FilePath -> IO (Either CompileError SomePreparedShader)
prepareWeslFileWith opts path = do
  result <- compileFileResult opts False path
  pure $ do
    cr <- result
    withCompiled opts cr $ \shader -> do
      prep <- toCompileError (prepareShader shader)
      pure (SomePreparedShader prep)

-- | Compile a WESL file and return diagnostics.
prepareWeslFileWithDiagnostics :: CompileOptions -> FilePath -> IO (Either CompileError (SomePreparedShader, [Diagnostic]))
prepareWeslFileWithDiagnostics opts path = do
  result <- compileFileResult opts True path
  pure $ do
    cr <- result
    withCompiled opts cr $ \shader -> do
      prep <- toCompileError (prepareShader shader)
      pure (SomePreparedShader prep, cr.crDiagnostics)

compileInlineResult :: CompileOptions -> Bool -> String -> Either CompileError CompileResult
compileInlineResult opts wantDiagnostics src = do
  moduleAst0 <- parseModuleWith opts.enabledFeatures src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst2 <- inferOverrideTypes [] "" moduleAst1
  moduleAst2' <- lowerOverridesWith [] (overrideValuesText opts.overrideValues) moduleAst2
  moduleAst <- resolveConstExprs [] "" moduleAst2'
  unless (null moduleAst.modImports) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface opts moduleAst
  spirv <- emitSpirv opts moduleAst iface
  diags <-
    if wantDiagnostics
      then collectDiagnosticsMerged opts [] moduleAst
      else Right []
  pure CompileResult
    { crAst = moduleAst
    , crInterface = iface
    , crSpirv = spirv
    , crDiagnostics = diags
    }

compileInlineResultIO :: CompileOptions -> Bool -> String -> IO (Either CompileError CompileResult)
compileInlineResultIO opts wantDiagnostics src = runExceptT $ do
  moduleAst0 <- ExceptT (timedPhase opts "parse" (evaluate (parseModuleWith opts.enabledFeatures src)))
  moduleAst1 <- ExceptT (timedPhase opts "type-aliases" (evaluate (resolveTypeAliases moduleAst0)))
  moduleAst2 <- ExceptT (timedPhase opts "infer-overrides" (evaluate (inferOverrideTypes [] "" moduleAst1)))
  moduleAst2' <- ExceptT (timedPhase opts "overrides" (evaluate (lowerOverridesWith [] (overrideValuesText opts.overrideValues) moduleAst2)))
  moduleAst <- ExceptT (timedPhase opts "resolve-const" (evaluate (resolveConstExprs [] "" moduleAst2')))
  unless (null moduleAst.modImports) $
    throwE (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  _ <- ExceptT (timedPhase opts "validate" (evaluate (validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node])))
  iface <- ExceptT (timedPhase opts "interface" (evaluate (buildInterface opts moduleAst)))
  spirv <- ExceptT (timedPhase opts "emit" (evaluate (emitSpirv opts moduleAst iface)))
  diagList <-
    if wantDiagnostics
      then ExceptT (timedPhase opts "diagnostics" (evaluate (collectDiagnosticsMerged opts [] moduleAst)))
      else pure []
  pure
    CompileResult
      { crAst = moduleAst
      , crInterface = iface
      , crSpirv = spirv
      , crDiagnostics = diagList
      }

compileFileResult :: CompileOptions -> Bool -> FilePath -> IO (Either CompileError CompileResult)
compileFileResult opts wantDiagnostics path =
  runExceptT $ do
    filePath <- ExceptT (resolveInputPath path)
    src <- liftIO (timedPhase opts "read-file" (readFile filePath))
    let annotate :: ExceptT CompileError IO a -> ExceptT CompileError IO a
        annotate = withExceptT (annotateErrorWithSource (Just filePath) src)
    moduleAst0 <- annotate (ExceptT (timedPhase opts "parse" (evaluate (parseModuleWith opts.enabledFeatures src))))
    -- resolveImports performs module linking and validateModuleScopes for file-based builds.
    linked <- annotate (ExceptT (timedPhase opts "imports" (resolveImports opts (dropExtension filePath) moduleAst0)))
    linked' <- annotate (ExceptT (timedPhase opts "type-aliases" (evaluate (resolveTypeAliases linked))))
    let rootDir = takeDirectory filePath
        rootPath = modulePathFromFile rootDir filePath
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
    spirv <- annotate (ExceptT (timedPhase opts "emit" (evaluate (emitSpirv opts lowered iface))))
    pure CompileResult
      { crAst = lowered
      , crInterface = iface
      , crSpirv = spirv
      , crDiagnostics = diags
      }

weslCacheVersion :: String
weslCacheVersion = "wesl-cache-v4"

defaultCacheDir :: FilePath
defaultCacheDir = "dist-newstyle" </> ".wesl-cache"

weslCacheKey :: CompileOptions -> String -> String
weslCacheKey opts src =
  let keyLines =
        [ weslCacheVersion
        , "v=" <> show (opts.spirvVersion)
        , "features=" <> show (opts.enabledFeatures)
        , "overrides=" <> show opts.overrideValues
        , "spec=" <> show opts.overrideSpecMode
        , "samplerMode=" <> show opts.samplerBindingMode
        , "entry=" <> show opts.entryPointName
        , src
        ]
      hash = foldl' updateLine fnv1a64Offset (zip [0 :: Int ..] keyLines)
      hex = showHex hash ""
  in replicate (16 - length hex) '0' <> hex

updateLine :: Word64 -> (Int, String) -> Word64
updateLine acc (ix, line) =
  let acc' =
        if ix == 0
          then acc
          else fnv1a64Step acc (fromIntegral (ord '\n'))
  in foldl' (\a ch -> fnv1a64Step a (fromIntegral (ord ch))) acc' line

fnv1a64Offset :: Word64
fnv1a64Offset = 14695981039346656037

fnv1a64Prime :: Word64
fnv1a64Prime = 1099511628211

fnv1a64Step :: Word64 -> Word8 -> Word64
fnv1a64Step acc byte = (acc `xor` fromIntegral byte) * fnv1a64Prime

fnv1a64 :: ByteString -> Word64
fnv1a64 = BS.foldl' fnv1a64Step fnv1a64Offset

weslCachePaths :: CompileOptions -> String -> (FilePath, FilePath)
weslCachePaths opts src =
  let key = weslCacheKey opts src
      baseDir = if null opts.cacheDir then defaultCacheDir else opts.cacheDir
      base = baseDir </> key
  in (base <.> "spv", base <.> "iface")

loadWeslCache :: CompileOptions -> String -> IO (Maybe (ByteString, ShaderInterface))
loadWeslCache opts src =
  if not (opts.cacheEnabled)
    then pure Nothing
    else do
      let (spvPath, ifacePath) = weslCachePaths opts src
      okSpv <- doesFileExist spvPath
      okIface <- doesFileExist ifacePath
      if not (okSpv && okIface)
        then pure Nothing
        else
          (do
            bytes <- BS.readFile spvPath
            ifaceText <- readFile ifacePath
            case readMaybe ifaceText of
              Just iface -> pure (Just (bytes, iface))
              Nothing -> pure Nothing
          ) `catch` \(_ :: SomeException) -> pure Nothing

writeWeslCache :: CompileOptions -> String -> ByteString -> ShaderInterface -> IO ()
writeWeslCache opts src bytes iface =
  if not (opts.cacheEnabled)
    then pure ()
    else
      let (spvPath, ifacePath) = weslCachePaths opts src
      in (do
            createDirectoryIfMissing True (takeDirectory spvPath)
            BS.writeFile spvPath bytes
            writeFile ifacePath (show iface)
         ) `catch` \(_ :: SomeException) -> pure ()

-- Quasiquoter

-- | Quasiquoter for inline WESL (compile-time).
wesl :: QuasiQuoter
wesl = weslWith defaultCompileOptions

-- | Quasiquoter with explicit compile options.
weslWith :: CompileOptions -> QuasiQuoter
weslWith opts =
  QuasiQuoter
    { quoteExp = weslExpWith opts
    , quotePat = const (fail "wesl: pattern context not supported")
    , quoteType = const (fail "wesl: type context not supported")
    , quoteDec = const (fail "wesl: declaration context not supported")
    }

weslExpWith :: CompileOptions -> String -> Q Exp
weslExpWith opts src = do
  (bytes, iface) <- compileInlineCached opts src
  preparedExpWith opts bytes iface

compileInlineCached :: CompileOptions -> String -> Q (ByteString, ShaderInterface)
compileInlineCached opts src = do
  cached <- TH.runIO (timed opts "cache-read" (loadWeslCache opts src))
  maybe compileFresh pure cached
  where
    compileFresh =
      either
        (fail . renderErrorWithSource (Just "<inline>") src)
        (\result -> do
            let bytes = result.crSpirv
                iface = result.crInterface
            TH.runIO (timed opts "cache-write" (writeWeslCache opts src bytes iface))
            pure (bytes, iface)
        )
        (compileInlineResult opts False src)

preparedExpWith :: CompileOptions -> ByteString -> ShaderInterface -> Q Exp
preparedExpWith opts bytes iface = do
  bytesExp <- bytesToExp bytes
  ifaceExp <- interfaceToExp iface
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
          (TH.AppE (TH.AppE (TH.ConE 'CompiledShader) bytesExp) ifaceExp)
          compiledTy
  let prepExp = TH.AppE (TH.VarE 'unsafePrepareInline) compiledExp
  let shaderExp = TH.AppE (TH.VarE 'shaderFromPrepared) prepExp
  pure (TH.SigE shaderExp (TH.AppT (TH.AppT (TH.ConT ''Shader) modeTy) ifaceTy))

-- | Compile multiple inline WESL shaders in a single splice.
weslBatch :: [(String, String)] -> Q [TH.Dec]
weslBatch = weslBatchWith defaultCompileOptions

-- | Compile multiple inline WESL shaders with explicit options.
weslBatchWith :: CompileOptions -> [(String, String)] -> Q [TH.Dec]
weslBatchWith opts entries = fmap concat (mapM compileOne entries)
  where
    compileOne (nameStr, src) = do
      (bytes, iface) <- compileInlineCached opts src
      expr <- preparedExpWith opts bytes iface
      let name = TH.mkName nameStr
      pure [TH.ValD (TH.VarP name) (TH.NormalB expr) []]

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

-- | Minimal package metadata parsed from @wesl.toml@.
data PackageInfo = PackageInfo
  { pkgName :: String
  , pkgVersion :: Maybe String
  , pkgRoot :: FilePath
  , pkgSourceRoot :: FilePath
  , pkgDependencies :: [PackageDependency]
  } deriving (Eq, Show)

-- | Dependency entry from @wesl.toml@.
data PackageDependency = PackageDependency
  { depName :: String
  , depVersion :: Maybe String
  , depPath :: Maybe FilePath
  } deriving (Eq, Show)

data TomlSection
  = TomlSectionNone
  | TomlSectionPackage
  | TomlSectionDependencies
  | TomlSectionDependency String
  deriving (Eq, Show)

-- | Find and parse the nearest @wesl.toml@ above a file path.
discoverPackageInfo :: FilePath -> IO (Maybe PackageInfo)
discoverPackageInfo filePath = do
  let start = takeDirectory filePath
  findWeslToml start
  where
    findWeslToml dir = do
      let candidate = dir </> "wesl.toml"
      exists <- doesFileExist candidate
      if exists
        then parseWeslToml candidate
        else
          let parent = takeDirectory dir
          in if parent == dir
              then pure Nothing
              else findWeslToml parent

parseWeslToml :: FilePath -> IO (Maybe PackageInfo)
parseWeslToml path = do
  contents <- readFile path
  let root = takeDirectory path
      (_, mName, mVersion, mSource, deps) = foldl (parseLine root) (TomlSectionNone, Nothing, Nothing, Nothing, Map.empty) (lines contents)
      name = fromMaybe "wesl-package" mName
      sourceRoot = fromMaybe "." mSource
  pure (Just (PackageInfo name mVersion root (root </> sourceRoot) (Map.elems deps)))
  where
    parseLine root (section, mName, mVersion, mSource, deps) line =
      let trimmed = trim (takeWhile (/= '#') line)
      in case trimmed of
          [] -> (section, mName, mVersion, mSource, deps)
          ('[':rest) ->
            case dropWhileEnd (== ']') rest of
              "package" -> (TomlSectionPackage, mName, mVersion, mSource, deps)
              "dependencies" -> (TomlSectionDependencies, mName, mVersion, mSource, deps)
              name
                | "dependencies." `isPrefixOf` name ->
                    let depName = drop (length ("dependencies." :: String)) name
                    in (TomlSectionDependency depName, mName, mVersion, mSource, deps)
              _ -> (TomlSectionNone, mName, mVersion, mSource, deps)
          _ ->
            case break (== '=') trimmed of
              (key, '=':val) ->
                let key' = trim key
                    val' = trim val
                in case section of
                    TomlSectionPackage ->
                      case key' of
                        "name" -> (section, Just (stripQuotes val'), mVersion, mSource, deps)
                        "version" -> (section, mName, Just (stripQuotes val'), mSource, deps)
                        "source_root" -> (section, mName, mVersion, Just (stripQuotes val'), deps)
                        _ -> (section, mName, mVersion, mSource, deps)
                    TomlSectionDependencies ->
                      let (dep, deps') = parseDependencyLine root key' val' deps
                      in (section, mName, mVersion, mSource, Map.insert (dep.depName) dep deps')
                    TomlSectionDependency depName ->
                      let deps' = updateDependency root depName key' val' deps
                      in (section, mName, mVersion, mSource, deps')
                    _ -> (section, mName, mVersion, mSource, deps)
              _ -> (section, mName, mVersion, mSource, deps)

    parseDependencyLine root name val deps =
      if "{" `isPrefixOf` val
        then
          let fields = parseInlineTable val
              dep = applyDepFields root (emptyDep name) fields
          in (dep, deps)
        else
          let dep = (emptyDep name) { depVersion = Just (stripQuotes val) }
          in (dep, deps)

    updateDependency root name key val deps =
      let dep0 = Map.findWithDefault (emptyDep name) name deps
          dep1 =
            case key of
              "version" -> dep0 { depVersion = Just (stripQuotes val) }
              "path" -> dep0 { depPath = Just (resolvePath root (stripQuotes val)) }
              _ -> dep0
      in Map.insert name dep1 deps

    emptyDep name = PackageDependency name Nothing Nothing

    applyDepFields root =
      foldl
        (\d (k, v) ->
           case k of
             "version" -> d { depVersion = Just v }
             "path" -> d { depPath = Just (resolvePath root v) }
             _ -> d)

    parseInlineTable raw =
      let inner = trim (dropWhile (== '{') (dropWhileEnd (== '}') raw))
      in mapMaybe parsePair (splitComma inner)

    parsePair item =
      case break (== '=') item of
        (k, '=':v) ->
          let key = trim k
              val = stripQuotes (trim v)
          in if null key then Nothing else Just (key, val)
        _ -> Nothing

    splitComma str = go str [] []
      where
        go [] acc cur = reverse (reverse cur : acc)
        go (c:cs) acc cur
          | c == ',' = go cs (reverse cur : acc) []
          | otherwise = go cs acc (c : cur)

    resolvePath root val =
      if isRelative val
        then normalise (root </> val)
        else val

    trim = dropWhileEnd isSpace . dropWhile isSpace
    dropWhileEnd f = reverse . dropWhile f . reverse
    stripQuotes s =
      case s of
        ('"':rest) -> reverse (drop 1 (reverse rest))
        _ -> s

resolveInputPath :: FilePath -> IO (Either CompileError FilePath)
resolveInputPath path = do
  exists <- doesFileExist path
  if exists
    then pure (Right path)
    else do
      let weslPath = path <.> "wesl"
      let wgslPath = path <.> "wgsl"
      weslExists <- doesFileExist weslPath
      if weslExists
        then pure (Right weslPath)
        else do
          wgslExists <- doesFileExist wgslPath
          if wgslExists
            then pure (Right wgslPath)
            else pure (Left (CompileError ("file not found: " <> path) Nothing Nothing))

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compiler pipeline and quasiquoter implementation.
module Spirdo.Wesl.Compiler where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace, ord)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
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
import Spirdo.Wesl.Util (renderError)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, isRelative, normalise, takeDirectory, (<.>), (</>))
import Text.Read (readMaybe)

overrideValuesText :: [(String, OverrideValue)] -> [(Text, OverrideValue)]
overrideValuesText = map (\(k, v) -> (T.pack k, v))

data CompileResult = CompileResult
  { crAst :: !ModuleAst
  , crInterface :: !ShaderInterface
  , crSpirv :: !ByteString
  , crDiagnostics :: ![Diagnostic]
  }

data SamplerModeProxy (mode :: SamplerBindingMode) where
  SamplerCombinedP :: SamplerModeProxy 'SamplerCombined
  SamplerSeparateP :: SamplerModeProxy 'SamplerSeparate

withSamplerMode :: SamplerBindingMode -> (forall mode. SamplerModeProxy mode -> r) -> r
withSamplerMode mode k =
  case mode of
    SamplerCombined -> k SamplerCombinedP
    SamplerSeparate -> k SamplerSeparateP

compileToCompiled :: CompileOptions -> CompileResult -> SomeCompiledShader
compileToCompiled opts result =
  withSamplerMode opts.samplerBindingMode $ \(_ :: SamplerModeProxy mode) ->
    SomeCompiledShader (CompiledShader @mode (result.crSpirv) (result.crInterface))

-- | Compile inline WESL to a prepared shader (runtime).
prepareWesl :: String -> Either CompileError SomePreparedShader
prepareWesl = prepareWeslWith defaultCompileOptions

-- | Compile inline WESL with explicit options (runtime).
prepareWeslWith :: CompileOptions -> String -> Either CompileError SomePreparedShader
prepareWeslWith opts src = do
  result <- compileInlineResult opts False src
  case compileToCompiled opts result of
    SomeCompiledShader shader -> do
      prep <- toCompileError (prepareShader shader)
      pure (SomePreparedShader prep)

-- | Compile inline WESL with diagnostics (runtime).
prepareWeslWithDiagnostics :: CompileOptions -> String -> Either CompileError (SomePreparedShader, [Diagnostic])
prepareWeslWithDiagnostics opts src = do
  result <- compileInlineResult opts True src
  case compileToCompiled opts result of
    SomeCompiledShader shader -> do
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
    case compileToCompiled opts cr of
      SomeCompiledShader shader -> do
        prep <- toCompileError (prepareShader shader)
        pure (SomePreparedShader prep)

-- | Compile a WESL file and return diagnostics.
prepareWeslFileWithDiagnostics :: CompileOptions -> FilePath -> IO (Either CompileError (SomePreparedShader, [Diagnostic]))
prepareWeslFileWithDiagnostics opts path = do
  result <- compileFileResult opts True path
  pure $ do
    cr <- result
    case compileToCompiled opts cr of
      SomeCompiledShader shader -> do
        prep <- toCompileError (prepareShader shader)
        pure (SomePreparedShader prep, cr.crDiagnostics)

compileInlineResult :: CompileOptions -> Bool -> String -> Either CompileError CompileResult
compileInlineResult opts wantDiagnostics src = do
  moduleAst0 <- parseModuleWith opts.enabledFeatures src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText opts.overrideValues) moduleAst1
  when (not (null moduleAst.modImports)) $
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
compileInlineResultIO opts wantDiagnostics src = do
  moduleAst0 <- timedPhase opts "parse" (evaluate (parseModuleWith opts.enabledFeatures src))
  case moduleAst0 of
    Left err -> pure (Left err)
    Right ast0 -> do
      moduleAst1 <- timedPhase opts "type-aliases" (evaluate (resolveTypeAliases ast0))
      case moduleAst1 of
        Left err -> pure (Left err)
        Right ast1 -> do
          moduleAst2 <- timedPhase opts "overrides" (evaluate (lowerOverridesWith [] (overrideValuesText opts.overrideValues) ast1))
          case moduleAst2 of
            Left err -> pure (Left err)
            Right moduleAst -> do
              if not (null moduleAst.modImports)
                then pure (Left (CompileError "imports require file-based compilation" Nothing Nothing))
                else do
                  let node = ModuleNode "<inline>" [] moduleAst []
                  let constIndex = buildConstIndex [node]
                  let fnIndex = buildFunctionIndex [node]
                  let structIndex = buildStructIndex [node]
                  let overrideIndex = buildOverrideIndex [node]
                  validation <- timedPhase opts "validate" (evaluate (validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]))
                  case validation of
                    Left err -> pure (Left err)
                    Right () -> do
                      ifaceRes <- timedPhase opts "interface" (evaluate (buildInterface opts moduleAst))
                      case ifaceRes of
                        Left err -> pure (Left err)
                        Right iface -> do
                          spirvRes <- timedPhase opts "emit" (evaluate (emitSpirv opts moduleAst iface))
                          case spirvRes of
                            Left err -> pure (Left err)
                            Right spirv -> do
                              diags <-
                                if wantDiagnostics
                                  then timedPhase opts "diagnostics" (evaluate (collectDiagnosticsMerged opts [] moduleAst))
                                  else pure (Right [])
                              case diags of
                                Left err -> pure (Left err)
                                Right diagList ->
                                  pure
                                    ( Right
                                        CompileResult
                                          { crAst = moduleAst
                                          , crInterface = iface
                                          , crSpirv = spirv
                                          , crDiagnostics = diagList
                                          }
                                    )

compileFileResult :: CompileOptions -> Bool -> FilePath -> IO (Either CompileError CompileResult)
compileFileResult opts wantDiagnostics path =
  runExceptT $ do
    filePath <- ExceptT (resolveInputPath path)
    src <- liftIO (timedPhase opts "read-file" (readFile filePath))
    moduleAst0 <- ExceptT (timedPhase opts "parse" (evaluate (parseModuleWith opts.enabledFeatures src)))
    -- resolveImports performs module linking and validateModuleScopes for file-based builds.
    linked <- ExceptT (timedPhase opts "imports" (resolveImports opts (dropExtension filePath) moduleAst0))
    linked' <- ExceptT (timedPhase opts "type-aliases" (evaluate (resolveTypeAliases linked)))
    let rootDir = takeDirectory filePath
        rootPath = modulePathFromFile rootDir filePath
    lowered <- ExceptT (timedPhase opts "overrides" (evaluate (lowerOverridesWith rootPath (overrideValuesText opts.overrideValues) linked')))
    diags <-
      if wantDiagnostics
        then ExceptT (timedPhase opts "diagnostics" (evaluate (collectDiagnosticsMerged opts rootPath lowered)))
        else do
          _ <- ExceptT (timedPhase opts "const-asserts" (evaluate (validateConstAssertsMerged opts rootPath lowered)))
          pure []
    iface <- ExceptT (timedPhase opts "interface" (evaluate (buildInterface opts lowered)))
    spirv <- ExceptT (timedPhase opts "emit" (evaluate (emitSpirv opts lowered iface)))
    pure CompileResult
      { crAst = lowered
      , crInterface = iface
      , crSpirv = spirv
      , crDiagnostics = diags
      }

weslCacheVersion :: String
weslCacheVersion = "wesl-cache-v4"

weslCacheDir :: FilePath
weslCacheDir = "dist-newstyle" </> ".wesl-cache"

weslCacheKey :: CompileOptions -> String -> String
weslCacheKey opts src =
  let keySrc =
        intercalate
          "\n"
          [ weslCacheVersion
          , "v=" <> show (opts.spirvVersion)
          , "features=" <> show (opts.enabledFeatures)
          , "overrides=" <> show opts.overrideValues
          , "spec=" <> show opts.overrideSpecMode
          , "samplerMode=" <> show opts.samplerBindingMode
          , src
          ]
      bytes = BS.pack (map (fromIntegral . ord) keySrc)
      hash = fnv1a64 bytes
      hex = showHex hash ""
  in replicate (16 - length hex) '0' <> hex

fnv1a64 :: ByteString -> Word64
fnv1a64 = BS.foldl' step offset
  where
    offset = 14695981039346656037
    prime = 1099511628211
    step acc byte = (acc `xor` fromIntegral byte) * prime

weslCachePaths :: CompileOptions -> String -> (FilePath, FilePath)
weslCachePaths opts src =
  let key = weslCacheKey opts src
      base = weslCacheDir </> key
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
  case cached of
    Just (bytes, iface) -> pure (bytes, iface)
    Nothing ->
      case compileInlineResult opts False src of
        Left err -> fail (renderError err)
        Right result -> do
          let bytes = result.crSpirv
              iface = result.crInterface
          TH.runIO (timed opts "cache-write" (writeWeslCache opts src bytes iface))
          pure (bytes, iface)

preparedExpWith :: CompileOptions -> ByteString -> ShaderInterface -> Q Exp
preparedExpWith opts bytes iface = do
  bytesExp <- bytesToExp bytes
  ifaceExp <- interfaceToExp iface
  ifaceTy <- case interfaceToType iface of
    Left err -> fail ("wesl: " <> err)
    Right ty -> pure ty
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
  pure (TH.SigE prepExp (TH.AppT (TH.AppT (TH.ConT ''PreparedShader) modeTy) ifaceTy))

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

    applyDepFields root dep fields =
      foldl
        (\d (k, v) ->
           case k of
             "version" -> d { depVersion = Just v }
             "path" -> d { depPath = Just (resolvePath root v) }
             _ -> d)
        dep
        fields

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

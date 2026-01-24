{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Spirdo.Wesl.Compiler where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
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
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (showHex)
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

-- Public API

compileWeslToSpirv :: String -> Either CompileError SomeCompiledShader
compileWeslToSpirv = compileWeslToSpirvWith defaultCompileOptions

compileWeslToSpirvWith :: CompileOptions -> String -> Either CompileError SomeCompiledShader
compileWeslToSpirvWith opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  spirv <- emitSpirv opts moduleAst iface
  let shader = CompiledShader spirv iface
  pure (SomeCompiledShader shader)

compileWeslToSpirvWithDiagnostics :: CompileOptions -> String -> Either CompileError (SomeCompiledShader, [Diagnostic])
compileWeslToSpirvWithDiagnostics opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  spirv <- emitSpirv opts moduleAst iface
  diags <- collectDiagnosticsMerged opts [] moduleAst
  let shader = CompiledShader spirv iface
  pure (SomeCompiledShader shader, diags)

compileWeslToSpirvFile :: FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFile = compileWeslToSpirvFileWith defaultCompileOptions

compileWeslToSpirvFileWith :: CompileOptions -> FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFileWith opts path = do
  inputPath <- resolveInputPath path
  case inputPath of
    Left err -> pure (Left err)
    Right filePath -> do
      src <- readFile filePath
      case parseModuleWith (enabledFeatures opts) src of
        Left err -> pure (Left err)
        Right moduleAst0 -> do
          resolved <- resolveImports opts (dropExtension filePath) moduleAst0
          case resolved of
            Left err -> pure (Left err)
            Right linked -> do
              case resolveTypeAliases linked of
                Left err -> pure (Left err)
                Right linked' -> do
                  let rootDir = takeDirectory filePath
                      rootPath = modulePathFromFile rootDir filePath
                  case lowerOverridesWith rootPath (overrideValuesText (overrideValues opts)) linked' of
                    Left err -> pure (Left err)
                    Right lowered -> do
                      case validateConstAssertsMerged opts rootPath lowered of
                        Left err -> pure (Left err)
                        Right () -> do
                          let iface = buildInterface (overrideSpecMode opts) lowered
                          case iface of
                            Left err -> pure (Left err)
                            Right iface' -> do
                              case emitSpirv opts lowered iface' of
                                Left err -> pure (Left err)
                                Right spirv -> pure (Right (SomeCompiledShader (CompiledShader spirv iface')))

compileWeslToSpirvFileWithDiagnostics :: CompileOptions -> FilePath -> IO (Either CompileError (SomeCompiledShader, [Diagnostic]))
compileWeslToSpirvFileWithDiagnostics opts path = do
  inputPath <- resolveInputPath path
  case inputPath of
    Left err -> pure (Left err)
    Right filePath -> do
      src <- readFile filePath
      case parseModuleWith (enabledFeatures opts) src of
        Left err -> pure (Left err)
        Right moduleAst0 -> do
          resolved <- resolveImports opts (dropExtension filePath) moduleAst0
          case resolved of
            Left err -> pure (Left err)
            Right linked -> do
              case resolveTypeAliases linked of
                Left err -> pure (Left err)
                Right linked' -> do
                  let rootDir = takeDirectory filePath
                      rootPath = modulePathFromFile rootDir filePath
                  case lowerOverridesWith rootPath (overrideValuesText (overrideValues opts)) linked' of
                    Left err -> pure (Left err)
                    Right lowered -> do
                      case collectDiagnosticsMerged opts rootPath lowered of
                        Left err -> pure (Left err)
                        Right diags -> do
                          let iface = buildInterface (overrideSpecMode opts) lowered
                          case iface of
                            Left err -> pure (Left err)
                            Right iface' -> do
                              case emitSpirv opts lowered iface' of
                                Left err -> pure (Left err)
                                Right spirv ->
                                  pure (Right (SomeCompiledShader (CompiledShader spirv iface'), diags))

compileWeslToSpirvBytes :: String -> Either CompileError ByteString
compileWeslToSpirvBytes src = compileWeslToSpirvBytesWith defaultCompileOptions src

compileWeslToSpirvBytesWith :: CompileOptions -> String -> Either CompileError ByteString
compileWeslToSpirvBytesWith opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  emitSpirv opts moduleAst iface

compileWeslToSpirvBytesWithDiagnostics :: CompileOptions -> String -> Either CompileError (ByteString, [Diagnostic])
compileWeslToSpirvBytesWithDiagnostics opts src = do
  moduleAst0 <- parseModuleWith (enabledFeatures opts) src
  moduleAst1 <- resolveTypeAliases moduleAst0
  moduleAst <- lowerOverridesWith [] (overrideValuesText (overrideValues opts)) moduleAst1
  when (not (null (modImports moduleAst))) $
    Left (CompileError "imports require file-based compilation" Nothing Nothing)
  let node = ModuleNode "<inline>" [] moduleAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let overrideIndex = buildOverrideIndex [node]
  validateModuleScopes opts False [] "" constIndex fnIndex structIndex overrideIndex [node]
  iface <- buildInterface (overrideSpecMode opts) moduleAst
  bytes <- emitSpirv opts moduleAst iface
  diags <- collectDiagnosticsMerged opts [] moduleAst
  pure (bytes, diags)

weslCacheVersion :: String
weslCacheVersion = "wesl-cache-v3"

weslCacheDir :: FilePath
weslCacheDir = "dist-newstyle" </> ".wesl-cache"

weslCacheKey :: CompileOptions -> String -> String
weslCacheKey opts src =
  let keySrc =
        intercalate
          "\n"
          [ weslCacheVersion
          , "v=" <> show (spirvVersion opts)
          , "features=" <> show (enabledFeatures opts)
          , "overrides=" <> show (overrideValues opts)
          , "spec=" <> show (overrideSpecMode opts)
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
loadWeslCache opts src = do
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
  let (spvPath, ifacePath) = weslCachePaths opts src
  in (do
        createDirectoryIfMissing True (takeDirectory spvPath)
        BS.writeFile spvPath bytes
        writeFile ifacePath (show iface)
     ) `catch` \(_ :: SomeException) -> pure ()

-- Quasiquoter

wesl :: QuasiQuoter
wesl =
  QuasiQuoter
    { quoteExp = weslExp
    , quotePat = const (fail "wesl: pattern context not supported")
    , quoteType = const (fail "wesl: type context not supported")
    , quoteDec = const (fail "wesl: declaration context not supported")
    }

weslExp :: String -> Q Exp
weslExp src = do
  let opts = defaultCompileOptions
  cached <- TH.runIO (loadWeslCache opts src)
  case cached of
    Just (bytes, iface) -> emitWeslExp bytes iface
    Nothing ->
      case compileWeslToSpirvWith opts src of
        Left err -> fail (renderError err)
        Right (SomeCompiledShader (CompiledShader bytes iface)) -> do
          TH.runIO (writeWeslCache opts src bytes iface)
          emitWeslExp bytes iface
  where
    emitWeslExp bytes iface = do
      bytesExp <- bytesToExp bytes
      ifaceExp <- interfaceToExp iface
      ifaceTy <- case interfaceToType iface of
        Left err -> fail ("wesl: " <> err)
        Right ty -> pure ty
      let shaderExp = TH.AppE (TH.AppE (TH.ConE 'CompiledShader) bytesExp) ifaceExp
      pure (TH.SigE shaderExp (TH.AppT (TH.ConT ''CompiledShader) ifaceTy))

-- Package metadata

data PackageInfo = PackageInfo
  { pkgName :: String
  , pkgVersion :: Maybe String
  , pkgRoot :: FilePath
  , pkgSourceRoot :: FilePath
  , pkgDependencies :: [PackageDependency]
  } deriving (Eq, Show)

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
                      in (section, mName, mVersion, mSource, Map.insert (depName dep) dep deps')
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

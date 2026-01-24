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

-- | Compiler pipeline and quasiquoter implementation.
module Spirdo.Wesl.Compiler where

import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
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

data CompileResult = CompileResult
  { crAst :: !ModuleAst
  , crInterface :: !ShaderInterface
  , crSpirv :: !ByteString
  , crDiagnostics :: ![Diagnostic]
  }

-- Public API

compileWeslToSpirv :: String -> Either CompileError SomeCompiledShader
compileWeslToSpirv = compileWeslToSpirvWith defaultCompileOptions

compileWeslToSpirvWith :: CompileOptions -> String -> Either CompileError SomeCompiledShader
compileWeslToSpirvWith opts src = do
  result <- compileInlineResult opts False src
  let shader = CompiledShader (crSpirv result) (crInterface result)
  pure (SomeCompiledShader shader)

compileWeslToSpirvWithDiagnostics :: CompileOptions -> String -> Either CompileError (SomeCompiledShader, [Diagnostic])
compileWeslToSpirvWithDiagnostics opts src = do
  result <- compileInlineResult opts True src
  let shader = CompiledShader (crSpirv result) (crInterface result)
  pure (SomeCompiledShader shader, crDiagnostics result)

compileWeslToSpirvFile :: FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFile = compileWeslToSpirvFileWith defaultCompileOptions

compileWeslToSpirvFileWith :: CompileOptions -> FilePath -> IO (Either CompileError SomeCompiledShader)
compileWeslToSpirvFileWith opts path = do
  result <- compileFileResult opts False path
  pure $ do
    cr <- result
    let shader = CompiledShader (crSpirv cr) (crInterface cr)
    pure (SomeCompiledShader shader)

compileWeslToSpirvFileWithDiagnostics :: CompileOptions -> FilePath -> IO (Either CompileError (SomeCompiledShader, [Diagnostic]))
compileWeslToSpirvFileWithDiagnostics opts path = do
  result <- compileFileResult opts True path
  pure $ do
    cr <- result
    let shader = CompiledShader (crSpirv cr) (crInterface cr)
    pure (SomeCompiledShader shader, crDiagnostics cr)

compileWeslToSpirvBytes :: String -> Either CompileError ByteString
compileWeslToSpirvBytes src = compileWeslToSpirvBytesWith defaultCompileOptions src

compileWeslToSpirvBytesWith :: CompileOptions -> String -> Either CompileError ByteString
compileWeslToSpirvBytesWith opts src = do
  result <- compileInlineResult opts False src
  pure (crSpirv result)

compileWeslToSpirvBytesWithDiagnostics :: CompileOptions -> String -> Either CompileError (ByteString, [Diagnostic])
compileWeslToSpirvBytesWithDiagnostics opts src = do
  result <- compileInlineResult opts True src
  pure (crSpirv result, crDiagnostics result)

compileInlineResult :: CompileOptions -> Bool -> String -> Either CompileError CompileResult
compileInlineResult opts wantDiagnostics src = do
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

compileFileResult :: CompileOptions -> Bool -> FilePath -> IO (Either CompileError CompileResult)
compileFileResult opts wantDiagnostics path =
  runExceptT $ do
    filePath <- ExceptT (resolveInputPath path)
    src <- liftIO (readFile filePath)
    moduleAst0 <- ExceptT (pure (parseModuleWith (enabledFeatures opts) src))
    -- resolveImports performs module linking and validateModuleScopes for file-based builds.
    linked <- ExceptT (resolveImports opts (dropExtension filePath) moduleAst0)
    linked' <- ExceptT (pure (resolveTypeAliases linked))
    let rootDir = takeDirectory filePath
        rootPath = modulePathFromFile rootDir filePath
    lowered <- ExceptT (pure (lowerOverridesWith rootPath (overrideValuesText (overrideValues opts)) linked'))
    diags <-
      if wantDiagnostics
        then ExceptT (pure (collectDiagnosticsMerged opts rootPath lowered))
        else do
          case validateConstAssertsMerged opts rootPath lowered of
            Left err -> throwE err
            Right () -> pure []
    iface <- ExceptT (pure (buildInterface (overrideSpecMode opts) lowered))
    spirv <- ExceptT (pure (emitSpirv opts lowered iface))
    pure CompileResult
      { crAst = lowered
      , crInterface = iface
      , crSpirv = spirv
      , crDiagnostics = diags
      }

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

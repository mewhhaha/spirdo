{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Cache and file-boundary regressions through public compiler APIs.
module CompilerCacheRegression (checks) where

import Control.Exception (IOException, bracket, bracketOnError, catch)
import Control.Monad (unless, when)
import Data.Bits (shiftR, xor)
import qualified Data.ByteString as BS
import Data.List (findIndex, isInfixOf, isPrefixOf, tails)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import qualified Language.Haskell.TH as TH
import Spirdo.Wesl.Reflection
  ( CompileError(..)
  , SomeShader(..)
  , compileFile
  , defaultCompileOptions
  , imports
  , module_
  , spirv
  , withCacheDir
  , withFeatures
  , (<:)
  )
import System.Directory
  ( createDirectory
  , doesFileExist
  , doesDirectoryExist
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>), takeExtension)
import System.IO (Handle, hClose, openTempFile)

$(do
    let hitCacheDirectory = "dist-newstyle/.wesl-cache-hit-regression"
        defaultlessWorkgroupCacheDirectory = "dist-newstyle/.wesl-cache-defaultless-workgroup-regression"
        corruptionCacheDirectory = "dist-newstyle/.wesl-cache-corruption-regression"
        identityCacheDirectory = "dist-newstyle/.wesl-cache-identity-regression"
        unicodeCacheDirectory = "dist-newstyle/.wesl-cache-unicode-regression"
        duplicateImportCacheDirectory = "dist-newstyle/.wesl-cache-duplicate-import-regression"
        normalizedImportCacheDirectory = "dist-newstyle/.wesl-cache-normalized-import-regression"
        layoutCacheDirectory = "dist-newstyle/.wesl-cache-layout-regression"
        immediateCacheDirectory = "dist-newstyle/.wesl-cache-immediate-regression"
        cacheDirectories =
          [ hitCacheDirectory
          , defaultlessWorkgroupCacheDirectory
          , corruptionCacheDirectory
          , identityCacheDirectory
          , unicodeCacheDirectory
          , duplicateImportCacheDirectory
          , normalizedImportCacheDirectory
          , layoutCacheDirectory
          , immediateCacheDirectory
          ]
        fragmentSource value =
          unlines
            [ "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4<f32>(" <> value <> ");"
            , "}"
            ]
        cacheMagic = BS.pack [0x53, 0x50, 0x57, 0x53, 0x43, 0x30, 0x34, 0x0a]
        cacheHeaderBytes = BS.length cacheMagic + 4 + 8
        checksumOffset = 14695981039346656037 :: Word64
        checksumPrime = 1099511628211 :: Word64
        checksumStep acc byte = (acc `xor` fromIntegral byte) * checksumPrime
        checksum = BS.foldl' checksumStep checksumOffset
        word32LE word =
          BS.pack
            [ fromIntegral (word `shiftR` bit)
            | bit <- [0, 8 .. 24]
            ]
        word64LE word =
          BS.pack
            [ fromIntegral (word `shiftR` bit)
            | bit <- [0, 8 .. 56]
            ]
        word32At bytes offset =
          foldl'
            (\acc bit -> acc + fromIntegral (BS.index bytes (offset + bit `div` 8)) * 2 ^ bit)
            0
            [0, 8 .. 24]
            :: Word32
        word64At bytes offset =
          foldl'
            (\acc bit -> acc + fromIntegral (BS.index bytes (offset + bit `div` 8)) * 2 ^ bit)
            0
            [0, 8 .. 56]
            :: Word64
        framePayload payload =
          cacheMagic
            <> word32LE (fromIntegral (BS.length payload) :: Word32)
            <> word64LE (checksum payload)
            <> payload
        readPayload cachePath = do
          artifact <- BS.readFile cachePath
          unless (BS.length artifact >= cacheHeaderBytes) $
            fail ("cache artifact is shorter than its frame: " <> cachePath)
          unless (BS.take (BS.length cacheMagic) artifact == cacheMagic) $
            fail ("cache artifact has the wrong frame magic: " <> cachePath)
          let declaredBytes = word32At artifact (BS.length cacheMagic)
              storedChecksum = word64At artifact (BS.length cacheMagic + 4)
              payload = BS.drop cacheHeaderBytes artifact
          unless (toInteger declaredBytes == toInteger (BS.length payload)) $
            fail ("cache artifact has the wrong framed length: " <> cachePath)
          unless (storedChecksum == checksum payload) $
            fail ("cache artifact has the wrong payload checksum: " <> cachePath)
          pure payload
        cacheArtifacts cacheDirectory = do
          exists <- doesDirectoryExist cacheDirectory
          unless exists $
            fail ("cache directory was not created: " <> cacheDirectory)
          files <- listDirectory cacheDirectory
          pure
            [ cacheDirectory </> file
            | file <- files
            , takeExtension file == ".cache"
            ]
        onlyCacheArtifact cacheDirectory = do
          artifacts <- cacheArtifacts cacheDirectory
          case artifacts of
            [cachePath] -> pure cachePath
            _ ->
              fail
                ( "expected exactly one cache artifact in "
                    <> cacheDirectory
                    <> ", found "
                    <> show (length artifacts)
                )
        resetCacheDirectory cacheDirectory = do
          exists <- doesDirectoryExist cacheDirectory
          when exists (removeDirectoryRecursive cacheDirectory)
        assertCacheHit cachePath sentinelArtifact = do
          actualArtifact <- BS.readFile cachePath
          unless (actualArtifact == sentinelArtifact) $
            fail ("unchanged warm compilation rewrote the hit sentinel: " <> cachePath)
        assertCacheMissRewrote cachePath corruptedArtifact = do
          actualArtifact <- BS.readFile cachePath
          unless (actualArtifact /= corruptedArtifact) $
            fail ("invalid cache artifact was reused: " <> cachePath)
          _ <- readPayload cachePath
          pure ()
        splitAtMarker marker contents = do
          markerOffset <- findIndex (isPrefixOf marker) (tails contents)
          pure
            ( take markerOffset contents
            , drop (markerOffset + length marker) contents
            )
        replaceBetween start end replacement payload = do
          text <- either (const Nothing) (Just . T.unpack) (TE.decodeUtf8' payload)
          (beforeStart, afterStart) <- splitAtMarker start text
          (_, afterEnd) <- splitAtMarker end afterStart
          pure (TE.encodeUtf8 (T.pack (beforeStart <> start <> replacement <> end <> afterEnd)))
        mutateFramedPayload cachePath modifyPayload = do
          artifactBefore <- BS.readFile cachePath
          payloadBefore <- readPayload cachePath
          payloadAfter <-
            case modifyPayload payloadBefore of
              Nothing -> fail ("could not mutate cache payload: " <> cachePath)
              Just payload -> pure payload
          let artifactAfter = framePayload payloadAfter
          when (artifactAfter == artifactBefore) $
            fail ("cache mutation did not change the artifact: " <> cachePath)
          BS.writeFile cachePath artifactAfter
          pure artifactAfter
        mutateRawArtifact cachePath modifyArtifact = do
          artifactBefore <- BS.readFile cachePath
          let artifactAfter = modifyArtifact artifactBefore
          when (artifactAfter == artifactBefore) $
            fail ("cache mutation did not change the artifact: " <> cachePath)
          BS.writeFile cachePath artifactAfter
          pure artifactAfter
        runMissRegression opts src cachePath mutateArtifact = do
          corruptedArtifact <- TH.runIO (mutateArtifact cachePath)
          _ <- spirv opts imports src
          TH.runIO (assertCacheMissRewrote cachePath corruptedArtifact)

    TH.runIO (mapM_ resetCacheDirectory cacheDirectories)

    let hitOptions = withCacheDir hitCacheDirectory defaultCompileOptions
        hitSource = fragmentSource "0.125"
    _ <- spirv hitOptions imports hitSource
    hitArtifact <- TH.runIO (onlyCacheArtifact hitCacheDirectory)
    hitSentinel <-
      TH.runIO
        (mutateFramedPayload hitArtifact (Just . BS.cons 0x20))
    _ <- spirv hitOptions imports hitSource
    TH.runIO (assertCacheHit hitArtifact hitSentinel)

    let defaultlessWorkgroupOptions = withCacheDir defaultlessWorkgroupCacheDirectory defaultCompileOptions
        defaultlessWorkgroupSource =
          unlines
            [ "@id(7) override x: u32;"
            , "@compute @workgroup_size(x) fn main() {}"
            ]
    _ <- spirv defaultlessWorkgroupOptions imports defaultlessWorkgroupSource
    defaultlessWorkgroupArtifact <- TH.runIO (onlyCacheArtifact defaultlessWorkgroupCacheDirectory)
    defaultlessWorkgroupSentinel <-
      TH.runIO
        (mutateFramedPayload defaultlessWorkgroupArtifact (Just . BS.cons 0x20))
    _ <- spirv defaultlessWorkgroupOptions imports defaultlessWorkgroupSource
    TH.runIO (assertCacheHit defaultlessWorkgroupArtifact defaultlessWorkgroupSentinel)

    let corruptionOptions = withCacheDir corruptionCacheDirectory defaultCompileOptions
        corruptionSource = fragmentSource "0.25"
    _ <- spirv corruptionOptions imports corruptionSource
    corruptionArtifact <- TH.runIO (onlyCacheArtifact corruptionCacheDirectory)
    runMissRegression
      corruptionOptions
      corruptionSource
      corruptionArtifact
      (\cachePath ->
          mutateRawArtifact cachePath $ \artifact ->
            case BS.unsnoc artifact of
              Nothing -> artifact
              Just (prefix, finalByte) -> prefix <> BS.singleton (finalByte `xor` 1)
      )

    let fakeSpirv =
          BS.concat
            ( map
                word32LE
                ([ 0x07230203
                , 0x00010600
                , 0
                , 2
                , 0
                , 0x00010000
                ] :: [Word32])
            )
        fakeSpirvShown = show fakeSpirv
        fakeSpirvContents = take (length fakeSpirvShown - 2) (drop 1 fakeSpirvShown)
    runMissRegression
      corruptionOptions
      corruptionSource
      corruptionArtifact
      (\cachePath ->
          mutateFramedPayload
            cachePath
            (replaceBetween "wceSpirv = \"" "\", wceInterface =" fakeSpirvContents)
      )

    runMissRegression
      corruptionOptions
      corruptionSource
      corruptionArtifact
      (\cachePath ->
          mutateFramedPayload
            cachePath
            (const (Just (BS.replicate 193 0x28 <> BS.replicate 193 0x29)))
      )

    runMissRegression
      corruptionOptions
      corruptionSource
      corruptionArtifact
      (\cachePath ->
          mutateFramedPayload cachePath (const (Just (BS.replicate 500001 0x2c)))
      )

    let identityOptions = withCacheDir identityCacheDirectory defaultCompileOptions
        identitySource = fragmentSource "0.5"
    _ <- spirv identityOptions imports identitySource
    identityArtifact <- TH.runIO (onlyCacheArtifact identityCacheDirectory)
    runMissRegression
      identityOptions
      identitySource
      identityArtifact
      (\cachePath ->
          mutateFramedPayload
            cachePath
            ( replaceBetween
                "wceIdentity = \""
                "\", wceSpirv ="
                "different-cache-input"
            )
      )

    let unicodeOptions = withCacheDir unicodeCacheDirectory defaultCompileOptions
        unicodeSourceA =
          unlines
            [ "const é: f32 = 0.0;"
            , "const ǩ: f32 = 1.0;"
            , fragmentSource "é"
            ]
        unicodeSourceB =
          unlines
            [ "const é: f32 = 0.0;"
            , "const ǩ: f32 = 1.0;"
            , fragmentSource "ǩ"
            ]
    _ <- spirv unicodeOptions imports unicodeSourceA
    _ <- spirv unicodeOptions imports unicodeSourceB
    unicodeArtifacts <- TH.runIO (cacheArtifacts unicodeCacheDirectory)
    unless (length unicodeArtifacts == 2) $
      fail "distinct Unicode source inputs did not create distinct cache artifacts"

    let duplicateOptions = withCacheDir duplicateImportCacheDirectory defaultCompileOptions
        importedSource = "fn importedValue() -> f32 { return 0.75; }"
        replacedSource = "fn importedValue() -> f32 { return 0.25; }"
        rootSource =
          unlines
            [ "import cache_module;"
            , "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4<f32>(cache_module::importedValue());"
            , "}"
            ]
        validImports = imports <: module_ @"cache_module" importedSource
        duplicateImports =
          imports
            <: module_ @"cache_module" replacedSource
            <: module_ @"cache_module" importedSource
    _ <- spirv duplicateOptions validImports rootSource
    _ <- TH.runIO (onlyCacheArtifact duplicateImportCacheDirectory)
    duplicateRejected <-
      TH.recover
        (pure True)
        (do
            _ <- spirv duplicateOptions duplicateImports rootSource
            pure False
        )
    unless duplicateRejected $
      fail "duplicate typed imports reused the map-collapsed warm cache artifact"
    duplicateArtifacts <- TH.runIO (cacheArtifacts duplicateImportCacheDirectory)
    unless (length duplicateArtifacts == 1) $
      fail "duplicate typed imports created an additional cache artifact"

    let normalizedOptions = withCacheDir normalizedImportCacheDirectory defaultCompileOptions
        normalizedCollisionImports =
          imports
            <: module_ @"foo" importedSource
            <: module_ @"./foo" replacedSource
        normalizedRootSource =
          unlines
            [ "import foo;"
            , "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4<f32>(foo::importedValue());"
            , "}"
            ]
    normalizedCollisionRejected <-
      TH.recover
        (pure True)
        (do
            _ <- spirv normalizedOptions normalizedCollisionImports normalizedRootSource
            pure False
        )
    unless normalizedCollisionRejected $
      fail "raw import keys that collide after normalization compiled successfully"
    normalizedArtifacts <-
      TH.runIO $ do
        exists <- doesDirectoryExist normalizedImportCacheDirectory
        if not exists
          then pure []
          else do
            files <- listDirectory normalizedImportCacheDirectory
            pure [file | file <- files, takeExtension file == ".cache"]
    unless (null normalizedArtifacts) $
      fail "a rejected normalized import collision wrote a cache artifact"

    let layoutOptions =
          withFeatures
            ["uniform_buffer_standard_layout"]
            (withCacheDir layoutCacheDirectory defaultCompileOptions)
        layoutSource =
          unlines
            [ "enable uniform_buffer_standard_layout;"
            , "struct Params {"
            , "  @size(20) direction: vec3<f32>,"
            , "}"
            , "struct Values {"
            , "  count: u32,"
            , "  values: array<u32>,"
            , "}"
            , "@group(0) @binding(0) var<uniform> params: Params;"
            , "@group(0) @binding(1) var<storage, read> values: Values;"
            , "@fragment"
            , "fn main() -> @location(0) vec4<f32> {"
            , "  return vec4<f32>(params.direction, f32(values.count));"
            , "}"
            ]
    _ <- spirv layoutOptions imports layoutSource
    layoutArtifact <- TH.runIO (onlyCacheArtifact layoutCacheDirectory)
    layoutSentinel <-
      TH.runIO
        (mutateFramedPayload layoutArtifact (Just . BS.cons 0x20))
    _ <- spirv layoutOptions imports layoutSource
    TH.runIO (assertCacheHit layoutArtifact layoutSentinel)

    let immediateOptions = withCacheDir immediateCacheDirectory defaultCompileOptions
        immediateSource =
          unlines
            [ "var<immediate> constants: u32;"
            , "@compute @workgroup_size(1)"
            , "fn main() { let selected = constants; }"
            ]
    _ <- spirv immediateOptions imports immediateSource
    immediateArtifact <- TH.runIO (onlyCacheArtifact immediateCacheDirectory)
    immediateSentinel <-
      TH.runIO
        (mutateFramedPayload immediateArtifact (Just . BS.cons 0x20))
    _ <- spirv immediateOptions imports immediateSource
    TH.runIO (assertCacheHit immediateArtifact immediateSentinel)
    runMissRegression
      immediateOptions
      immediateSource
      immediateArtifact
      (\cachePath ->
          mutateFramedPayload
            cachePath
            (replaceBetween "siPushConstants = Just " ", siSamplerMode =" "TLSampler")
      )

    [d|
      cacheCompileTimeRegressionsPassed :: Bool
      cacheCompileTimeRegressionsPassed = True
      |]
 )

checkCompilerCacheHardening :: IO ()
checkCompilerCacheHardening =
  unless cacheCompileTimeRegressionsPassed $
    fail "compiler cache compile-time regressions did not run"

checkMissingImport :: IO ()
checkMissingImport =
  withScratchDirectory "missing-import" $ \rootDir -> do
    let rootFile = rootDir </> "main.wesl"
    writeFile rootFile (unlines ["import absent;", fragmentShader])
    result <- compileFile rootFile
    case result of
      Left err
        | "import module not found" `isInfixOf` err.ceMessage -> pure ()
        | otherwise -> fail ("missing import returned the wrong error: " <> show err)
      Right (SomeShader _) -> fail "missing import compiled successfully"

checks :: [(String, IO ())]
checks =
  [ ("compiler cache hardening regressions", checkCompilerCacheHardening)
  , ("file compilation reports missing imports", checkMissingImport)
  ]

fragmentShader :: String
fragmentShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(0.0);"
    , "}"
    ]

withScratchDirectory :: String -> (FilePath -> IO a) -> IO a
withScratchDirectory label = bracket (createScratchDirectory label) removeDirectoryRecursive

createScratchDirectory :: String -> IO FilePath
createScratchDirectory label = do
  temporaryDirectory <- getTemporaryDirectory
  bracketOnError
    (openTempFile temporaryDirectory ("spirdo-" <> label))
    cleanupTemporaryFile
    $ \(temporaryFile, handle) -> do
        hClose handle
        removeFile temporaryFile
        createDirectory temporaryFile
        pure temporaryFile

cleanupTemporaryFile :: (FilePath, Handle) -> IO ()
cleanupTemporaryFile (path, handle) = do
  hClose handle `catch` ignoreIOException
  exists <- doesFileExist path
  when exists (removeFile path)

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()

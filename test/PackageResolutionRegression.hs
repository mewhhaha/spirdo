-- | Package discovery and import-containment regressions through the public
-- file compiler API.
module PackageResolutionRegression (checks) where

import Control.Exception (IOException, bracket, bracketOnError, catch)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (isInfixOf, sort)
import Spirdo.Wesl.Reflection
  ( BindingInfo(..)
  , CompileError(..)
  , FieldLayout(..)
  , Option(..)
  , OverrideInfo(..)
  , OverrideValue(..)
  , Scalar(..)
  , ShaderInterface(..)
  , SomeShader(..)
  , TypeLayout(..)
  , bindingInfoFor
  , compileFile
  , compileFileWith
  , shaderInterface
  , shaderSpirv
  )
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , createDirectoryLink
  , createFileLink
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, hClose, openTempFile)

checks :: [(String, IO ())]
checks =
  [ ("package resolution uses the default shaders root", checkDefaultRoot)
  , ("package resolution finds nested entries and package imports", checkPackageImport)
  , ("package resolution follows path dependencies into their source roots", checkPathDependency)
  , ("package resolution reports unknown dependencies", checkUnknownDependency)
  , ("package resolution rejects file symlinks outside the package", checkOutsideFileSymlink)
  , ("package resolution rejects directory symlinks outside the package", checkOutsideDirectorySymlink)
  , ("package resolution preserves relative imports without a manifest", checkRelativeImportWithoutManifest)
  , ("package resolution rejects malformed manifests", checkMalformedManifest)
  , ("package resolution rejects keyword dependency aliases", checkKeywordDependencyAlias)
  , ("package resolution rejects duplicate manifest keys", checkDuplicateManifestKey)
  , ("package resolution rejects drive-relative manifest paths", checkDriveRelativeManifestPaths)
  , ("package resolution accepts cyclic module imports", checkCyclicImports)
  , ("file compilation bounds entry source bytes", checkEntrySourceByteLimit)
  , ("file compilation rejects invalid UTF-8 entry source", checkInvalidEntryUtf8)
  , ("package resolution rejects invalid UTF-8 manifests", checkInvalidManifestUtf8)
  , ("package resolution accepts cyclic path dependencies", checkCyclicPathDependencies)
  , ("package resolution rejects colliding canonical source roots", checkPackageSourceRootCollision)
  , ("package resolution preserves identities for overlapping package roots", checkOverlappingPackageRootIdentities)
  , ("package resolution bounds dependency depth", checkPackageDependencyDepthLimit)
  , ("package resolution bounds depth through a preloaded sibling", checkPreloadedPackageDependencyDepthLimit)
  , ("package resolution bounds descendants of a preloaded dependency", checkPreloadedPackageDescendantDepthLimit)
  , ("package resolution bounds package count", checkPackageCountLimit)
  , ("package resolution bounds cumulative manifest bytes", checkPackageManifestByteLimit)
  , ("package resolution gives dependency resources stable source-qualified names", checkStableDependencyResourceNames)
  , ("package resolution rejects private encoded override keys", checkPrivateOverrideKeyRejection)
  , ("package resolution keeps old-mangling module paths distinct", checkMangledModulePathCollision)
  , ("package resolution reserves a boundary between dependency packages and modules", checkDependencyPackageModuleBoundary)
  , ("package resolution distinguishes current modules from dependency aliases", checkCurrentModuleDependencyAliasBoundary)
  , ("package resolution specializes overrides by their reflected canonical names", checkCanonicalOverrideKeyRoundTrip)
  , ("package resolution reports canonical candidates for ambiguous shorthand", checkAmbiguousOverrideShorthand)
  ]

checkDefaultRoot :: IO ()
checkDefaultRoot =
  withScratchDirectory "package-default-root" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\""]
    writeSource entry fragmentShader
    expectCompile entry

checkPackageImport :: IO ()
checkPackageImport =
  withScratchDirectory "package-import" $ \workspace -> do
    let packageDir = workspace </> "app"
        shaderDir = packageDir </> "shaders"
        entry = shaderDir </> "nested" </> "main.wesl"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\"", "root = \"./shaders\""]
    writeSource (shaderDir </> "common.wesl") "fn value() -> f32 { return 0.5; }"
    writeSource entry (fragmentUsing "import package::common as class;" "class::value()")
    expectCompile entry

checkPathDependency :: IO ()
checkPathDependency =
  withScratchDirectory "package-path-dependency" $ \workspace -> do
    let appDir = workspace </> "app"
        depDir = workspace </> "dep"
        entry = appDir </> "shaders" </> "main.wesl"
    writeManifest
      appDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"./shaders\""
      , "[dependencies]"
      , "math = { path = \"../dep\" }"
      ]
    writeManifest depDir ["name = \"math\"", "edition = \"2026_pre\"", "root = \"./library\""]
    writeSource (depDir </> "library" </> "constants.wesl") "fn amount() -> f32 { return 0.75; }"
    writeSource
      (depDir </> "library" </> "colors.wesl")
      "import package::constants;\nfn value() -> f32 { return constants::amount(); }"
    writeSource entry (fragmentUsing "import math::colors;" "colors::value()")
    expectCompile entry

checkUnknownDependency :: IO ()
checkUnknownDependency =
  withScratchDirectory "package-unknown-dependency" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\""]
    writeSource entry (fragmentUsing "import missing::colors;" "colors::value()")
    expectFailureContaining entry ["unknown package dependency: missing"]

checkOutsideFileSymlink :: IO ()
checkOutsideFileSymlink =
  withScratchDirectory "package-file-symlink" $ \workspace -> do
    let packageDir = workspace </> "app"
        shaderDir = packageDir </> "shaders"
        entry = shaderDir </> "main.wesl"
        outside = workspace </> "outside.wesl"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\""]
    writeSource outside "fn value() -> f32 { return 1.0; }"
    createDirectoryIfMissing True shaderDir
    createFileLink outside (shaderDir </> "outside.wesl")
    writeSource entry (fragmentUsing "import outside;" "outside::value()")
    expectFailureContaining entry ["import path escapes package root", "resolves to", "outside"]

checkOutsideDirectorySymlink :: IO ()
checkOutsideDirectorySymlink =
  withScratchDirectory "package-directory-symlink" $ \workspace -> do
    let packageDir = workspace </> "app"
        shaderDir = packageDir </> "shaders"
        entry = shaderDir </> "main.wesl"
        outsideDir = workspace </> "outside"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\""]
    writeSource (outsideDir </> "library.wesl") "fn value() -> f32 { return 1.0; }"
    createDirectoryIfMissing True shaderDir
    createDirectoryLink outsideDir (shaderDir </> "linked")
    writeSource entry (fragmentUsing "import linked::library;" "library::value()")
    expectFailureContaining entry ["import path escapes package root", "resolves to", "outside"]

checkRelativeImportWithoutManifest :: IO ()
checkRelativeImportWithoutManifest =
  withScratchDirectory "package-no-manifest" $ \workspace -> do
    let entry = workspace </> "main.wesl"
    writeSource (workspace </> "library.wesl") "fn value() -> f32 { return 0.25; }"
    writeSource entry (fragmentUsing "import library;" "library::value()")
    expectCompile entry

checkMalformedManifest :: IO ()
checkMalformedManifest =
  withScratchDirectory "package-malformed-manifest" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifestText packageDir "[package\nname = \"app\"\n"
    writeSource entry fragmentShader
    expectFailureContaining entry ["wesl.toml:1: malformed manifest section header"]

checkKeywordDependencyAlias :: IO ()
checkKeywordDependencyAlias =
  withScratchDirectory "package-keyword-dependency" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "[dependencies]"
      , "package = { path = \"../dependency\" }"
      ]
    writeSource entry fragmentShader
    expectFailureContaining entry ["invalid dependency name: package"]

checkDuplicateManifestKey :: IO ()
checkDuplicateManifestKey =
  withScratchDirectory "package-duplicate-manifest" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"./shaders\""
      , "root = \"./other-shaders\""
      ]
    writeSource entry fragmentShader
    expectFailureContaining entry ["duplicate [package] field root"]

checkDriveRelativeManifestPaths :: IO ()
checkDriveRelativeManifestPaths =
  withScratchDirectory "package-drive-relative-paths" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"C:outside\""
      ]
    writeSource entry fragmentShader
    expectFailureContaining entry ["package root must be relative: C:outside"]

    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"/outside\""
      ]
    expectFailureContaining entry ["package root must be relative: /outside"]

    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"\\\\outside\""
      ]
    expectFailureContaining entry ["package root must be relative: \\outside"]

    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"c:/outside\""
      ]
    expectFailureContaining entry ["package root must be relative: c:/outside"]

    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \"D:\\\\outside\""
      ]
    expectFailureContaining entry ["package root must be relative: D:\\outside"]

    writeManifest
      packageDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "[dependencies]"
      , "outside = { path = \"C:outside\" }"
      ]
    expectFailureContaining entry ["dependency path must be relative: C:outside"]

checkCyclicImports :: IO ()
checkCyclicImports =
  withScratchDirectory "package-cyclic-imports" $ \workspace -> do
    let entry = workspace </> "main.wesl"
    writeSource (workspace </> "first.wesl") "import second;\nfn value() -> f32 { return 0.5; }"
    writeSource (workspace </> "second.wesl") "import first;\nfn other() -> f32 { return 1.0; }"
    writeSource entry (fragmentUsing "import first;" "first::value()")
    expectCompile entry

checkEntrySourceByteLimit :: IO ()
checkEntrySourceByteLimit =
  withScratchDirectory "entry-source-byte-limit" $ \workspace -> do
    let entry = workspace </> "main.wesl"
    writeSourceBytes entry (BS.replicate (4 * 1024 * 1024 + 1) 32)
    expectFailureContaining entry ["input file exceeds UTF-8 byte limit", entry, "limit 4194304"]

checkInvalidEntryUtf8 :: IO ()
checkInvalidEntryUtf8 =
  withScratchDirectory "entry-source-invalid-utf8" $ \workspace -> do
    let entry = workspace </> "main.wesl"
    writeSourceBytes entry (BS.pack [0xff])
    expectFailureContaining entry ["invalid UTF-8 in input file", entry]

checkInvalidManifestUtf8 :: IO ()
checkInvalidManifestUtf8 =
  withScratchDirectory "manifest-invalid-utf8" $ \workspace -> do
    let packageDir = workspace </> "app"
        entry = packageDir </> "shaders" </> "main.wesl"
    writeSourceBytes (packageDir </> "wesl.toml") (BS.pack [0xff])
    writeSource entry fragmentShader
    expectFailureContaining entry ["invalid UTF-8 in WESL manifest", packageDir </> "wesl.toml"]

checkCyclicPathDependencies :: IO ()
checkCyclicPathDependencies =
  withScratchDirectory "package-dependency-cycle" $ \workspace -> do
    let appDir = workspace </> "app"
        libraryDir = workspace </> "library"
        entry = appDir </> "shaders" </> "main.wesl"
    writeManifest appDir (packageWithPathDependency "app" "library" "../library")
    writeManifest libraryDir (packageWithPathDependency "library" "app" "../app")
    writeSource entry fragmentShader
    writeSource (libraryDir </> "shaders" </> "library.wesl") "fn value() -> f32 { return 1.0; }"
    expectCompile entry

checkPackageSourceRootCollision :: IO ()
checkPackageSourceRootCollision =
  withScratchDirectory "package-source-root-collision" $ \workspace -> do
    let appDir = workspace </> "app"
        firstDir = workspace </> "first"
        secondDir = workspace </> "second"
        sharedRoot = workspace </> "shared"
        entry = appDir </> "shaders" </> "main.wesl"
    writeManifest
      appDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "[dependencies]"
      , "first_alias = { path = \"../first\" }"
      , "second_alias = { path = \"../second\" }"
      ]
    createDirectoryIfMissing True sharedRoot
    writeManifest firstDir ["name = \"first\"", "edition = \"2026_pre\"", "root = \"../shared\""]
    writeManifest secondDir ["name = \"second\"", "edition = \"2026_pre\"", "root = \"../shared\""]
    writeSource entry fragmentShader
    expectFailureContaining
      entry
      [ "package source root collision"
      , sharedRoot
      , firstDir </> "wesl.toml"
      , secondDir </> "wesl.toml"
      ]

checkOverlappingPackageRootIdentities :: IO ()
checkOverlappingPackageRootIdentities =
  withScratchDirectory "package-overlapping-roots" $ \workspace -> do
    let appDir = workspace </> "app"
        dependencyDir = appDir </> "dep"
        entry = appDir </> "main.wesl"
    writeManifest
      appDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "root = \".\""
      , "[dependencies]"
      , "dep = { path = \"./dep\" }"
      ]
    writeManifest dependencyDir ["name = \"dep\"", "edition = \"2026_pre\"", "root = \".\""]
    writeSource (dependencyDir </> "foo.wesl") "fn value() -> f32 { return 0.5; }"
    writeSource
      entry
      ( fragmentUsing
          (unlines ["import package::dep::foo as localFoo;", "import dep::foo as dependencyFoo;"])
          "localFoo::value() + dependencyFoo::value()"
      )
    expectCompile entry

checkPackageDependencyDepthLimit :: IO ()
checkPackageDependencyDepthLimit =
  withScratchDirectory "package-dependency-depth" $ \workspace -> do
    let packageDirs = [workspace </> ("package-" <> show index) | index <- [1 :: Int .. 65]]
        entry = workspace </> "package-1" </> "shaders" </> "main.wesl"
    forM_ (zip packageDirs [1 :: Int ..]) $ \(packageDir, index) -> do
      createDirectoryIfMissing True (packageDir </> "shaders")
      if index < 65
        then writeManifest packageDir (packageWithPathDependency ("package-" <> show index) "next" ("../package-" <> show (index + 1)))
        else writeManifest packageDir ["name = \"package-65\"", "edition = \"2026_pre\""]
    writeSource entry fragmentShader
    expectFailureContaining entry ["package dependency depth 65 exceeds limit 64", workspace </> "package-65" </> "wesl.toml"]

checkPreloadedPackageDependencyDepthLimit :: IO ()
checkPreloadedPackageDependencyDepthLimit =
  withScratchDirectory "package-preloaded-dependency-depth" $ \workspace -> do
    let appDir = workspace </> "app"
        chainDirs = [workspace </> ("chain-" <> show index) | index <- [1 :: Int .. 63]]
        targetDir = workspace </> "target"
        entry = appDir </> "shaders" </> "main.wesl"
    writeManifest
      appDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "[dependencies]"
      , "chain = { path = \"../chain-1\" }"
      , "target = { path = \"../target\" }"
      ]
    forM_ (zip chainDirs [1 :: Int ..]) $ \(chainDir, index) -> do
      createDirectoryIfMissing True (chainDir </> "shaders")
      let nextPath =
            if index < 63
              then "../chain-" <> show (index + 1)
              else "../target"
      writeManifest chainDir (packageWithPathDependency ("chain-" <> show index) "next" nextPath)
    createDirectoryIfMissing True (targetDir </> "shaders")
    writeManifest targetDir ["name = \"target\"", "edition = \"2026_pre\""]
    writeSource entry fragmentShader
    expectFailureContaining
      entry
      [ "package dependency depth 65 exceeds limit 64"
      , targetDir </> "wesl.toml"
      ]

checkPreloadedPackageDescendantDepthLimit :: IO ()
checkPreloadedPackageDescendantDepthLimit =
  withScratchDirectory "package-preloaded-descendant-depth" $ \workspace -> do
    let appDir = workspace </> "app"
        chainDirs = [workspace </> ("chain-" <> show index) | index <- [1 :: Int .. 62]]
        packageX = workspace </> "x"
        packageY = workspace </> "y"
        entry = appDir </> "shaders" </> "main.wesl"
    writeManifest
      appDir
      [ "name = \"app\""
      , "edition = \"2026_pre\""
      , "[dependencies]"
      , "a_short = { path = \"../x\" }"
      , "z_chain = { path = \"../chain-1\" }"
      ]
    forM_ (zip chainDirs [1 :: Int ..]) $ \(chainDir, index) -> do
      createDirectoryIfMissing True (chainDir </> "shaders")
      let nextPath =
            if index < 62
              then "../chain-" <> show (index + 1)
              else "../x"
      writeManifest chainDir (packageWithPathDependency ("chain-" <> show index) "next" nextPath)
    createDirectoryIfMissing True (packageX </> "shaders")
    createDirectoryIfMissing True (packageY </> "shaders")
    writeManifest packageX (packageWithPathDependency "x" "y" "../y")
    writeManifest packageY ["name = \"y\"", "edition = \"2026_pre\""]
    writeSource entry fragmentShader
    expectFailureContaining
      entry
      [ "package dependency depth 65 exceeds limit 64"
      , packageY </> "wesl.toml"
      ]

checkPackageCountLimit :: IO ()
checkPackageCountLimit =
  withScratchDirectory "package-count" $ \workspace -> do
    let appDir = workspace </> "app"
        dependencyDirs = [workspace </> ("dependency-" <> show index) | index <- [1 :: Int .. 256]]
        entry = appDir </> "shaders" </> "main.wesl"
        dependencyLines =
          [ "dependency" <> show index <> " = { path = \"../dependency-" <> show index <> "\" }"
          | index <- [1 :: Int .. 256]
          ]
    writeManifest appDir (["name = \"app\"", "edition = \"2026_pre\"", "[dependencies]"] <> dependencyLines)
    forM_ (zip dependencyDirs [1 :: Int ..]) $ \(dependencyDir, index) -> do
      createDirectoryIfMissing True (dependencyDir </> "shaders")
      writeManifest dependencyDir ["name = \"dependency-" <> show index <> "\"", "edition = \"2026_pre\""]
    writeSource entry fragmentShader
    expectFailureContaining entry ["package count 257 exceeds limit 256", "wesl.toml"]

checkPackageManifestByteLimit :: IO ()
checkPackageManifestByteLimit =
  withScratchDirectory "package-manifest-byte-limit" $ \workspace -> do
    let appDir = workspace </> "app"
        dependencyDirs = [workspace </> ("dependency-" <> show index) | index <- [1 :: Int .. 5]]
        entry = appDir </> "shaders" </> "main.wesl"
        dependencyLines =
          [ "dependency" <> show index <> " = { path = \"../dependency-" <> show index <> "\" }"
          | index <- [1 :: Int .. 5]
          ]
    writeManifest appDir (["name = \"app\"", "edition = \"2026_pre\"", "[dependencies]"] <> dependencyLines)
    forM_ (zip dependencyDirs [1 :: Int ..]) $ \(dependencyDir, index) -> do
      createDirectoryIfMissing True (dependencyDir </> "shaders")
      writeManifestText
        dependencyDir
        ( unlines ["[package]", "name = \"dependency-" <> show index <> "\"", "edition = \"2026_pre\""]
            <> "#" <> replicate 220000 'x' <> "\n"
        )
    writeSource entry fragmentShader
    expectFailureContaining entry ["cumulative package manifest bytes", "exceeds limit 1048576", "wesl.toml"]

checkStableDependencyResourceNames :: IO ()
checkStableDependencyResourceNames =
  withScratchDirectory "package-stable-resource-names" $ \workspace -> do
    firstEntry <- writeStableResourcePackage workspace "first"
    secondEntry <- writeStableResourcePackage workspace "second"
    (firstSpirv, firstInterface) <- compileArtifact firstEntry
    (secondSpirv, secondInterface) <- compileArtifact secondEntry
    unless (firstInterface == secondInterface) $
      fail ("expected identical interfaces across package roots, got " <> show (firstInterface, secondInterface))
    unless (firstSpirv == secondSpirv) $
      fail "expected identical SPIR-V across package roots"
    case firstInterface.siBindings of
      [binding]
        | binding.biName == "shared::config::params" -> pure ()
      bindings -> fail ("expected source-qualified uniform binding, got " <> show bindings)
    case firstInterface.siOverrides of
      [override]
        | override.oiName == "shared::config::scale"
        , override.oiId == Just 7
        , override.oiSpecId == Just 7 -> pure ()
      overrides -> fail ("expected source-qualified override, got " <> show overrides)
    case bindingInfoFor "shared::config::params" firstInterface of
      Left err -> fail ("expected qualified binding lookup to succeed: " <> err)
      Right _ -> pure ()
    (_, specializedInterface) <-
      compileArtifactWith
        [OptOverrides [("shared::config::scale", OVF32 2.0)]]
        firstEntry
    unless (null specializedInterface.siOverrides) $
      fail ("expected specialized override to be absent from reflection, got " <> show specializedInterface.siOverrides)

checkPrivateOverrideKeyRejection :: IO ()
checkPrivateOverrideKeyRejection =
  withScratchDirectory "package-private-override-key" $ \workspace -> do
    entry <- writeStableResourcePackage workspace "root"
    let privateKey = "__wesl__q9_$package$6_shared8_$module$6_config5_scale"
    expectFailureWithContaining
      [OptOverrides [(privateKey, OVF32 2.0)]]
      entry
      ["unknown override option keys", privateKey]

checkMangledModulePathCollision :: IO ()
checkMangledModulePathCollision =
  withScratchDirectory "package-mangled-module-path-collision" $ \workspace -> do
    let packageDir = workspace </> "app"
        shaderDir = packageDir </> "shaders"
        entry = shaderDir </> "main.wesl"
    writeManifest packageDir ["name = \"app\"", "edition = \"2026_pre\"", "root = \"./shaders\""]
    writeSource (shaderDir </> "alpha__beta" </> "Shape.wesl") scalarStorageModule
    writeSource (shaderDir </> "alpha" </> "beta__Shape.wesl") vectorStorageModule
    writeSource entry mangledModulePathEntry
    (_, interface) <- compileArtifact entry
    scalarBinding <- bindingAt 0 interface
    vectorBinding <- bindingAt 1 interface
    assertScalarStorageLayout scalarBinding
    assertVectorStorageLayout vectorBinding

checkDependencyPackageModuleBoundary :: IO ()
checkDependencyPackageModuleBoundary =
  withScratchDirectory "package-module-boundary" $ \workspace -> do
    entry <- writeDependencyPackageModuleBoundaryPackage workspace
    (_, interface) <- compileArtifact entry
    assertOverrideNames
      [ "a::b::package::x"
      , "a::b::x"
      ]
      interface

checkCurrentModuleDependencyAliasBoundary :: IO ()
checkCurrentModuleDependencyAliasBoundary =
  withScratchDirectory "package-current-module-boundary" $ \workspace -> do
    firstEntry <- writeCurrentModuleDependencyAliasPackage workspace "first"
    secondEntry <- writeCurrentModuleDependencyAliasPackage workspace "second"
    (_, firstInterface) <- compileArtifact firstEntry
    (_, secondInterface) <- compileArtifact secondEntry
    unless (firstInterface == secondInterface) $
      fail ("expected identical interfaces across package roots, got " <> show (firstInterface, secondInterface))
    assertOverrideNames
      [ "package::shared::x"
      , "shared::x"
      ]
      firstInterface

checkCanonicalOverrideKeyRoundTrip :: IO ()
checkCanonicalOverrideKeyRoundTrip =
  withScratchDirectory "package-canonical-override-key" $ \workspace -> do
    entry <- writeDependencyPackageModuleBoundaryPackage workspace
    (_, interface) <- compileArtifactWith [OptOverrides [("a::b::x", OVF32 3.0)]] entry
    assertOverrideNames ["a::b::package::x"] interface

checkAmbiguousOverrideShorthand :: IO ()
checkAmbiguousOverrideShorthand =
  withScratchDirectory "package-ambiguous-override-shorthand" $ \workspace -> do
    entry <- writeAmbiguousOverridePackage workspace
    expectFailureWithContaining
      [OptOverrides [("a::b::c::x", OVF32 3.0)]]
      entry
      [ "ambiguous override option key a::b::c::x matches "
          <> "a::b::package::c::x, a::b::c::package::x"
      ]

writeDependencyPackageModuleBoundaryPackage :: FilePath -> IO FilePath
writeDependencyPackageModuleBoundaryPackage workspace = do
  let appDir = workspace </> "app"
      directDependencyDir = workspace </> "a"
      transitiveDependencyDir = workspace </> "b"
      entry = appDir </> "shaders" </> "main.wesl"
  writeManifest
    appDir
    [ "name = \"app\""
    , "edition = \"2026_pre\""
    , "[dependencies]"
    , "a = { path = \"../a\" }"
    ]
  writeManifest
    directDependencyDir
    [ "name = \"a\""
    , "edition = \"2026_pre\""
    , "[dependencies]"
    , "b = { path = \"../b\" }"
    ]
  writeManifest transitiveDependencyDir ["name = \"b\"", "edition = \"2026_pre\"", "root = \"./entry.wesl\""]
  writeSource (directDependencyDir </> "shaders" </> "b.wesl") directDependencyModule
  writeSource (transitiveDependencyDir </> "entry.wesl") transitiveDependencyRoot
  writeSource entry dependencyPackageModuleBoundaryEntry
  pure entry

writeAmbiguousOverridePackage :: FilePath -> IO FilePath
writeAmbiguousOverridePackage workspace = do
  let appDir = workspace </> "app"
      packageA = workspace </> "a"
      packageB = workspace </> "b"
      packageC = workspace </> "c"
      entry = appDir </> "shaders" </> "main.wesl"
  writeManifest appDir (packageWithPathDependency "app" "a" "../a")
  writeManifest packageA (packageWithPathDependency "a" "b" "../b")
  writeManifest packageB (packageWithPathDependency "b" "c" "../c")
  writeManifest packageC ["name = \"c\"", "edition = \"2026_pre\"", "root = \"./entry.wesl\""]
  writeSource
    (packageA </> "shaders" </> "relay.wesl")
    "import b::relay; fn value() -> f32 { return relay::value(); }"
  writeSource
    (packageB </> "shaders" </> "relay.wesl")
    ( unlines
        [ "import package::c as localC;"
        , "import c as dependencyC;"
        , "fn value() -> f32 { return localC::value() + dependencyC::value(); }"
        ]
    )
  writeSource
    (packageB </> "shaders" </> "c.wesl")
    "override x: f32 = 1.0; fn value() -> f32 { return x; }"
  writeSource
    (packageC </> "entry.wesl")
    "override x: f32 = 2.0; fn value() -> f32 { return x; }"
  writeSource entry (fragmentUsing "import a::relay;" "relay::value()")
  pure entry

writeCurrentModuleDependencyAliasPackage :: FilePath -> FilePath -> IO FilePath
writeCurrentModuleDependencyAliasPackage workspace rootName = do
  let root = workspace </> rootName
      appDir = root </> "app"
      dependencyDir = root </> "shared"
      shaderDir = appDir </> "shaders"
      entry = shaderDir </> "main.wesl"
  writeManifest
    appDir
    [ "name = \"app\""
    , "edition = \"2026_pre\""
    , "root = \"./shaders\""
    , "[dependencies]"
    , "shared = { path = \"../shared\" }"
    ]
  writeManifest dependencyDir ["name = \"shared\"", "edition = \"2026_pre\"", "root = \"./entry.wesl\""]
  writeSource (shaderDir </> "shared.wesl") currentPackageSharedModule
  writeSource (dependencyDir </> "entry.wesl") dependencySharedRoot
  writeSource entry currentModuleDependencyAliasEntry
  pure entry

assertOverrideNames :: [String] -> ShaderInterface -> IO ()
assertOverrideNames expected interface =
  let actual = sort (map (.oiName) interface.siOverrides)
  in unless (actual == sort expected) $
      fail ("expected canonical override names " <> show expected <> ", got " <> show actual)

writeStableResourcePackage :: FilePath -> FilePath -> IO FilePath
writeStableResourcePackage workspace rootName = do
  let root = workspace </> rootName
      appDir = root </> "app"
      dependencyDir = root </> "shared"
      entry = appDir </> "shaders" </> "main.wesl"
  writeManifest
    appDir
    [ "name = \"app\""
    , "edition = \"2026_pre\""
    , "root = \"./shaders\""
    , "[dependencies]"
    , "shared = { path = \"../shared\" }"
    ]
  writeManifest dependencyDir ["name = \"shared\"", "edition = \"2026_pre\"", "root = \"./library\""]
  writeSource (dependencyDir </> "library" </> "config.wesl") stableResourceModule
  writeSource entry stableResourceEntry
  pure entry

compileArtifact :: FilePath -> IO (BS.ByteString, ShaderInterface)
compileArtifact path = do
  result <- compileFile path
  unpackArtifact path result

compileArtifactWith :: [Option] -> FilePath -> IO (BS.ByteString, ShaderInterface)
compileArtifactWith options path = do
  result <- compileFileWith options path
  unpackArtifact path result

unpackArtifact :: FilePath -> Either CompileError SomeShader -> IO (BS.ByteString, ShaderInterface)
unpackArtifact path result =
  case result of
    Left err -> fail ("expected " <> path <> " to compile: " <> show err)
    Right (SomeShader shader) -> pure (shaderSpirv shader, shaderInterface shader)

bindingAt :: Int -> ShaderInterface -> IO BindingInfo
bindingAt index interface =
  case filter ((== fromIntegral index) . (.biBinding)) interface.siBindings of
    [binding] -> pure binding
    bindings -> fail ("expected one binding at index " <> show index <> ", got " <> show bindings)

assertScalarStorageLayout :: BindingInfo -> IO ()
assertScalarStorageLayout binding =
  case binding.biType of
    TLStruct _ [FieldLayout "value" 0 (TLScalar F32 4 4) 4 4] 4 4 -> pure ()
    layout -> fail ("expected scalar storage layout, got " <> show layout)

assertVectorStorageLayout :: BindingInfo -> IO ()
assertVectorStorageLayout binding =
  case binding.biType of
    TLStruct _ [FieldLayout "value" 0 (TLVector 4 F32 16 16) 16 16] 16 16 -> pure ()
    layout -> fail ("expected vec4 storage layout, got " <> show layout)

packageWithPathDependency :: String -> String -> FilePath -> [String]
packageWithPathDependency packageName dependencyName dependencyPath =
  [ "name = \"" <> packageName <> "\""
  , "edition = \"2026_pre\""
  , "[dependencies]"
  , dependencyName <> " = { path = \"" <> dependencyPath <> "\" }"
  ]

stableResourceModule :: String
stableResourceModule =
  unlines
    [ "struct Params { value: f32, };"
    , "@group(0) @binding(0) var<uniform> params: Params;"
    , "@id(7) override scale: f32 = 1.0;"
    ]

stableResourceEntry :: String
stableResourceEntry =
  unlines
    [ "import shared::config;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(config::params.value * config::scale);"
    , "}"
    ]

scalarStorageModule :: String
scalarStorageModule =
  unlines
    [ "struct Payload { value: f32, };"
    , "@group(0) @binding(0) var<storage, read> params: Payload;"
    , "fn value() -> f32 { return params.value; }"
    ]

vectorStorageModule :: String
vectorStorageModule =
  unlines
    [ "struct Payload { value: vec4<f32>, };"
    , "@group(0) @binding(1) var<storage, read> params: Payload;"
    , "fn value() -> f32 { return params.value.x; }"
    ]

mangledModulePathEntry :: String
mangledModulePathEntry =
  unlines
    [ "import alpha__beta::Shape as scalarShape;"
    , "import alpha::beta__Shape as vectorShape;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(scalarShape::value() + vectorShape::value());"
    , "}"
    ]

directDependencyModule :: String
directDependencyModule =
  unlines
    [ "import b as transitive;"
    , "override x: f32 = 1.0;"
    , "fn value() -> f32 { return x + transitive::x; }"
    ]

transitiveDependencyRoot :: String
transitiveDependencyRoot = "override x: f32 = 2.0;"

dependencyPackageModuleBoundaryEntry :: String
dependencyPackageModuleBoundaryEntry =
  unlines
    [ "import a::b as direct;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(direct::value());"
    , "}"
    ]

currentPackageSharedModule :: String
currentPackageSharedModule =
  unlines
    [ "override x: f32 = 1.0;"
    , "fn value() -> f32 { return x; }"
    ]

dependencySharedRoot :: String
dependencySharedRoot =
  unlines
    [ "override x: f32 = 2.0;"
    , "fn value() -> f32 { return x; }"
    ]

currentModuleDependencyAliasEntry :: String
currentModuleDependencyAliasEntry =
  unlines
    [ "import package::shared as localShared;"
    , "import shared as dependencyShared;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(localShared::value() + dependencyShared::value());"
    , "}"
    ]

fragmentShader :: String
fragmentShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(0.0);"
    , "}"
    ]

fragmentUsing :: String -> String -> String
fragmentUsing importLine value =
  unlines
    [ importLine
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(" <> value <> ");"
    , "}"
    ]

expectCompile :: FilePath -> IO ()
expectCompile path = do
  result <- compileFile path
  case result of
    Left err -> fail ("expected " <> path <> " to compile: " <> show err)
    Right (SomeShader _) -> pure ()

expectFailureContaining :: FilePath -> [String] -> IO ()
expectFailureContaining = expectFailureWithContaining []

expectFailureWithContaining :: [Option] -> FilePath -> [String] -> IO ()
expectFailureWithContaining options path evidence = do
  result <- compileFileWith options path
  case result of
    Left err ->
      unless (all (`isInfixOf` err.ceMessage) evidence) $
        fail ("expected " <> path <> " to fail with " <> show evidence <> ", got " <> show err)
    Right (SomeShader _) -> fail ("expected " <> path <> " to fail")

writeManifest :: FilePath -> [String] -> IO ()
writeManifest packageDir lines' = writeManifestText packageDir (unlines (["[package]"] <> lines'))

writeManifestText :: FilePath -> String -> IO ()
writeManifestText packageDir contents = do
  createDirectoryIfMissing True packageDir
  writeFile (packageDir </> "wesl.toml") contents

writeSource :: FilePath -> String -> IO ()
writeSource path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path contents

writeSourceBytes :: FilePath -> BS.ByteString -> IO ()
writeSourceBytes path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path contents

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

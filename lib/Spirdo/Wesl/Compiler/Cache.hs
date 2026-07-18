-- | Cache identity, key construction, and artifact storage for WESL compilation.
module Spirdo.Wesl.Compiler.Cache
  ( normalizeImportsForRoot
  , loadWeslCacheWithImports
  , writeWeslCacheWithImports
  ) where

import Control.Exception
  ( AsyncException(HeapOverflow, StackOverflow)
  , IOException
  , bracketOnError
  , evaluate
  , try
  , tryJust
  )
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric (showHex)
import Spirdo.Wesl.Types
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (isRelative, normalise, splitDirectories, takeDirectory, takeFileName, (<.>), (</>))
import System.IO (IOMode(ReadMode), hClose, hFileSize, hFlush, openBinaryTempFile, withBinaryFile)
import Text.Read (readMaybe)

normalizeImportsForRoot :: FilePath -> Map.Map FilePath Text -> Map.Map FilePath Text
normalizeImportsForRoot rootDir imports0 =
  if null rootDir || rootDir == "."
    then imports0
    else foldl' add imports0 (Map.toList imports0)
  where
    add acc (key, src)
      | isRelative key && not (isUnder rootDir key) =
          Map.insertWith (\_ old -> old) (rootDir </> key) src acc
      | otherwise = acc

    isUnder base path =
      let baseSegs = splitDirectories (normalise base)
          pathSegs = splitDirectories (normalise path)
      in baseSegs `isPrefixOf` pathSegs

weslCacheSchema :: String
weslCacheSchema = "spirdo.wesl.cache/4"

-- Bump this revision whenever compiler output, reflection, validation, or the
-- cache payload contract changes incompatibly. Keep this independent of the
-- generated Paths_spirdo module: this code runs while the package is linked
-- into Template Haskell, where that generated symbol is not always available.
weslCacheAbiRevision :: String
weslCacheAbiRevision = "10"

weslCompilerIdentity :: String
weslCompilerIdentity = "spirdo/wesl-cache-abi/" <> weslCacheAbiRevision

defaultCacheDir :: FilePath
defaultCacheDir = "dist-newstyle" </> ".wesl-cache"

cacheInputLinesWithImports :: FilePath -> Imports mods -> String -> [String]
cacheInputLinesWithImports rootName importSet src =
  let rootDir = takeDirectory rootName
      importMap = normalizeImportsForRoot rootDir (importsMap importSet)
      importEntries = sort (Map.toList importMap)
      importOrderLines =
        map ("import-name=" <>) (map normalizeModuleKey (importsNames importSet))
      importLines =
        concatMap
          ( \(name, text) ->
              [ "import=" <> normalise name
              , T.unpack text
              ]
          )
          importEntries
  in ["kind=imports", "root=" <> normalise rootName, src]
      <> importOrderLines
      <> importLines

weslCacheKeyFromLines :: CompileOptions -> [String] -> String
weslCacheKeyFromLines opts bodyLines =
  let keyLines = cacheInputLines opts bodyLines
      hash = foldl' updateLine fnv1a64Offset keyLines
      hex = showHex hash ""
  in replicate (16 - length hex) '0' <> hex

cacheInputIdentity :: CompileOptions -> [String] -> ByteString
cacheInputIdentity opts bodyLines =
  BS.concat (map encodeIdentityLine (cacheInputLines opts bodyLines))
  where
    -- Length framing keeps embedded newlines from changing input boundaries.
    encodeIdentityLine line =
      let utf8 = TE.encodeUtf8 (T.pack line)
          lengthPrefix = TE.encodeUtf8 (T.pack (show (BS.length utf8) <> ":"))
      in lengthPrefix <> utf8 <> "\n"

cacheInputLines :: CompileOptions -> [String] -> [String]
cacheInputLines opts bodyLines =
  [ "schema=" <> weslCacheSchema
  , "compiler=" <> weslCompilerIdentity
  , "v=" <> show (opts.spirvVersion)
  , "target=" <> show opts.targetEnvironment
  , "openGlBindingRemaps=" <> show opts.openGlBindingRemaps
  , "features=" <> show (opts.enabledFeatures)
  , "overrides=" <> show opts.overrideValues
  , "spec=" <> show opts.overrideSpecMode
  , "samplerMode=" <> show opts.samplerBindingMode
  , "entry=" <> show opts.entryPointName
  ]
    <> bodyLines

updateLine :: Word64 -> String -> Word64
updateLine acc line =
  let utf8 = TE.encodeUtf8 (T.pack line)
      withLength = hashWord64 acc (fromIntegral (BS.length utf8))
  in BS.foldl' fnv1a64Step withLength utf8

hashWord64 :: Word64 -> Word64 -> Word64
hashWord64 acc word =
  foldl'
    fnv1a64Step
    acc
    [ fromIntegral (word `shiftR` bit)
    | bit <- [0, 8 .. 56]
    ]

fnv1a64Offset :: Word64
fnv1a64Offset = 14695981039346656037

fnv1a64Prime :: Word64
fnv1a64Prime = 1099511628211

fnv1a64Step :: Word64 -> Word8 -> Word64
fnv1a64Step acc byte = (acc `xor` fromIntegral byte) * fnv1a64Prime

data WeslCacheEntry = WeslCacheEntry
  { wceSchema :: !String
  , wceCompiler :: !String
  , wceKey :: !String
  , wceIdentity :: !ByteString
  , wceSpirv :: !ByteString
  , wceInterface :: !ShaderInterface
  } deriving (Read, Show)

weslCachePath :: CompileOptions -> String -> FilePath
weslCachePath opts key =
  let baseDir = if null opts.cacheDir then defaultCacheDir else opts.cacheDir
      base = baseDir </> key
  in base <.> "cache"

loadWeslCacheWithImports :: CompileOptions -> FilePath -> Imports mods -> String -> IO (Maybe (ByteString, ShaderInterface))
loadWeslCacheWithImports opts rootName importSet src =
  if not (opts.cacheEnabled)
    then pure Nothing
    else do
      let bodyLines = cacheInputLinesWithImports rootName importSet src
          key = weslCacheKeyFromLines opts bodyLines
          identity = cacheInputIdentity opts bodyLines
          cachePath = weslCachePath opts key
      loadCacheEntry opts cachePath key identity

loadCacheEntry :: CompileOptions -> FilePath -> String -> ByteString -> IO (Maybe (ByteString, ShaderInterface))
loadCacheEntry opts cachePath key identity = do
  ioResult <-
    try
      (tryJust cacheResourceExhaustion (readCacheEntry opts cachePath key identity))
      :: IO
          ( Either
              IOException
              (Either () (Maybe (ByteString, ShaderInterface)))
          )
  pure $
    case ioResult of
      Left _ -> Nothing
      Right (Left _) -> Nothing
      Right (Right cached) -> cached
  where
    cacheResourceExhaustion asyncException =
      case asyncException of
        HeapOverflow -> Just ()
        StackOverflow -> Just ()
        _ -> Nothing

writeWeslCacheEntry :: CompileOptions -> String -> ByteString -> ByteString -> ShaderInterface -> IO ()
writeWeslCacheEntry opts key identity bytes iface =
  if not (opts.cacheEnabled) || not (validCacheKey key)
    then pure ()
    else do
      let cachePath = weslCachePath opts key
          entry = WeslCacheEntry weslCacheSchema weslCompilerIdentity key identity bytes iface
          encoded = encodeCacheEntry entry
      if not (validShaderInterface opts iface && isSaneSpirv opts iface bytes)
          || BS.length encoded > maxCacheArtifactBytes
        then pure ()
        else do
          -- The cache is an optimization; a filesystem cache failure must not fail compilation.
          _ <- try (writeCacheEntry cachePath encoded) :: IO (Either IOException ())
          pure ()

readCacheEntry :: CompileOptions -> FilePath -> String -> ByteString -> IO (Maybe (ByteString, ShaderInterface))
readCacheEntry opts cachePath key identity = do
  contents <- readBoundedCacheEntry cachePath
  evaluate $
    case contents >>= decodeCacheEntry of
      Just entry
        | entry.wceSchema == weslCacheSchema
        , entry.wceCompiler == weslCompilerIdentity
        , entry.wceKey == key
        , entry.wceIdentity == identity
        , validShaderInterface opts entry.wceInterface
        , isSaneSpirv opts entry.wceInterface entry.wceSpirv ->
            Just (entry.wceSpirv, entry.wceInterface)
      _ -> Nothing

maxCacheArtifactBytes :: Int
maxCacheArtifactBytes = 8 * 1024 * 1024

validCacheKey :: String -> Bool
validCacheKey key =
  length key == 16
    && all (\character -> character >= '0' && character <= '9' || character >= 'a' && character <= 'f') key

readBoundedCacheEntry :: FilePath -> IO (Maybe ByteString)
readBoundedCacheEntry cachePath =
  withBinaryFile cachePath ReadMode $ \handle -> do
    size <- hFileSize handle
    if size < 0 || size > fromIntegral maxCacheArtifactBytes
      then pure Nothing
      else Just <$> BS.hGet handle (fromIntegral size)

encodeCacheEntry :: WeslCacheEntry -> ByteString
encodeCacheEntry entry =
  let payload = TE.encodeUtf8 (T.pack (show entry))
  in cacheFrameMagic
      <> encodeWord32LE (fromIntegral (BS.length payload))
      <> encodeWord64LE (cachePayloadChecksum payload)
      <> payload

decodeCacheEntry :: ByteString -> Maybe WeslCacheEntry
decodeCacheEntry bytes = do
  if BS.length bytes < cacheFrameHeaderSize
      || BS.take (BS.length cacheFrameMagic) bytes /= cacheFrameMagic
    then Nothing
    else Just ()
  let declaredPayloadBytes = word32At bytes (BS.length cacheFrameMagic)
      storedChecksum = word64At bytes (BS.length cacheFrameMagic + 4)
      payload = BS.drop cacheFrameHeaderSize bytes
  if toInteger declaredPayloadBytes /= toInteger (BS.length payload)
      || storedChecksum /= cachePayloadChecksum payload
      || not (preflightCachePayload payload)
    then Nothing
    else Just ()
  text <- either (const Nothing) Just (TE.decodeUtf8' payload)
  readMaybe (T.unpack text)

cacheFrameMagic :: ByteString
cacheFrameMagic = BS.pack [0x53, 0x50, 0x57, 0x53, 0x43, 0x30, 0x34, 0x0a]

cacheFrameHeaderSize :: Int
cacheFrameHeaderSize = BS.length cacheFrameMagic + 4 + 8

-- This checksum detects accidental local-cache corruption. It does not
-- authenticate artifacts and must not be used to establish provenance.
cachePayloadChecksum :: ByteString -> Word64
cachePayloadChecksum = BS.foldl' fnv1a64Step fnv1a64Offset

encodeWord32LE :: Word32 -> ByteString
encodeWord32LE word =
  BS.pack
    [ fromIntegral (word `shiftR` bit)
    | bit <- [0, 8 .. 24]
    ]

encodeWord64LE :: Word64 -> ByteString
encodeWord64LE word =
  BS.pack
    [ fromIntegral (word `shiftR` bit)
    | bit <- [0, 8 .. 56]
    ]

word64At :: ByteString -> Int -> Word64
word64At bytes offset =
  foldl'
    (\acc bit -> acc .|. (fromIntegral (BS.index bytes (offset + bit `div` 8)) `shiftL` bit))
    0
    [0, 8 .. 56]

maxCacheSyntaxDepth :: Int
maxCacheSyntaxDepth = 192

maxCacheSyntaxSeparators :: Int
maxCacheSyntaxSeparators = 500000

maxCacheSyntaxTokens :: Int
maxCacheSyntaxTokens = 2000000

maxCacheQuotedStrings :: Int
maxCacheQuotedStrings = 200000

maxCacheQuotedLiteralBytes :: Int
maxCacheQuotedLiteralBytes = 6 * 1024 * 1024

maxCacheAtomBytes :: Int
maxCacheAtomBytes = 4096

maxCacheEscapeBytes :: Int
maxCacheEscapeBytes = 32

preflightCachePayload :: ByteString -> Bool
preflightCachePayload bytes =
  BS.length bytes <= maxCacheArtifactBytes - cacheFrameHeaderSize
    && BS.all (< 0x80) bytes
    && scan 0 [] 0 0 0
  where
    inputBytes = BS.length bytes

    scan offset delimiters separators tokens quotedStrings
      | separators > maxCacheSyntaxSeparators = False
      | tokens > maxCacheSyntaxTokens = False
      | quotedStrings > maxCacheQuotedStrings = False
      | offset == inputBytes = null delimiters
      | otherwise =
          case BS.index bytes offset of
            byte
              | isCacheWhitespace byte ->
                  scan (offset + 1) delimiters separators tokens quotedStrings
              | byte == 0x22 ->
                  case scanQuoted (offset + 1) 0 of
                    Nothing -> False
                    Just nextOffset ->
                      scan nextOffset delimiters separators (tokens + 1) (quotedStrings + 1)
              | Just closing <- openingDelimiter byte ->
                  length delimiters < maxCacheSyntaxDepth
                    && scan (offset + 1) (closing : delimiters) separators (tokens + 1) quotedStrings
              | isClosingDelimiter byte ->
                  case delimiters of
                    expected : rest
                      | byte == expected ->
                          scan (offset + 1) rest separators (tokens + 1) quotedStrings
                    _ -> False
              | byte == 0x2c ->
                  scan (offset + 1) delimiters (separators + 1) (tokens + 1) quotedStrings
              | otherwise ->
                  case scanAtom offset 0 of
                    Nothing -> False
                    Just nextOffset ->
                      scan nextOffset delimiters separators (tokens + 1) quotedStrings

    scanQuoted offset quotedBytes
      | offset >= inputBytes || quotedBytes > maxCacheQuotedLiteralBytes = Nothing
      | otherwise =
          case BS.index bytes offset of
            0x22 -> Just (offset + 1)
            0x0a -> Nothing
            0x0d -> Nothing
            0x5c -> do
              afterEscape <- scanEscape (offset + 1)
              scanQuoted afterEscape (quotedBytes + afterEscape - offset)
            _ -> scanQuoted (offset + 1) (quotedBytes + 1)

    scanEscape offset
      | offset >= inputBytes = Nothing
      | isCacheDigit (BS.index bytes offset) = scanEscapeRun isCacheDigit offset 0
      | BS.index bytes offset == 0x78 = scanEscapeRun isCacheHexDigit (offset + 1) 0
      | BS.index bytes offset == 0x6f = scanEscapeRun isCacheOctalDigit (offset + 1) 0
      | isCacheUpper (BS.index bytes offset) = scanEscapeRun isCacheUpper offset 0
      | otherwise = Just (offset + 1)

    scanEscapeRun accepts offset escapeBytes
      | escapeBytes >= maxCacheEscapeBytes = Nothing
      | offset >= inputBytes = Just offset
      | accepts (BS.index bytes offset) =
          scanEscapeRun accepts (offset + 1) (escapeBytes + 1)
      | escapeBytes == 0 = Nothing
      | otherwise = Just offset

    scanAtom offset atomBytes
      | offset >= inputBytes = Just offset
      | atomBytes >= maxCacheAtomBytes = Nothing
      | isCacheTokenDelimiter (BS.index bytes offset) = Just offset
      | otherwise = scanAtom (offset + 1) (atomBytes + 1)

isCacheWhitespace :: Word8 -> Bool
isCacheWhitespace byte = byte `elem` [0x09, 0x0a, 0x0d, 0x20]

isCacheDigit :: Word8 -> Bool
isCacheDigit byte = byte >= 0x30 && byte <= 0x39

isCacheOctalDigit :: Word8 -> Bool
isCacheOctalDigit byte = byte >= 0x30 && byte <= 0x37

isCacheHexDigit :: Word8 -> Bool
isCacheHexDigit byte =
  isCacheDigit byte
    || (byte >= 0x41 && byte <= 0x46)
    || (byte >= 0x61 && byte <= 0x66)

isCacheUpper :: Word8 -> Bool
isCacheUpper byte = byte >= 0x41 && byte <= 0x5a

isCacheTokenDelimiter :: Word8 -> Bool
isCacheTokenDelimiter byte =
  isCacheWhitespace byte
    || isJust (openingDelimiter byte)
    || isClosingDelimiter byte
    || byte == 0x22
    || byte == 0x2c

openingDelimiter :: Word8 -> Maybe Word8
openingDelimiter byte =
  case byte of
    0x28 -> Just 0x29
    0x5b -> Just 0x5d
    0x7b -> Just 0x7d
    _ -> Nothing

isClosingDelimiter :: Word8 -> Bool
isClosingDelimiter byte = byte `elem` [0x29, 0x5d, 0x7d]

validShaderInterface :: CompileOptions -> ShaderInterface -> Bool
validShaderInterface opts iface =
  iface.siSamplerMode == opts.samplerBindingMode
    && hasAtMost maxReflectionEntries iface.siBindings
    && hasAtMost maxReflectionEntries iface.siOverrides
    && unique (map (.biName) iface.siBindings)
    && unique (map (\binding -> (binding.biGroup, binding.biBinding)) iface.siBindings)
    && unique (map (.oiName) iface.siOverrides)
    && unique (catMaybes (map (.oiId) iface.siOverrides))
    && unique (catMaybes (map (.oiSpecId) iface.siOverrides))
    && unique (catMaybes (map overrideRuntimeId iface.siOverrides))
    && unique (map (.biName) iface.siBindings <> map (.oiName) iface.siOverrides)
    && all (validBinding opts.samplerBindingMode) iface.siBindings
    && all (validOverride opts.overrideSpecMode) iface.siOverrides
    && maybe False validStageIO iface.siStageIO
    && maybe True validImmediateLayout iface.siPushConstants
    && withinReflectionLayoutNodeLimit iface
    && consistentNamedStructLayouts iface

maxReflectionEntries :: Int
maxReflectionEntries = 4096

maxReflectionFields :: Int
maxReflectionFields = 4096

maxReflectionLayoutDepth :: Int
maxReflectionLayoutDepth = 64

maxReflectionLayoutNodes :: Int
maxReflectionLayoutNodes = 65536

maxReflectionNameLength :: Int
maxReflectionNameLength = 4096

hasAtMost :: Int -> [a] -> Bool
hasAtMost remaining values =
  case (remaining, values) of
    (_, []) -> True
    (0, _) -> False
    (_, _ : rest) -> hasAtMost (remaining - 1) rest

validName :: String -> Bool
validName name = not (null name) && hasAtMost maxReflectionNameLength name

unique :: Ord a => [a] -> Bool
unique values = Set.size (Set.fromList values) == length values

validBinding :: SamplerBindingMode -> BindingInfo -> Bool
validBinding samplerMode binding =
  validName binding.biName
    && validTypeLayout 0 binding.biType
    && bindingKindMatchesLayout binding.biKind binding.biType
    && case samplerMode of
      SamplerCombined -> not (isSamplerKind binding.biKind)
      SamplerSeparate -> True

bindingKindMatchesLayout :: BindingKind -> TypeLayout -> Bool
bindingKindMatchesLayout kind layout =
  case (kind, layout) of
    (BUniform, _) ->
      isHostShareableLayout layout
        && not (containsAtomicLayout layout)
        && not (containsRuntimeArray layout)
    (BStorageRead, _) ->
      validStorageBindingLayout layout && not (containsAtomicLayout layout)
    (BStorageReadWrite, _) ->
      validStorageBindingLayout layout && containsOnlyStorageAtomics layout
    (BSampler, TLSampler) -> True
    (BSamplerComparison, TLSamplerComparison) -> True
    (BTexture1D, TLTexture1D _) -> True
    (BTexture1DArray, TLTexture1DArray _) -> True
    (BTexture2D, TLTexture2D _) -> True
    (BTexture2DArray, TLTexture2DArray _) -> True
    (BTexture3D, TLTexture3D _) -> True
    (BTextureCube, TLTextureCube _) -> True
    (BTextureCubeArray, TLTextureCubeArray _) -> True
    (BTextureMultisampled2D, TLTextureMultisampled2D _) -> True
    (BTextureDepth2D, TLTextureDepth2D) -> True
    (BTextureDepth2DArray, TLTextureDepth2DArray) -> True
    (BTextureDepthCube, TLTextureDepthCube) -> True
    (BTextureDepthCubeArray, TLTextureDepthCubeArray) -> True
    (BTextureDepthMultisampled2D, TLTextureDepthMultisampled2D) -> True
    (BStorageTexture1D, TLStorageTexture1D _ _) -> True
    (BStorageTexture2D, TLStorageTexture2D _ _) -> True
    (BStorageTexture2DArray, TLStorageTexture2DArray _ _) -> True
    (BStorageTexture3D, TLStorageTexture3D _ _) -> True
    _ -> False

validOverride :: OverrideSpecMode -> OverrideInfo -> Bool
validOverride specMode override =
  validName override.oiName
    && validConstLayout override.oiType
    && maybe True (\explicitId -> maybe True (== explicitId) override.oiSpecId) override.oiId
    && case specMode of
      SpecStrict -> True
      SpecParity -> isJust override.oiSpecId

overrideRuntimeId :: OverrideInfo -> Maybe Word32
overrideRuntimeId override =
  case override.oiSpecId of
    Just specId -> Just specId
    Nothing -> override.oiId

validStageIO :: StageIO -> Bool
validStageIO stageInfo =
  validIOParams stageInfo.sioInputs
    && validIOParams stageInfo.sioOutputs
    && all (validStageInput stageInfo.sioStage) stageInfo.sioInputs
    && all (validStageOutput stageInfo.sioStage) stageInfo.sioOutputs
    && case (stageInfo.sioStage, stageInfo.sioWorkgroupSize) of
      (ShaderStageCompute, Nothing) -> null stageInfo.sioOutputs
      (ShaderStageCompute, Just (x, y, z)) ->
        x > 0 && y > 0 && z > 0 && null stageInfo.sioOutputs
      (ShaderStageFragment, Nothing) -> True
      (ShaderStageVertex, Nothing) -> True
      _ -> False

validIOParams :: [IOParam] -> Bool
validIOParams params =
  hasAtMost maxReflectionEntries params
    && unique (map (.ioName) params)
    && unique (catMaybes (map (.ioLocation) params))
    && unique (catMaybes (map (.ioBuiltin) params))
    && all validIOParam params

validIOParam :: IOParam -> Bool
validIOParam param =
  validName param.ioName
    && maybe True validName param.ioBuiltin
    && isStageIOLayout param.ioType
    && validTypeLayout 0 param.ioType
    && case (param.ioLocation, param.ioBuiltin) of
      (Just _, Nothing) -> not (isBooleanLayout param.ioType)
      (Nothing, Just _) -> True
      _ -> False

validStageInput :: ShaderStage -> IOParam -> Bool
validStageInput stage param =
  case (param.ioLocation, param.ioBuiltin) of
    (Just _, Nothing) -> stage /= ShaderStageCompute
    (Nothing, Just builtin) ->
      case (stage, builtin) of
        (ShaderStageCompute, "global_invocation_id") -> layoutShapeIsVector 3 U32 param.ioType
        (ShaderStageCompute, "local_invocation_id") -> layoutShapeIsVector 3 U32 param.ioType
        (ShaderStageCompute, "workgroup_id") -> layoutShapeIsVector 3 U32 param.ioType
        (ShaderStageCompute, "local_invocation_index") -> layoutShapeIsScalar U32 param.ioType
        (ShaderStageCompute, "num_workgroups") -> layoutShapeIsVector 3 U32 param.ioType
        (ShaderStageVertex, "vertex_index") -> layoutShapeIsScalar U32 param.ioType
        (ShaderStageVertex, "instance_index") -> layoutShapeIsScalar U32 param.ioType
        (ShaderStageFragment, "position") -> layoutShapeIsVector 4 F32 param.ioType
        (ShaderStageFragment, "front_facing") -> layoutShapeIsScalar Bool param.ioType
        (ShaderStageFragment, "sample_index") -> layoutShapeIsScalar U32 param.ioType
        (ShaderStageFragment, "sample_mask") -> layoutShapeIsScalar U32 param.ioType
        _ -> False
    _ -> False

validStageOutput :: ShaderStage -> IOParam -> Bool
validStageOutput stage param =
  case (param.ioLocation, param.ioBuiltin) of
    (Just _, Nothing) -> stage /= ShaderStageCompute
    (Nothing, Just builtin) ->
      case (stage, builtin) of
        (ShaderStageVertex, "position") -> layoutShapeIsVector 4 F32 param.ioType
        (ShaderStageFragment, "frag_depth") -> layoutShapeIsScalar F32 param.ioType
        (ShaderStageFragment, "sample_mask") -> layoutShapeIsScalar U32 param.ioType
        _ -> False
    _ -> False

layoutShapeIsScalar :: Scalar -> TypeLayout -> Bool
layoutShapeIsScalar expected layout =
  case layout of
    TLScalar actual _ _ -> actual == expected
    _ -> False

layoutShapeIsVector :: Int -> Scalar -> TypeLayout -> Bool
layoutShapeIsVector expectedWidth expectedScalar layout =
  case layout of
    TLVector actualWidth actualScalar _ _ ->
      actualWidth == expectedWidth && actualScalar == expectedScalar
    _ -> False

validConstLayout :: TypeLayout -> Bool
validConstLayout layout =
  validTypeLayout 0 layout
    && isDataLayout layout
    && not (containsAtomicLayout layout)
    && not (containsRuntimeArray layout)

isStageIOLayout :: TypeLayout -> Bool
isStageIOLayout layout =
  case layout of
    TLScalar {} -> True
    TLVector {} -> True
    _ -> False

isBooleanLayout :: TypeLayout -> Bool
isBooleanLayout layout =
  case layout of
    TLScalar Bool _ _ -> True
    TLVector _ Bool _ _ -> True
    _ -> False

isDataLayout :: TypeLayout -> Bool
isDataLayout layout =
  case layout of
    TLScalar {} -> True
    TLVector {} -> True
    TLMatrix {} -> True
    TLArray _ _ element _ _ -> isDataLayout element
    TLStruct _ fields _ _ -> all (isDataLayout . (.flType)) fields
    TLAtomic {} -> True
    _ -> False

isHostShareableLayout :: TypeLayout -> Bool
isHostShareableLayout layout = isDataLayout layout && not (containsBooleanLayout layout)

validStorageBindingLayout :: TypeLayout -> Bool
validStorageBindingLayout layout =
  isHostShareableLayout layout
    && validStorageRuntimeArrayLayout layout

validImmediateLayout :: TypeLayout -> Bool
validImmediateLayout layout =
  validTypeLayout 0 layout
    && isHostShareableLayout layout
    && not (containsAtomicLayout layout)
    && not (containsRuntimeArray layout)

validStorageRuntimeArrayLayout :: TypeLayout -> Bool
validStorageRuntimeArrayLayout layout =
  case layout of
    TLArray Nothing _ element _ _ -> not (containsRuntimeArray element)
    TLStruct _ fields _ _ ->
      case reverse fields of
        [] -> True
        lastField : precedingFields ->
          all (not . containsRuntimeArray . (.flType)) precedingFields
            && case lastField.flType of
              TLArray Nothing _ element _ _ -> not (containsRuntimeArray element)
              lastLayout -> not (containsRuntimeArray lastLayout)
    _ -> not (containsRuntimeArray layout)

containsOnlyStorageAtomics :: TypeLayout -> Bool
containsOnlyStorageAtomics layout =
  case layout of
    TLAtomic scalar -> scalar `elem` [I32, U32]
    TLArray _ _ element _ _ -> containsOnlyStorageAtomics element
    TLStruct _ fields _ _ -> all (containsOnlyStorageAtomics . (.flType)) fields
    _ -> True

containsBooleanLayout :: TypeLayout -> Bool
containsBooleanLayout layout =
  case layout of
    TLScalar Bool _ _ -> True
    TLVector _ Bool _ _ -> True
    TLArray _ _ element _ _ -> containsBooleanLayout element
    TLStruct _ fields _ _ -> any (containsBooleanLayout . (.flType)) fields
    _ -> False

containsAtomicLayout :: TypeLayout -> Bool
containsAtomicLayout layout =
  case layout of
    TLAtomic {} -> True
    TLArray _ _ element _ _ -> containsAtomicLayout element
    TLStruct _ fields _ _ -> any (containsAtomicLayout . (.flType)) fields
    _ -> False

containsRuntimeArray :: TypeLayout -> Bool
containsRuntimeArray layout =
  case layout of
    TLArray Nothing _ _ _ _ -> True
    TLArray _ _ element _ _ -> containsRuntimeArray element
    TLStruct _ fields _ _ -> any (containsRuntimeArray . (.flType)) fields
    _ -> False

validTypeLayout :: Int -> TypeLayout -> Bool
validTypeLayout depth layout =
  depth <= maxReflectionLayoutDepth
    && case layout of
      TLScalar scalar align size -> (align, size) == scalarLayout scalar
      TLVector width scalar align size ->
        width `elem` [2, 3, 4] && (align, size) == vectorLayout scalar width
      TLMatrix columns rows scalar align size stride ->
        scalar `elem` [F16, F32]
          && columns `elem` [2, 3, 4]
          && rows `elem` [2, 3, 4]
          && layout == matrixLayout columns rows scalar
          && align > 0
          && size > 0
          && stride > 0
      TLArray count stride element align size ->
        validTypeLayout (depth + 1) element
          && isDataLayout element
          && not (containsRuntimeArray element)
          && align == layoutAlign element
          && align > 0
          && Just stride == canonicalRoundUp (layoutSize element) align
          && stride > 0
          && case count of
            Nothing -> size == 0
            Just elementCount ->
              elementCount > 0
                && toInteger elementCount <= toInteger (maxBound :: Word32)
                && toInteger size == toInteger stride * toInteger elementCount
      TLStruct name fields align size -> validStructLayout depth name fields align size
      TLSampler -> True
      TLSamplerComparison -> True
      TLTexture1D scalar -> validSampledTextureScalar scalar
      TLTexture1DArray scalar -> validSampledTextureScalar scalar
      TLTexture2D scalar -> validSampledTextureScalar scalar
      TLTexture2DArray scalar -> validSampledTextureScalar scalar
      TLTexture3D scalar -> validSampledTextureScalar scalar
      TLTextureCube scalar -> validSampledTextureScalar scalar
      TLTextureCubeArray scalar -> validSampledTextureScalar scalar
      TLTextureMultisampled2D scalar -> validSampledTextureScalar scalar
      TLTextureDepth2D -> True
      TLTextureDepth2DArray -> True
      TLTextureDepthCube -> True
      TLTextureDepthCubeArray -> True
      TLTextureDepthMultisampled2D -> True
      TLStorageTexture1D _ _ -> True
      TLStorageTexture2D _ _ -> True
      TLStorageTexture2DArray _ _ -> True
      TLStorageTexture3D _ _ -> True
      TLAtomic scalar -> scalar `elem` [I32, U32]
      TLPointer {} -> False

validStructLayout :: Int -> String -> [FieldLayout] -> Word32 -> Word32 -> Bool
validStructLayout depth name fields align size =
  validName name
    && hasAtMost maxReflectionFields fields
    && unique (map (.flName) fields)
    && align > 0
    && isPowerOfTwo align
    && all (validFieldLayout depth) fields
    && validRuntimeArrayPosition fields
    && align == maximum (1 : map (.flAlign) fields)
    && canonicalStructFields 0 fields
    && case reverse fields of
      [] -> align == 1 && size == 0
      lastField : _ ->
        toInteger size
          == roundUpInteger
              (toInteger lastField.flOffset + toInteger lastField.flSize)
              (toInteger align)

validFieldLayout :: Int -> FieldLayout -> Bool
validFieldLayout depth field =
  validName field.flName
    && validTypeLayout (depth + 1) field.flType
    && isDataLayout field.flType
    && field.flAlign >= layoutAlign field.flType
    && field.flAlign > 0
    && isPowerOfTwo field.flAlign
    && field.flSize >= layoutSize field.flType
    && (not (containsRuntimeArray field.flType) || field.flSize == layoutSize field.flType)

validRuntimeArrayPosition :: [FieldLayout] -> Bool
validRuntimeArrayPosition fields =
  case reverse fields of
    [] -> True
    lastField : precedingFields ->
      all (not . containsRuntimeArray . (.flType)) precedingFields
        && case lastField.flType of
          TLArray Nothing _ _ _ _ -> True
          _ -> not (containsRuntimeArray lastField.flType)

canonicalStructFields :: Word32 -> [FieldLayout] -> Bool
canonicalStructFields _ [] = True
canonicalStructFields previousEnd (field : rest) =
  let expectedOffset = canonicalRoundUp previousEnd field.flAlign
      fieldEnd = toInteger field.flOffset + toInteger field.flSize
  in Just field.flOffset == expectedOffset
      && fieldEnd <= toInteger (maxBound :: Word32)
      && canonicalStructFields (fromInteger fieldEnd) rest

validSampledTextureScalar :: Scalar -> Bool
validSampledTextureScalar scalar = scalar `elem` [I32, U32, F32]

canonicalRoundUp :: Word32 -> Word32 -> Maybe Word32
canonicalRoundUp value align =
  let rounded = roundUpInteger (toInteger value) (toInteger align)
  in if rounded > toInteger (maxBound :: Word32)
      then Nothing
      else Just (fromInteger rounded)

roundUpInteger :: Integer -> Integer -> Integer
roundUpInteger value align = ((value + align - 1) `div` align) * align

isPowerOfTwo :: Word32 -> Bool
isPowerOfTwo value = value /= 0 && value .&. (value - 1) == 0

consistentNamedStructLayouts :: ShaderInterface -> Bool
consistentNamedStructLayouts iface =
  go Map.empty (concatMap namedStructLayouts (allReflectionLayouts iface))
  where
    go _ [] = True
    go known ((name, layout) : rest) =
      case Map.lookup name known of
        Nothing -> go (Map.insert name layout known) rest
        Just expected -> expected == layout && go known rest

namedStructLayouts :: TypeLayout -> [(String, TypeLayout)]
namedStructLayouts layout =
  case layout of
    TLArray _ _ element _ _ -> namedStructLayouts element
    TLStruct name fields _ _ ->
      (name, layout) : concatMap (namedStructLayouts . (.flType)) fields
    TLPointer _ _ element -> namedStructLayouts element
    _ -> []

allReflectionLayouts :: ShaderInterface -> [TypeLayout]
allReflectionLayouts iface =
  map (.biType) iface.siBindings
    <> map (.oiType) iface.siOverrides
    <> maybe [] (map (.ioType) . (.sioInputs)) iface.siStageIO
    <> maybe [] (map (.ioType) . (.sioOutputs)) iface.siStageIO
    <> maybe [] pure iface.siPushConstants

withinReflectionLayoutNodeLimit :: ShaderInterface -> Bool
withinReflectionLayoutNodeLimit iface =
  isJust (consumeLayouts maxReflectionLayoutNodes (allReflectionLayouts iface))

consumeLayouts :: Int -> [TypeLayout] -> Maybe Int
consumeLayouts remaining layouts =
  case layouts of
    [] -> Just remaining
    layout : rest -> do
      afterLayout <- consumeLayout remaining layout
      consumeLayouts afterLayout rest

consumeLayout :: Int -> TypeLayout -> Maybe Int
consumeLayout remaining layout
  | remaining <= 0 = Nothing
  | otherwise =
      case layout of
        TLArray _ _ element _ _ -> consumeLayout (remaining - 1) element
        TLStruct _ fields _ _ -> consumeFieldLayouts (remaining - 1) fields
        TLPointer _ _ element -> consumeLayout (remaining - 1) element
        _ -> Just (remaining - 1)

consumeFieldLayouts :: Int -> [FieldLayout] -> Maybe Int
consumeFieldLayouts remaining fields =
  case fields of
    [] -> Just remaining
    field : rest -> do
      afterField <- consumeLayout remaining field.flType
      consumeFieldLayouts afterField rest

data SpirvFacts = SpirvFacts
  { sfShaderCapabilities :: !Int
  , sfMemoryModels :: !Int
  , sfEntryPoints :: !Int
  }

isSaneSpirv :: CompileOptions -> ShaderInterface -> ByteString -> Bool
isSaneSpirv opts iface bytes =
  BS.length bytes > spirvHeaderSize
    && BS.length bytes `mod` 4 == 0
    && word32At bytes 0 == spirvMagic
    && word32At bytes 4 == opts.spirvVersion
    && word32At bytes 8 == 0
    && saneBound
    && word32At bytes 16 == 0
    && case iface.siStageIO of
      Nothing -> False
      Just stageInfo ->
        case inspectInstructions (executionModel stageInfo.sioStage) spirvHeaderSize emptySpirvFacts of
          Just facts ->
            facts.sfShaderCapabilities == 1
              && facts.sfMemoryModels == 1
              && facts.sfEntryPoints == 1
          Nothing -> False
  where
    bound = word32At bytes 12
    moduleWords = BS.length bytes `div` 4
    saneBound = bound > 1 && toInteger bound <= toInteger moduleWords

    inspectInstructions expectedModel offset facts
      | offset == BS.length bytes = Just facts
      | otherwise =
          let instruction = word32At bytes offset
              wordCount = fromIntegral (instruction `shiftR` 16) :: Int
              opcode = fromIntegral (instruction .&. 0xffff) :: Word16
              remainingWords = (BS.length bytes - offset) `div` 4
              nextOffset = offset + wordCount * 4
          in if wordCount <= 0 || wordCount > remainingWords
              then Nothing
              else do
                nextFacts <- inspectInstruction expectedModel offset nextOffset opcode wordCount facts
                inspectInstructions expectedModel nextOffset nextFacts

    inspectInstruction expectedModel offset nextOffset opcode wordCount facts
      | opcode == opCapability =
          if wordCount /= 2
            then Nothing
            else
              let capability = word32At bytes (offset + 4)
              in Just
                  facts
                    { sfShaderCapabilities =
                        facts.sfShaderCapabilities + if capability == capabilityShader then 1 else 0
                    }
      | opcode == opMemoryModel =
          if wordCount == 3
              && word32At bytes (offset + 4) == addressingLogical
              && word32At bytes (offset + 8) == memoryModelGLSL450
            then Just facts { sfMemoryModels = facts.sfMemoryModels + 1 }
            else Nothing
      | opcode == opEntryPoint =
          let model = if wordCount >= 2 then word32At bytes (offset + 4) else maxBound
              entryPointId = if wordCount >= 3 then word32At bytes (offset + 8) else 0
              nameOffset = offset + 12
          in if wordCount >= 4
                && model == expectedModel
                && entryPointId > 0
                && entryPointId < bound
                && BS.index bytes nameOffset /= 0
                && hasNullByte nameOffset nextOffset
              then Just facts { sfEntryPoints = facts.sfEntryPoints + 1 }
              else Nothing
      | otherwise = Just facts

    hasNullByte offset end
      | offset >= end = False
      | BS.index bytes offset == 0 = True
      | BS.index bytes (offset + 1) == 0 = True
      | BS.index bytes (offset + 2) == 0 = True
      | BS.index bytes (offset + 3) == 0 = True
      | otherwise = hasNullByte (offset + 4) end

emptySpirvFacts :: SpirvFacts
emptySpirvFacts = SpirvFacts 0 0 0

opCapability :: Word16
opCapability = 17

opMemoryModel :: Word16
opMemoryModel = 14

opEntryPoint :: Word16
opEntryPoint = 15

capabilityShader :: Word32
capabilityShader = 1

addressingLogical :: Word32
addressingLogical = 0

memoryModelGLSL450 :: Word32
memoryModelGLSL450 = 1

executionModel :: ShaderStage -> Word32
executionModel stage =
  case stage of
    ShaderStageVertex -> 0
    ShaderStageFragment -> 4
    ShaderStageCompute -> 5

spirvHeaderSize :: Int
spirvHeaderSize = 5 * 4

spirvMagic :: Word32
spirvMagic = 0x07230203

word32At :: ByteString -> Int -> Word32
word32At bytes offset =
  fromIntegral (BS.index bytes offset)
    .|. (fromIntegral (BS.index bytes (offset + 1)) `shiftL` 8)
    .|. (fromIntegral (BS.index bytes (offset + 2)) `shiftL` 16)
    .|. (fromIntegral (BS.index bytes (offset + 3)) `shiftL` 24)

writeCacheEntry :: FilePath -> ByteString -> IO ()
writeCacheEntry cachePath contents = do
  let cacheDir = takeDirectory cachePath
      temporaryPrefix = takeFileName cachePath <> ".tmp"
  createDirectoryIfMissing True cacheDir
  bracketOnError
    (openBinaryTempFile cacheDir temporaryPrefix)
    cleanupTemporary
    (\(temporaryPath, handle) -> do
        BS.hPut handle contents
        hFlush handle
        hClose handle
        renameFile temporaryPath cachePath
    )
  where
    cleanupTemporary (temporaryPath, handle) = do
      ignoreIOException (hClose handle)
      ignoreIOException (removeFile temporaryPath)

ignoreIOException :: IO () -> IO ()
ignoreIOException action = do
  _ <- try action :: IO (Either IOException ())
  pure ()

writeWeslCacheWithImports :: CompileOptions -> FilePath -> Imports mods -> String -> ByteString -> ShaderInterface -> IO ()
writeWeslCacheWithImports opts rootName importSet src bytes iface = do
  let bodyLines = cacheInputLinesWithImports rootName importSet src
      key = weslCacheKeyFromLines opts bodyLines
      identity = cacheInputIdentity opts bodyLines
  writeWeslCacheEntry opts key identity bytes iface

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Runtime regression checks for public API safety boundaries.
module PublicApiRegression (checks) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import Spirdo.Wesl
  ( BindingLayout(..)
  , Option(..)
  , OverrideLayout(..)
  , OverrideType(..)
  , OverrideValue(..)
  , StorageAccess(..)
  , StorageFormat(..)
  , StorageTextureLayout(..)
  , SpirvTargetEnvironment(..)
  , compile
  , shaderBindings
  , shaderImmediateByteLength
  , shaderOverrides
  , overrideValueFromNumber
  , shaderStage
  , shaderSpirvWords
  , shaderWorkgroupSize
  , sourceNamed
  )
import qualified Spirdo.Wesl.Inputs as Inputs
import Spirdo.Wesl.Reflection
  ( Binding(..)
  , BindingKind(..)
  , SamplerBindingMode(..)
  , Scalar(..)
  , ScalarType(..)
  , Shader
  , ShaderInterface(..)
  , ShaderStage(..)
  , StageIO(..)
  , OverrideInfo(..)
  , TypeLayout(..)
  , Ty(..)
  , bindingInfoFor
  , defaultCompileOptions
  , imports
  , packUniformStorableUnchecked
  , shaderInterface
  , spirv
  , stageIO
  , validateUniformStorableUnchecked
  , wesl
  , withEntryPoint
  )

checks :: [(String, IO ())]
checks =
  [ ("public-api-source-named", checkSourceNamed)
  , ("public-api-spirv-words", checkSpirvWords)
  , ("public-api-storage-texture-layout", checkStorageTextureLayout)
  , ("public-api-entry-point", checkWithEntryPoint)
  , ("public-api-defaultless-workgroup-override", checkDefaultlessWorkgroupOverride)
  , ("public-api-numeric-override-conversion", checkNumericOverrideConversion)
  , ("public-api-unchecked-storable", checkUncheckedStorablePacking)
  , ("public-api-combined-input-boundary", checkCombinedInputBoundary)
  , ("public-api-immediate-layout", checkImmediateLayout)
  ]

checkImmediateLayout :: IO ()
checkImmediateLayout = do
  vulkanResult <- compile [] (sourceNamed "immediate-vulkan.wesl" immediateSource)
  openGlResult <-
    compile
      [OptTargetEnvironment TargetOpenGl]
      (sourceNamed "immediate-open-gl.wesl" immediateSource)
  case (vulkanResult, openGlResult) of
    (Right vulkanShader, Right openGlShader) ->
      unless
        ( shaderImmediateByteLength vulkanShader == Just 8
            && shaderImmediateByteLength openGlShader == Just 8
        ) $
        fail
          ( "immediate reflection: expected eight bytes, got "
              <> show
                ( shaderImmediateByteLength vulkanShader
                , shaderImmediateByteLength openGlShader
                )
          )
    (Left err, _) -> fail ("Vulkan immediate compilation: " <> show err)
    (_, Left err) -> fail ("OpenGL immediate compilation: " <> show err)

immediateSource :: String
immediateSource =
  unlines
    [ "struct Constants {"
    , "  ping: u32,"
    , "  rightmost_slot: u32,"
    , "}"
    , "var<immediate> constants: Constants;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let selected = constants.ping + constants.rightmost_slot;"
    , "}"
    ]

checkSourceNamed :: IO ()
checkSourceNamed = do
  result <- compile [] (sourceNamed "public-api.wesl" computeSource)
  case result of
    Left err -> fail ("sourceNamed: " <> show err)
    Right shader ->
      unless (shaderStage shader == ShaderStageCompute) $
        fail "sourceNamed: expected compute shader"

checkSpirvWords :: IO ()
checkSpirvWords = do
  result <- compile [] (sourceNamed "spirv-words.wesl" computeSource)
  case result of
    Left err -> fail ("shaderSpirvWords: " <> show err)
    Right shader ->
      case shaderSpirvWords shader of
        0x07230203 : _ -> pure ()
        spirvWords -> fail ("shaderSpirvWords: invalid SPIR-V header " <> show (take 1 spirvWords))

checkStorageTextureLayout :: IO ()
checkStorageTextureLayout = do
  result <- compile [] (sourceNamed "storage-texture-layout.wesl" storageTextureSource)
  case result of
    Left err -> fail ("storage texture reflection: " <> show err)
    Right shader ->
      unless
        ( map (\binding -> (binding.blName, binding.blStorageTexture)) (shaderBindings shader)
            == [ ("read_texture", Just (StorageTextureLayout FormatR32Float StorageRead))
               , ("write_texture", Just (StorageTextureLayout FormatRgba8Unorm StorageWrite))
               , ("array_texture", Just (StorageTextureLayout FormatRg32Uint StorageReadWrite))
               , ("volume_texture", Just (StorageTextureLayout FormatRgba16Float StorageRead))
               , ("uniform_value", Nothing)
               ]
        ) $
        fail ("storage texture reflection: unexpected layouts " <> show (shaderBindings shader))

checkWithEntryPoint :: IO ()
checkWithEntryPoint =
  let shader =
        $(spirv (withEntryPoint "second" defaultCompileOptions) imports [wesl|
@compute @workgroup_size(1)
fn first() {}

@compute @workgroup_size(2)
fn second() {}
|])
  in case stageIO (shaderInterface shader) of
      Just reflectedStage
        | reflectedStage.sioWorkgroupSize == Just (2, 1, 1) -> pure ()
      other -> fail ("withEntryPoint: expected workgroup size (2, 1, 1), got " <> show other)

checkDefaultlessWorkgroupOverride :: IO ()
checkDefaultlessWorkgroupOverride = do
  result <- compile [] (sourceNamed "defaultless-workgroup.wesl" defaultlessWorkgroupSource)
  case result of
    Left err -> fail ("defaultless workgroup runtime compile: " <> show err)
    Right shader -> do
      unless (shaderStage shader == ShaderStageCompute) $
        fail "defaultless workgroup runtime compile: expected compute stage"
      unless (shaderWorkgroupSize shader == Nothing) $
        fail ("defaultless workgroup runtime compile: expected no known workgroup default, got " <> show (shaderWorkgroupSize shader))
      unless (shaderOverrides shader == [OverrideLayout "x" (Just 7) OverrideU32]) $
        fail ("defaultless workgroup runtime compile: unexpected override reflection " <> show (shaderOverrides shader))

  let shader = $(spirv defaultCompileOptions imports (unlines
        [ "@id(7) override x: u32;"
        , "@compute @workgroup_size(x) fn main() {}"
        ]))
      iface = shaderInterface shader
  case stageIO iface of
    Just reflectedStage
      | reflectedStage.sioStage == ShaderStageCompute
      , reflectedStage.sioWorkgroupSize == Nothing -> pure ()
    other -> fail ("defaultless workgroup reflection: expected compute stage without a known default, got " <> show other)
  case iface.siOverrides of
    [override]
      | override.oiName == "x"
      , override.oiSpecId == Just 7 -> pure ()
    overrides -> fail ("defaultless workgroup reflection: expected SpecId 7, got " <> show overrides)

checkNumericOverrideConversion :: IO ()
checkNumericOverrideConversion = do
  unless (overrideValueFromNumber (OverrideLayout "count" Nothing OverrideU32) 7 == Right (OVU32 7)) $
    fail "overrideValueFromNumber: expected an exact u32 conversion"
  case overrideValueFromNumber (OverrideLayout "count" Nothing OverrideU32) 7.5 of
    Left message | "requires an integer" `isInfixOf` message -> pure ()
    outcome -> fail ("overrideValueFromNumber: expected fractional u32 rejection, got " <> show outcome)
  case overrideValueFromNumber (OverrideLayout "enabled" Nothing OverrideBool) 2 of
    Left message | "requires 0 or 1" `isInfixOf` message -> pure ()
    outcome -> fail ("overrideValueFromNumber: expected boolean rejection, got " <> show outcome)

defaultlessWorkgroupSource :: String
defaultlessWorkgroupSource = unlines
  [ "@id(7) override x: u32;"
  , "@compute @workgroup_size(x) fn main() {}"
  ]

checkUncheckedStorablePacking :: IO ()
checkUncheckedStorablePacking = do
  let layout = TLScalar F32 4 4
  case validateUniformStorableUnchecked layout (Proxy @Float) of
    Left err -> fail ("validateUniformStorableUnchecked: " <> err)
    Right () -> pure ()
  packed <- packUniformStorableUnchecked layout (1.0 :: Float)
  case packed of
    Left err -> fail ("packUniformStorableUnchecked: " <> err)
    Right bytes ->
      unless (BS.length bytes == 4) $
        fail "packUniformStorableUnchecked: expected four bytes"

-- The unchecked coercion models an untyped boundary. The public builders
-- themselves prevent this mismatch at compile time.
checkCombinedInputBoundary :: IO ()
checkCombinedInputBoundary =
  let shader =
        unsafeCoerce
          $(spirv defaultCompileOptions imports [wesl|
@group(0) @binding(0)
var tex: texture_2d<f32>;

@group(0) @binding(1)
var samp: sampler;

@fragment
fn main(@builtin(position) position: vec4<f32>) -> @location(0) vec4<f32> {
  return textureSample(tex, samp, position.xy);
}
|])
          :: Shader 'SamplerCombined TextureBindings
      separateTexture =
        Inputs.texture @"tex" (Inputs.TextureHandle 1)
          :: Inputs.InputsSeparate TextureBindings
      mismatchedInputs = unsafeCoerce separateTexture :: Inputs.InputsCombined TextureBindings
  in do
      case bindingInfoFor "tex" (shaderInterface shader) of
        Left err -> fail ("bindingInfoFor: " <> err)
        Right _ -> pure ()
      case Inputs.inputsFor shader mismatchedInputs of
        Left err
          | "texture is not supported in SamplerCombined mode" `isInfixOf` show err -> pure ()
        Left err -> fail ("combined input boundary: unexpected error: " <> show err)
        Right _ -> fail "combined input boundary: expected texture rejection"

type TextureBindings =
  '[ 'Binding "tex" 'BTexture2D 0 0 ('TTexture2D 'SF32)
   ]

computeSource :: String
computeSource =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main() {}"
    ]

storageTextureSource :: String
storageTextureSource =
  unlines
    [ "@group(0) @binding(0) var read_texture: texture_storage_1d<r32float, read>;"
    , "@group(0) @binding(1) var write_texture: texture_storage_2d<rgba8unorm, write>;"
    , "@group(0) @binding(2) var array_texture: texture_storage_2d_array<rg32uint, read_write>;"
    , "@group(0) @binding(3) var volume_texture: texture_storage_3d<rgba16float, read>;"
    , "@group(0) @binding(4) var<uniform> uniform_value: f32;"
    , "@compute @workgroup_size(1)"
    , "fn main() {}"
    ]

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
  ( OverrideLayout(..)
  , compile
  , shaderOverrides
  , shaderStage
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
  , ("public-api-entry-point", checkWithEntryPoint)
  , ("public-api-defaultless-workgroup-override", checkDefaultlessWorkgroupOverride)
  , ("public-api-unchecked-storable", checkUncheckedStorablePacking)
  , ("public-api-combined-input-boundary", checkCombinedInputBoundary)
  ]

checkSourceNamed :: IO ()
checkSourceNamed = do
  result <- compile [] (sourceNamed "public-api.wesl" computeSource)
  case result of
    Left err -> fail ("sourceNamed: " <> show err)
    Right shader ->
      unless (shaderStage shader == ShaderStageCompute) $
        fail "sourceNamed: expected compute shader"

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
      unless (shaderOverrides shader == [OverrideLayout "x" (Just 7)]) $
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

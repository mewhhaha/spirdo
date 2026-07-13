{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Regression checks for the public typed-input and binding-slot boundaries.
module InputSafetyRegression (checks) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Word (Word32)

import qualified Spirdo.Wesl.Inputs as Inputs
import Spirdo.Wesl.Reflection
  ( BindingInfo(..)
  , BindingKind(..)
  , BindingSlotCount(..)
  , Scalar(..)
  , TypeLayout(..)
  , bindingSlotCounts
  , defaultCompileOptions
  , imports
  , singleGroupBindingSlotCount
  , spirv
  , wesl
  )

checks :: [(String, IO ())]
checks =
  [ ("input-safety-empty-interface", checkEmptyInterface)
  , ("input-safety-sparse-slots", checkSparseSlots)
  , ("input-safety-maximum-binding", checkMaximumBinding)
  , ("input-safety-multiple-groups", checkMultipleGroups)
  ]

checkEmptyInterface :: IO ()
checkEmptyInterface =
  let shader =
        $(spirv defaultCompileOptions imports [wesl|
@compute @workgroup_size(1)
fn main() {}
|])
  in case Inputs.inputsFor shader mempty of
      Left err -> fail ("empty interface inputsFor failed: " <> show err)
      Right inputs -> do
        unless (null (Inputs.inputsUniforms inputs)) $
          fail "empty interface inputsFor produced uniform inputs"
        unless (null (Inputs.inputsSamplers inputs)) $
          fail "empty interface inputsFor produced sampler inputs"
        unless (null (Inputs.inputsTextures inputs)) $
          fail "empty interface inputsFor produced texture inputs"
        unless (null (Inputs.inputsStorageBuffers inputs)) $
          fail "empty interface inputsFor produced storage-buffer inputs"
        unless (null (Inputs.inputsStorageTextures inputs)) $
          fail "empty interface inputsFor produced storage-texture inputs"

checkSparseSlots :: IO ()
checkSparseSlots = do
  let bindings = [uniformBinding "zero" 3 0, uniformBinding "sparse" 3 7]
      expected = BindingSlotCount 3 8
  unless (bindingSlotCounts bindings == [expected]) $
    fail ("sparse bindings: expected " <> show [expected] <> ", got " <> show (bindingSlotCounts bindings))
  unless (singleGroupBindingSlotCount bindings == Right (Just expected)) $
    fail "sparse bindings: single-group helper returned the wrong slot count"
  unless (singleGroupBindingSlotCount [] == Right Nothing) $
    fail "empty bindings: single-group helper must preserve the absence of a group"

checkMaximumBinding :: IO ()
checkMaximumBinding = do
  let bindings = [uniformBinding "maximum" 5 (maxBound :: Word32)]
      expected = BindingSlotCount 5 4294967296
  unless (bindingSlotCounts bindings == [expected]) $
    fail ("maximum binding: expected " <> show [expected] <> ", got " <> show (bindingSlotCounts bindings))

checkMultipleGroups :: IO ()
checkMultipleGroups = do
  let bindings = [uniformBinding "first" 1 2, uniformBinding "second" 4 7]
      expected = [BindingSlotCount 1 3, BindingSlotCount 4 8]
  unless (bindingSlotCounts bindings == expected) $
    fail ("multiple groups: expected " <> show expected <> ", got " <> show (bindingSlotCounts bindings))
  case singleGroupBindingSlotCount bindings of
    Left err
      | "1, 4" `isInfixOf` err -> pure ()
      | otherwise -> fail ("multiple groups: missing group evidence in error: " <> err)
    Right result -> fail ("multiple groups: expected rejection, got " <> show result)

uniformBinding :: String -> Word32 -> Word32 -> BindingInfo
uniformBinding name group binding =
  BindingInfo
    { biName = name
    , biKind = BUniform
    , biGroup = group
    , biBinding = binding
    , biType = TLScalar F32 4 4
    }

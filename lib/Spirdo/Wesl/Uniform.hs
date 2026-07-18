-- | Uniform value types and packing helpers.
module Spirdo.Wesl.Uniform
  ( Half(..)
  , halfFromFloat
  , halfToFloat
  , V2(..)
  , V3(..)
  , V4(..)
  , M2(..)
  , M3(..)
  , M4(..)
  , M3x4(..)
  , M4x3(..)
  , ScalarValue(..)
  , UniformValue(..)
  , ToScalar(..)
  , ToUniform(..)
  , uniform
  , packUniform
  , packUniformFrom
  , validateUniformStorableUnchecked
  , packUniformStorableUnchecked
  ) where

import Spirdo.Wesl.Types.Uniform
import Spirdo.Wesl.Util (floatToHalfBits, halfBitsToFloat)

-- | Round a single-precision value to its IEEE 754 binary16 representation.
halfFromFloat :: Float -> Half
halfFromFloat = Half . floatToHalfBits

-- | Decode an IEEE 754 binary16 value as single precision.
halfToFloat :: Half -> Float
halfToFloat (Half bits) = halfBitsToFloat bits

-- | Uniform value types and packing helpers.
module Spirdo.Wesl.Uniform
  ( Half(..)
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
  , validateUniformStorable
  , packUniformStorable
  ) where

import Spirdo.Wesl.Types.Uniform

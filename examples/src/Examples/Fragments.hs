{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Fragment shader example registry (online-inspired gallery).
module Examples.Fragments
  ( fragmentGradientBloomShader
  , fragmentCirclePulseShader
  , fragmentSpectrumShiftShader
  , fragmentSineWavesShader
  , fragmentCheckerWarpShader
  , fragmentRippleCausticsShader
  , fragmentPlasmaStormShader
  , fragmentVignetteGlowShader
  , fragmentNoiseFlowShader
  , fragmentSwirlVortexShader
  , fragmentMetaballsShader
  , fragmentKaleidoscopeShader
  ) where

import Examples.Fragments.CheckerWarp (fragmentCheckerWarpShader)
import Examples.Fragments.CirclePulse (fragmentCirclePulseShader)
import Examples.Fragments.GradientBloom (fragmentGradientBloomShader)
import Examples.Fragments.Kaleidoscope (fragmentKaleidoscopeShader)
import Examples.Fragments.Metaballs (fragmentMetaballsShader)
import Examples.Fragments.NoiseFlow (fragmentNoiseFlowShader)
import Examples.Fragments.PlasmaStorm (fragmentPlasmaStormShader)
import Examples.Fragments.RippleCaustics (fragmentRippleCausticsShader)
import Examples.Fragments.SineWaves (fragmentSineWavesShader)
import Examples.Fragments.SpectrumShift (fragmentSpectrumShiftShader)
import Examples.Fragments.SwirlVortex (fragmentSwirlVortexShader)
import Examples.Fragments.VignetteGlow (fragmentVignetteGlowShader)

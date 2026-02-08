{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Inline Imports (compile-time module linking).
module Examples.Fragments.InlineImports (fragmentInlineImportsShader) where

import Spirdo.Wesl.Reflection
  ( spirv
  , (<:)
  , module_
  , imports
  , wesl
  )

fragmentInlineImportsShader =
  $(spirv
      (imports
        <: module_ @"palette"
            [wesl|
fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.5, 0.5, 0.5);
  let b = vec3(0.5, 0.5, 0.5);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.33, 0.67);
  return a + b * cos(6.28318 * (c * t + d));
}
|]
        <: module_ @"shape"
            [wesl|
fn ring(p: vec2<f32>, radius: f32, width: f32) -> f32 {
  let d = abs(length(p) - radius);
  let aa = max(fwidth(d), 0.001);
  return 1.0 - smoothstep(width - aa, width + aa, d);
}
|]
      )
      [wesl|
import palette::palette;
import shape::ring;

struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.25;

  let r = length(p);
  let rings =
    ring(p, 0.35 + 0.05 * sin(t * 2.0), 0.03)
      + ring(p, 0.65, 0.025);
  let glow = smoothstep(1.2, 0.1, r);
  let col = palette(r * 1.5 - t * 0.5);
  let pulse = 0.35 + 0.65 * (sin(t + r * 6.0) * 0.5 + 0.5);

  return vec4(col * (0.2 + rings * pulse) * glow, 1.0) * params.color;
}
|]
   )

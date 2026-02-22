{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Sine Waves.
module Examples.Fragments.SineWaves (fragmentSineWavesShader) where

import Spirdo.Wesl.Reflection (defaultCompileOptions, imports, spirv, wesl)

fragmentSineWavesShader =
      $(spirv defaultCompileOptions imports [wesl|
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
  let t = params.time_res.x;

  let w1 = sin(p.x * 4.0 + t) * 0.2;
  let w2 = sin(p.x * 3.0 - t * 1.3) * 0.25;
  let w3 = sin(p.x * 6.0 + t * 0.7) * 0.15;

  let d1 = abs(p.y - w1);
  let d2 = abs(p.y - w2);
  let d3 = abs(p.y - w3);
  let a1 = fwidth(d1);
  let a2 = fwidth(d2);
  let a3 = fwidth(d3);
  let l1 = smoothstep(0.025 + a1, 0.0 - a1, d1);
  let l2 = smoothstep(0.02 + a2, 0.0 - a2, d2);
  let l3 = smoothstep(0.018 + a3, 0.0 - a3, d3);

  let base = mix(vec3(0.02, 0.04, 0.06), vec3(0.05, 0.12, 0.18), vec3(uv.y, uv.y, uv.y));
  let wave = vec3(l1 * 0.8 + l2 * 0.5, l2 * 0.6 + l3 * 0.4, l1 * 0.2 + l3 * 0.8);
  let glow = wave * 1.4;
  let col = base + glow;

  return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|])

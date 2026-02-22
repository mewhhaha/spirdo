{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Ripple Caustics.
module Examples.Fragments.RippleCaustics (fragmentRippleCausticsShader) where

import Spirdo.Wesl.Reflection (defaultCompileOptions, imports, spirv, wesl)

fragmentRippleCausticsShader =
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
  let t = params.time_res.x * 1.2;

  let r = length(p);
  let ripple = sin(r * 25.0 - t * 4.0);
  let aa = max(fwidth(ripple), 0.002);
  let rings = smoothstep(0.0, aa, 1.0 - abs(ripple));
  let caustic = pow(rings, 3.0);

  let base = mix(vec3(0.02, 0.05, 0.1), vec3(0.08, 0.16, 0.22), vec3(uv.y, uv.y, uv.y));
  let glow = vec3(caustic * 0.6, caustic * 0.8, caustic);
  let col = base + glow;

  return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|])

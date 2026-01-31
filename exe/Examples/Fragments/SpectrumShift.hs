{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Spectrum Shift.
module Examples.Fragments.SpectrumShift (fragmentSpectrumShiftShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentSpectrumShiftShader =
      [wesl|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.55, 0.5, 0.5);
  let b = vec3(0.45, 0.45, 0.5);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.2, 0.45);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.2;

  let r = length(p);
  let angle = atan2(p.y, p.x);
  let sweep = uv.x + uv.y + 0.35 * sin(angle * 4.0 + t * 2.0) + r * 0.25;
  let col = palette(sweep + t);
  let vignette = smoothstep(1.3, 0.2, r);

  return vec4(col.x * vignette, col.y * vignette, col.z * vignette, 1.0) * params.color;
}
|]

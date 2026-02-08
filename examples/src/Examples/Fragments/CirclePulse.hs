{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Circle Pulse.
module Examples.Fragments.CirclePulse (fragmentCirclePulseShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentCirclePulseShader =
      [weslShader|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.45, 0.5, 0.55);
  let b = vec3(0.45, 0.4, 0.35);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.25, 0.5);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x;

  let r = length(p);
  let ar = fwidth(r);
  let pulse = 0.25 + 0.05 * sin(t * 2.0);
  let ring1 = smoothstep(0.012 + ar, 0.0 - ar, abs(r - pulse));
  let ring2Center = 0.51 + 0.08 * sin(t * 1.2);
  let ring2 = smoothstep(0.02 + ar, 0.0 - ar, abs(r - ring2Center));
  let ring3Center = 0.75 + 0.06 * sin(t * 0.7);
  let ring3 = smoothstep(0.018 + ar, 0.0 - ar, abs(r - ring3Center));
  let core = smoothstep(0.2 + ar, 0.0 - ar, r);

  let glow = ring1 + ring2 * 0.7 + ring3 * 0.5 + core * 0.8;
  let col = palette(r * 1.6 - t * 0.25) * glow + vec3(0.03, 0.04, 0.06);

  return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

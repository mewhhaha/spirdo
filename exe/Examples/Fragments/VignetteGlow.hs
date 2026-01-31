{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Vignette Glow.
module Examples.Fragments.VignetteGlow (fragmentVignetteGlowShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentVignetteGlowShader =
      [wesl|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
  return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

fn noise(p: vec2<f32>) -> f32 {
  let i = floor(p);
  let f = fract(p);
  let a = hash(i);
  let b = hash(i + vec2(1.0, 0.0));
  let c = hash(i + vec2(0.0, 1.0));
  let d = hash(i + vec2(1.0, 1.0));
  let u = f * f * (vec2(3.0, 3.0) - 2.0 * f);
  return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.4, 0.45, 0.5);
  let b = vec3(0.4, 0.35, 0.3);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.2, 0.4);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.25;

  let r = length(p);
  let vignette = smoothstep(1.2, 0.2, r);
  let glow = smoothstep(0.5, 0.0, length(p - vec2(0.25, 0.1)));
  let grain = noise(uv * 140.0 + vec2(t, -t)) * 0.05;

  let col = palette(uv.x + uv.y + t * 0.2) * (0.4 + glow * 0.9);
  let final = (col + vec3(grain, grain, grain)) * vignette;

  return vec4(final.x, final.y, final.z, 1.0) * params.color;
}
|]

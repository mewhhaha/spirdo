{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Gradient Bloom.
module Examples.Fragments.GradientBloom (fragmentGradientBloomShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentGradientBloomShader =
      [weslShader|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.5, 0.5, 0.5);
  let b = vec3(0.5, 0.5, 0.5);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.33, 0.67);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.15;

  let r = length(p);
  let band = uv.y + 0.15 * sin(r * 3.0 - t * 2.0);
  let col = palette(band + t);
  let glow = smoothstep(0.9, 0.0, r);
  let vignette = smoothstep(1.2, 0.4, r);
  let final = col * (0.4 + glow * 0.9) * vignette + vec3(0.02, 0.03, 0.05);

  return vec4(final.x, final.y, final.z, 1.0) * params.color;
}
|]

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Metaballs.
module Examples.Fragments.Metaballs (fragmentMetaballsShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentMetaballsShader =
      [wesl|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.35, 0.45, 0.5);
  let b = vec3(0.45, 0.35, 0.25);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.1, 0.35, 0.65);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x;

  let c1 = vec2(0.35 * sin(t * 0.7), 0.35 * cos(t * 0.9));
  let c2 = vec2(0.35 * sin(t * 1.1 + 1.2), 0.25 * cos(t * 1.3 + 2.4));
  let c3 = vec2(0.2 * sin(t * 0.5 + 3.0), 0.3 * cos(t * 0.8 + 4.0));

  let f1 = 0.4 / (length(p - c1) + 0.05);
  let f2 = 0.4 / (length(p - c2) + 0.05);
  let f3 = 0.35 / (length(p - c3) + 0.05);
  let field = f1 + f2 + f3;

  let blob = smoothstep(1.0, 1.6, field);
  let rim = smoothstep(1.4, 1.6, field) - smoothstep(1.6, 1.8, field);

  let col = palette(field * 0.15) * (0.3 + blob * 0.9) + vec3(rim * 0.6, rim * 0.3, rim * 0.2);
  let vignette = smoothstep(1.3, 0.3, length(p));

  return vec4(col.x * vignette, col.y * vignette, col.z * vignette, 1.0) * params.color;
}
|]

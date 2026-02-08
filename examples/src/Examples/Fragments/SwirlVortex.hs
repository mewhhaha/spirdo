{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Swirl Vortex.
module Examples.Fragments.SwirlVortex (fragmentSwirlVortexShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentSwirlVortexShader =
      [weslShader|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.4, 0.4, 0.45);
  let b = vec3(0.45, 0.4, 0.35);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.0, 0.3, 0.6);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.4;

  let r = length(p);
  let ang = atan2(p.y, p.x);
  let angle = ang + r * 2.0 - t;
  let swirl = sin(angle * 6.0 + r * 4.0 - t * 2.0);
  let band = smoothstep(0.2, 0.8, swirl * 0.5 + 0.5);

  let angVec = vec2(cos(ang), sin(ang));
  let angWarp = dot(angVec, vec2(0.6, 0.8));
  let col = palette(r + angWarp * 0.35 + t * 0.1) * (0.3 + band * 0.9);
  let vignette = smoothstep(1.2, 0.2, r);

  return vec4(col.x * vignette, col.y * vignette, col.z * vignette, 1.0) * params.color;
}
|]

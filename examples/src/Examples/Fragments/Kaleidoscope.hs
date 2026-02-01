{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Kaleidoscope.
module Examples.Fragments.Kaleidoscope (fragmentKaleidoscopeShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentKaleidoscopeShader =
      [wesl|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
  return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
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

fn fbm(p: vec2<f32>) -> f32 {
  var f = 0.0;
  var a = 0.5;
  var x = p;
  for (var i = 0; i < 4; i = i + 1) {
    f = f + a * noise(x);
    x = x * 2.2 + vec2(1.7, 9.2);
    a = a * 0.5;
  }
  return f;
}

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.35, 0.45, 0.55);
  let b = vec3(0.45, 0.4, 0.35);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.05, 0.35, 0.75);
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
  let sectors = 8.0;
  let sector = 6.28318 / sectors;
  var ang = atan2(p.y, p.x) + sector * 0.5;
  ang = ang - sector * floor(ang / sector);
  ang = abs(ang - sector * 0.5);
  let k = vec2(cos(ang), sin(ang)) * r;

  let n = fbm(k * 3.0 + vec2(t, -t * 0.7));
  let band = sin((k.x + k.y) * 6.0 + t * 3.0) * 0.5 + 0.5;
  let col = palette(n + band * 0.2 + r * 0.4);

  let vignette = smoothstep(1.3, 0.2, r);
  return vec4(col.x * vignette, col.y * vignette, col.z * vignette, 1.0) * params.color;
}
|]

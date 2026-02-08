{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Plasma Storm.
module Examples.Fragments.PlasmaStorm (fragmentPlasmaStormShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentPlasmaStormShader =
      [weslShader|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
  return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

fn fade(f: vec2<f32>) -> vec2<f32> {
  return f * f * f * (f * (f * 6.0 - 15.0) + 10.0);
}

fn grad(p: vec2<f32>) -> vec2<f32> {
  let a = 6.28318 * hash(p);
  return vec2(cos(a), sin(a));
}

fn rotate(p: vec2<f32>, a: f32) -> vec2<f32> {
  let c = cos(a);
  let s = sin(a);
  return vec2(p.x * c - p.y * s, p.x * s + p.y * c);
}

fn noise(p: vec2<f32>) -> f32 {
  let i = floor(p);
  let f = fract(p);
  let u = fade(f);
  let g00 = grad(i);
  let g10 = grad(i + vec2(1.0, 0.0));
  let g01 = grad(i + vec2(0.0, 1.0));
  let g11 = grad(i + vec2(1.0, 1.0));
  let n00 = dot(g00, f - vec2(0.0, 0.0));
  let n10 = dot(g10, f - vec2(1.0, 0.0));
  let n01 = dot(g01, f - vec2(0.0, 1.0));
  let n11 = dot(g11, f - vec2(1.0, 1.0));
  let nx0 = mix(n00, n10, u.x);
  let nx1 = mix(n01, n11, u.x);
  return mix(nx0, nx1, u.y) * 0.5 + 0.5;
}

fn fbm(p: vec2<f32>) -> f32 {
  var f = 0.0;
  var a = 0.55;
  var x = p;
  for (var i = 0; i < 6; i = i + 1) {
    f = f + a * noise(x);
    x = rotate(x * 2.03 + vec2(1.7, 9.2), 0.85);
    a = a * 0.5;
  }
  return f;
}

fn palette(t: f32) -> vec3<f32> {
  let a = vec3(0.35, 0.4, 0.45);
  let b = vec3(0.45, 0.45, 0.4);
  let c = vec3(1.0, 1.0, 1.0);
  let d = vec3(0.15, 0.35, 0.7);
  return a + b * cos(6.28318 * (c * t + d));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let res = vec2(params.time_res.y, params.time_res.z);
  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * (res.x / res.y), p0.y);
  let t = params.time_res.x * 0.35;

  let warp = vec2(
    fbm(p * 1.35 + vec2(t * 0.12, -t * 0.1)),
    fbm(p * 1.35 + vec2(3.2, 1.7) - t * 0.08)
  );
  let pw = p + (warp - vec2(0.5, 0.5)) * 0.55;
  let n = fbm(pw * 2.0 + vec2(t * 0.3, -t * 0.22));
  let m = fbm(pw * 2.9 - vec2(t * 0.22, t * 0.18));
  let v = sin((n + m * 0.75 + length(pw)) * 6.0 + t * 1.4);
  let col = palette(n + v * 0.16 + t * 0.08);

  let glow = smoothstep(0.9, 0.0, length(p));
  let final = col * (0.4 + glow * 0.8);

  return vec4(final.x, final.y, final.z, 1.0) * params.color;
}
|]

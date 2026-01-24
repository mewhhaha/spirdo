{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Grass (fragmentGrassShader) where

import Spirdo.Wesl (wesl)

fragmentGrassShader =
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
let u = f * f * (vec2(3.0, 3.0) - vec2(2.0 * f.x, 2.0 * f.y));
let x = mix(a, b, u.x);
let y = mix(c, d, u.x);
return mix(x, y, u.y);
}

fn bladeMask(uv: vec2<f32>, g: f32, density: f32, t: f32) -> f32 {
let x = uv.x * density;
let cell = floor(x);
let row = floor(g * 60.0);
let rnd = hash(vec2(cell, row));
let local = fract(x) - 0.5;
let height = mix(0.04, 0.28, rnd) * (0.25 + 0.75 * g);
let top = 1.0 - height;
if (g < top) {
  return 0.0;
}
let bladeY = (g - top) / height;
let width = mix(0.08, 0.28, rnd) * (1.0 - bladeY);
let sway = sin(t * 1.5 + rnd * 6.283 + g * 3.0) * 0.25 * (1.0 - bladeY);
let d = abs(local + sway) - width;
return smoothstep(0.02, 0.0, d);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let horizon = 0.45;
let t = params.time_res.x;

if (uv.y < horizon) {
  let skyT = uv.y / horizon;
  let sky = mix(vec3(0.08, 0.18, 0.35), vec3(0.5, 0.7, 0.95), vec3(skyT, skyT, skyT));
  let sunPos = vec2(0.75, 0.18);
  let sun = smoothstep(0.08, 0.0, length(uv - sunPos));
  let glow = smoothstep(0.2, 0.0, length(uv - sunPos));
  let col = sky + vec3(1.0, 0.85, 0.6) * sun * 0.8 + vec3(0.8, 0.6, 0.3) * glow * 0.2;
  return vec4(col.x, col.y, col.z, 1.0) * params.color;
}

let g = (uv.y - horizon) / (1.0 - horizon);
let wind = sin(t * 0.8 + uv.y * 6.0) * 0.002;
let uvw = vec2(uv.x + wind, uv.y);
let density = mix(120.0, 28.0, g);
let layer0 = bladeMask(uvw, g, density, t);
let layer1 = bladeMask(uvw + vec2(0.17, 0.0), g, density * 1.8, t + 1.2) * 0.6;
let blade = clamp(layer0 + layer1, 0.0, 1.0);
let base = mix(vec3(0.03, 0.11, 0.04), vec3(0.12, 0.28, 0.08), vec3(g, g, g));
let shade = noise(vec2(uv.x * 6.0, g * 3.5));
let bladeCol = mix(vec3(0.06, 0.22, 0.06), vec3(0.25, 0.45, 0.12), vec3(shade, shade, shade));
let fog = smoothstep(0.0, 0.8, 1.0 - g);
var col = mix(base, bladeCol, vec3(blade, blade, blade));
col = mix(col, vec3(0.2, 0.35, 0.2), vec3(fog, fog, fog) * 0.35);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

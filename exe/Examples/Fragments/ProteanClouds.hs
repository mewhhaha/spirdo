{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.ProteanClouds (fragmentProteanCloudsShader) where

import Spirdo.Wesl (wesl)

fragmentProteanCloudsShader =
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

fn fbm(p: vec2<f32>) -> f32 {
var q = p;
var sum = 0.0;
var amp = 0.55;
for (var i = 0; i < 5; i = i + 1) {
  sum = sum + noise(q) * amp;
  q = vec2(q.x * 2.1 + 1.7, q.y * 2.1 + 9.2);
  amp = amp * 0.5;
}
return sum;
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.1;

let swirl = vec2(sin(t * 1.2) * 0.6, cos(t * 1.1) * 0.6);
let base = fbm(vec2(p.x * 1.3 + swirl.x, p.y * 1.3 + swirl.y));
let detail = fbm(vec2(p.x * 3.2 - swirl.x * 0.7, p.y * 3.2 - swirl.y * 0.7));
let density = clamp(base + detail * 0.4, 0.0, 1.0);

let sky = mix(vec3(0.15, 0.22, 0.35), vec3(0.55, 0.75, 0.95), vec3(uv.y, uv.y, uv.y));
let cloud = mix(vec3(0.9, 0.9, 0.95), vec3(1.0, 0.95, 0.85), vec3(detail, detail, detail));
let shade = smoothstep(0.35, 0.85, density);
let col = mix(sky, cloud, vec3(shade, shade, shade));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

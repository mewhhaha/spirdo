{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Primitives (fragmentPrimitivesShader) where

import Spirdo.Wesl (wesl)

fragmentPrimitivesShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn sdSphere(p: vec3<f32>, r: f32) -> f32 {
return length(p) - r;
}

fn sdBox(p: vec3<f32>, b: vec3<f32>) -> f32 {
let q = vec3(abs(p.x), abs(p.y), abs(p.z)) - b;
let qx = max(q.x, 0.0);
let qy = max(q.y, 0.0);
let qz = max(q.z, 0.0);
let outside = length(vec3(qx, qy, qz));
let inside = min(max(q.x, max(q.y, q.z)), 0.0);
return outside + inside;
}

fn scene(p: vec3<f32>, t: f32) -> vec2<f32> {
let s = sdSphere(p - vec3(0.0, 0.2, 0.0), 0.9);
let b = sdBox(p - vec3(1.2, -0.2, 0.3), vec3(0.5, 0.6, 0.5));
let wobble = sin(t + p.x * 2.0) * 0.15;
let s2 = sdSphere(p - vec3(-1.1, wobble, -0.4), 0.6);
var d = s;
var m = 0.0;
if (b < d) {
  d = b;
  m = 1.0;
}
if (s2 < d) {
  d = s2;
  m = 2.0;
}
return vec2(d, m);
}

fn calcNormal(p: vec3<f32>, t: f32) -> vec3<f32> {
let e = 0.002;
let d = scene(p, t).x;
let nx = scene(p + vec3(e, 0.0, 0.0), t).x - d;
let ny = scene(p + vec3(0.0, e, 0.0), t).x - d;
let nz = scene(p + vec3(0.0, 0.0, e), t).x - d;
return normalize(vec3(nx, ny, nz));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.6;

let ro = vec3(0.0, 0.3, -3.0);
let rd = normalize(vec3(p.x, p.y, 1.6));
var dist = 0.0;
var hit = false;
var mat = 0.0;
for (var i = 0; i < 64; i = i + 1) {
  let pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
  let resHit = scene(pos, t);
  let d = resHit.x;
  if (d < 0.001) {
    hit = true;
    mat = resHit.y;
    break;
  }
  dist = dist + d;
  if (dist > 20.0) {
    break;
  }
}

let sky = mix(vec3(0.04, 0.06, 0.1), vec3(0.35, 0.5, 0.7), vec3(uv.y, uv.y, uv.y));
if (!hit) {
  return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
}

let pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
let n = calcNormal(pos, t);
let light = normalize(vec3(0.6, 0.7, 0.3));
let diff = max(dot(n, light), 0.0);
let view = vec3(-rd.x, -rd.y, -rd.z);
let rim = pow(1.0 - max(dot(n, view), 0.0), 2.0);
let base0 = vec3(0.2, 0.6, 1.0);
let base1 = vec3(1.0, 0.45, 0.2);
let base2 = vec3(0.6, 0.9, 0.4);
var base = base0;
if (mat > 0.5 && mat < 1.5) {
  base = base1;
} else {
  if (mat > 1.5) {
    base = base2;
  }
}
let col = base * vec3(diff, diff, diff) + vec3(rim * 0.25, rim * 0.25, rim * 0.25);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

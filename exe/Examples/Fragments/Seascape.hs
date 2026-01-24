{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Seascape.
module Examples.Fragments.Seascape (fragmentSeascapeShader) where

import Spirdo.Wesl (wesl)

fragmentSeascapeShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn seaHeight(p: vec2<f32>, t: f32) -> f32 {
var q = p;
var h = 0.0;
var freq = 0.8;
var amp = 0.6;
for (var i = 0; i < 4; i = i + 1) {
  let wave = sin(q.x * freq + t) * cos(q.y * freq - t * 0.7);
  h = h + wave * amp;
  q = vec2(q.y * 1.6, q.x * 1.6);
  freq = freq * 1.7;
  amp = amp * 0.5;
}
return h;
}

fn seaNormal(p: vec2<f32>, t: f32) -> vec3<f32> {
let eps = 0.05;
let h = seaHeight(p, t);
let hx = seaHeight(p + vec2(eps, 0.0), t);
let hz = seaHeight(p + vec2(0.0, eps), t);
let n = vec3(h - hx, eps, h - hz);
return normalize(n);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.4;

let ro = vec3(0.0, 1.3, -2.5);
let rd = normalize(vec3(p.x, p.y * 0.7 - 0.2, 1.6));

var dist = 0.0;
var pos = ro;
var hit = false;
for (var i = 0; i < 48; i = i + 1) {
  pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
  let h = seaHeight(vec2(pos.x, pos.z), t);
  let d = pos.y - h;
  if (d < 0.0) {
    hit = true;
    break;
  }
  dist = dist + clamp(d * 0.7, 0.02, 0.3);
}

let sky = mix(vec3(0.08, 0.14, 0.22), vec3(0.5, 0.7, 0.9), vec3(uv.y, uv.y, uv.y));
if (!hit) {
  return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
}

let n = seaNormal(vec2(pos.x, pos.z), t);
let light = normalize(vec3(0.6, 0.7, 0.4));
let diff = max(dot(n, light), 0.0);
let view = vec3(-rd.x, -rd.y, -rd.z);
let fres = pow(1.0 - max(dot(n, view), 0.0), 3.0);
let sea = mix(vec3(0.02, 0.12, 0.2), vec3(0.05, 0.35, 0.5), vec3(diff, diff, diff));
let col = mix(sea, sky, vec3(fres, fres, fres));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

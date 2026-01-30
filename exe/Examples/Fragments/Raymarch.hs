{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Raymarch.
module Examples.Fragments.Raymarch (fragmentRaymarchShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentRaymarchShader =
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

fn mapScene(p: vec3<f32>) -> f32 {
let wobble = sin(p.y * 1.5 + params.time_res.x) * 0.25;
let sphere = sdSphere(p - vec3(0.0, wobble, 0.0), 0.7);
let plane = p.y + 0.9;
return min(sphere, plane);
}

fn estimateNormal(p: vec3<f32>) -> vec3<f32> {
let e = 0.001;
let ex = vec3(e, 0.0, 0.0);
let ey = vec3(0.0, e, 0.0);
let ez = vec3(0.0, 0.0, e);
let nx = mapScene(p + ex) - mapScene(p - ex);
let ny = mapScene(p + ey) - mapScene(p - ey);
let nz = mapScene(p + ez) - mapScene(p - ez);
return normalize(vec3(nx, ny, nz));
}

fn raymarch(ro: vec3<f32>, rd: vec3<f32>) -> f32 {
var t = 0.0;
for (var i = 0; i < 64; i = i + 1) {
  let p = ro + rd * vec3(t, t, t);
  let d = mapScene(p);
  if (d < 0.001) {
    return t;
  }
  t = t + d;
  if (t > 20.0) {
    break;
  }
}
return -1.0;
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let ro = vec3(0.0, 0.0, -3.0);
let rd = normalize(vec3(p.x, p.y, 1.6));
let t = raymarch(ro, rd);
var col = vec3(0.02, 0.03, 0.05);
if (t > 0.0) {
  let pos = ro + rd * vec3(t, t, t);
  let n = estimateNormal(pos);
  let lightDir = normalize(vec3(0.5, 0.8, -0.2));
  let diff = max(dot(n, lightDir), 0.0);
  let rim = pow(1.0 - max(dot(n, rd), 0.0), 2.0);
  col = vec3(diff + rim * 0.4, diff * 0.6 + 0.1, diff * 0.9);
}
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Plasma.
module Examples.Fragments.Plasma (fragmentPlasmaShader) where

import Spirdo.Wesl (wesl)

fragmentPlasmaShader =
      [wesl|
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
let v = t + d.x;
let w = t + d.y;
let z = t + d.z;
return a + b * vec3(cos(6.2831 * v), cos(6.2831 * w), cos(6.2831 * z));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let t = params.time_res.x;
let v0 = sin(p.x * 8.0 + t);
let v1 = cos(p.y * 9.0 - t * 1.2);
let v2 = sin((p.x + p.y) * 6.0 + t * 0.5);
let v = (v0 + v1 + v2) * 0.6;
let w = smoothstep(-1.0, 1.0, v);
let tone = fract(v + t * 0.1);
let col = palette(tone) * vec3(w, w, w);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

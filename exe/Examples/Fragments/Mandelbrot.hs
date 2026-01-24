{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Mandelbrot (fragmentMandelbrotShader) where

import Spirdo.Wesl (wesl)

fragmentMandelbrotShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let c = vec2(p.x * 1.4 - 0.4, p.y);

var z = vec2(0.0, 0.0);
var iter = 0;
for (var i = 0; i < 80; i = i + 1) {
  let x = z.x * z.x - z.y * z.y + c.x;
  let y = 2.0 * z.x * z.y + c.y;
  z = vec2(x, y);
  if (dot(z, z) > 4.0) {
    iter = i;
    break;
  }
  iter = i;
}

let t = f32(iter) / 80.0;
let col = vec3(0.2 + 0.8 * t, 0.1 + 0.4 * t, 0.3 + 0.7 * t * t);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

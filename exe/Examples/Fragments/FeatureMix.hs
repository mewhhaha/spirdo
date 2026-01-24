{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.FeatureMix (fragmentFeatureShader) where

import Spirdo.Wesl (wesl)

fragmentFeatureShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

fn tune(x: i32) -> i32 {
let y = x + 2;
if (y > 3) {
  return y;
}
return y - 1;
}

fn pick(n: i32) -> i32 {
switch (n) {
  case 0: { return 2; }
  case 1: { return 3; }
  default: { return 4; }
}
return 4;
}

fn make(v: f32) -> vec2<f32> {
return vec2(v, v + 1.0);
}

fn sum(n: i32) -> i32 {
var acc = 0;
var i = 0;
while (i < n) {
  acc += i;
  i++;
}
return acc;
}

const_assert(tune(1) == 2);
const_assert(pick(1) == 3);
const_assert(make(1.5).y == 2.5);
const_assert(sum(4) == 6);

@group(3) @binding(0)
var<uniform> params: Params;

fn rotate2d(p: vec2<f32>, t: f32) -> vec2<f32> {
let s = sin(t);
let c = cos(t);
let m = mat2x2(c, s, -s, c);
let x = m[0].x * p.x + m[1].x * p.y;
let y = m[0].y * p.x + m[1].y * p.y;
return vec2(x, y);
}

fn shade(p: vec2<f32>, t: f32) -> vec3<f32> {
let len = length(vec2(p.x, p.y));
let dist = distance(vec2(p.x, p.y), vec2(0.0, 0.0));
let n = normalize(vec3(p.x, p.y, 1.0));
let l = normalize(vec3(0.4, 0.6, -0.2));
let diff = max(dot(n, l), 0.0);
let rim = pow(1.0 - max(dot(n, n), 0.0), 2.0);
let glow = min(sqrt(abs(p.x)), 1.0);
let base = mix(vec3(0.1, 0.2, 0.3), vec3(0.8, 0.4, 0.2), vec3(diff, diff, diff));
return base + vec3(len * 0.1 + glow * 0.2 + dist * 0.05, rim * 0.2, 0.0);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = rotate2d(p0, params.time_res.x * 0.2);
let dx = abs(dpdx(p.x));
let dy = abs(dpdy(p.y));
let w = fwidth(p.x);
let edge = clamp(dx + dy + w, 0.0, 1.0);

let weights = array(0.1, 0.3, 0.6);
let w1 = weights[1];
var acc = 0.0;
let pacc = &acc;
*pacc = *pacc + 0.0;
var i = 0;
while (i < 3) {
  acc = acc + w1;
  i = i + 1;
}
for (var j = 0; j < 2; j = j + 1) {
  acc = acc + f32(j) * 0.1;
}
loop {
  acc = acc + 0.01;
  break if (acc > 1.0);
  continuing {
    acc = acc + 0.02;
  }
}

let mode = u32(params.time_res.w);
var tint = vec3(0.0, 0.0, 0.0);
switch (mode) {
  case 0: { tint = vec3(0.1, 0.2, 0.3); fallthrough; }
  case 1, 2: { tint = tint + vec3(0.2, 0.0, 0.2); }
  default: { tint = tint + vec3(0.0, 0.15, 0.0); }
}

let col0 = shade(p, params.time_res.x);
let col = mix(col0, tint, vec3(edge, edge, edge));

if ((p.x > 1.2) || (p.y > 1.2 && !(p.x < -1.2))) {
  discard;
}

return vec4(col.x + acc * 0.1, col.y, col.z, 1.0) * params.color;
}
|]

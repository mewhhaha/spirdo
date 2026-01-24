{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Voronoi (fragmentVoronoiShader) where

import Spirdo.Wesl (wesl)

fragmentVoronoiShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash2(p: vec2<f32>) -> vec2<f32> {
let n = vec2(
  dot(p, vec2(127.1, 311.7)),
  dot(p, vec2(269.5, 183.3))
);
return fract(sin(n) * vec2(43758.5453, 43758.5453));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.2;

let g = vec2(p.x * 3.5, p.y * 3.5) + vec2(t, -t * 0.7);
let cell = floor(g);
let f = fract(g);
var minDist = 9.0;
var id = vec2(0.0, 0.0);

for (var y = -1; y <= 1; y = y + 1) {
  for (var x = -1; x <= 1; x = x + 1) {
    let neighbor = vec2(f32(x), f32(y));
    let h = hash2(cell + neighbor);
    let diff = neighbor + h - f;
    let d = dot(diff, diff);
    if (d < minDist) {
      minDist = d;
      id = h;
    }
  }
}

let dist = sqrt(minDist);
let edge = smoothstep(0.1, 0.0, dist);
let base = mix(vec3(0.08, 0.1, 0.14), vec3(id.x, id.y, 1.0 - id.x), vec3(0.6, 0.6, 0.6));
let col = base + vec3(edge, edge, edge);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

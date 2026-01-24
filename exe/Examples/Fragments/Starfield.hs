{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Starfield.
module Examples.Fragments.Starfield (fragmentStarfieldShader) where

import Spirdo.Wesl (wesl)

fragmentStarfieldShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
return fract(sin(dot(p, vec2(f32(12.9898), f32(78.233)))) * 43758.5453);
}

fn hash2(p: vec2<f32>) -> vec2<f32> {
let n = vec2(
  dot(p, vec2(f32(127.1), f32(311.7))),
  dot(p, vec2(f32(269.5), f32(183.3)))
);
return fract(sin(n) * vec2(43758.5453, 43758.5453));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let t = params.time_res.x;

let scale = 70.0;
let g = vec2(uv.x * scale, uv.y * scale);
let cell = floor(g);
let f = fract(g);
let rnd = hash(cell);
let offset = hash2(cell) - vec2(0.5, 0.5);
let d = length(f - vec2(0.5, 0.5) - vec2(offset.x * 0.35, offset.y * 0.35));
let twinkle = smoothstep(0.08, 0.0, d);
let flicker = 0.6 + 0.4 * sin(t * 2.5 + rnd * 6.283);
let star = step(0.86, rnd) * twinkle * flicker;

let dust = fract(sin(dot(vec2(uv.x * 420.0, uv.y * 420.0), vec2(f32(12.0), f32(78.0)))) * 43758.5453);
let haze = smoothstep(0.0, 1.0, uv.y);
let base = mix(vec3(0.01, 0.02, 0.05), vec3(0.06, 0.12, 0.2), vec3(haze, haze, haze));
let glow = vec3(star, star * 0.85, star * 0.7) + vec3(dust * 0.04, dust * 0.02, dust * 0.05);
let col = base + glow;
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

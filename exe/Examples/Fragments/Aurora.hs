{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Aurora.
module Examples.Fragments.Aurora (fragmentAuroraShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentAuroraShader =
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
let t = params.time_res.x * 0.35;

let w1 = sin(p.x * 2.3 + t) * 0.35 + sin(p.x * 4.7 - t * 1.1) * 0.2;
let w2 = sin(p.x * 1.5 - t * 0.6) * 0.25 + sin(p.x * 3.9 + t * 0.8) * 0.15;
let band1 = 1.0 - smoothstep(0.0, 0.08, abs(p.y - w1));
let band2 = 1.0 - smoothstep(0.0, 0.12, abs(p.y + 0.35 + w2));
let glow = clamp(band1 + band2 * 0.7, 0.0, 1.0);
let flicker = 0.5 + 0.5 * sin(t + p.x * 6.0);

let base = mix(vec3(0.02, 0.06, 0.12), vec3(0.05, 0.2, 0.4), vec3(uv.y, uv.y, uv.y));
let aurora = mix(vec3(0.1, 0.6, 0.3), vec3(0.3, 0.9, 0.6), vec3(glow, glow, glow));
let gain = glow * (0.6 + flicker * 0.5);
let gain3 = vec3(gain, gain, gain);
let col = base + aurora * gain3;
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

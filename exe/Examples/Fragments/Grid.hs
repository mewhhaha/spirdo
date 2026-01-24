{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Grid.
module Examples.Fragments.Grid (fragmentGridShader) where

import Spirdo.Wesl (wesl)

fragmentGridShader =
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
let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let scale = 12.0;
let gx = abs(sin(p.x * scale));
let gy = abs(sin(p.y * scale));
let line = min(gx, gy);
let w = fwidth(line);
let g = clamp((0.04 - line) / (w + 0.001), 0.0, 1.0);
let ix = u32(frag_coord.x);
let iy = u32(frag_coord.y);
var bits = (ix ^ iy) & u32(15);
bits = bits % u32(17);
bits = (bits << u32(1)) | (bits >> u32(3));
let jitter = f32(bits) / 63.0;
let g2 = clamp(g + jitter * 0.2, 0.0, 1.0);
let base = mix(vec3(0.05, 0.08, 0.12), vec3(0.3, 0.7, 0.9), vec3(g2, g2, g2));
return vec4(base.x, base.y, base.z, 1.0) * params.color;
}
|]

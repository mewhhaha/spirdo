{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Tunnel.
module Examples.Fragments.Tunnel (fragmentTunnelShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentTunnelShader =
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
let t = params.time_res.x * 0.6;

let r = length(p);
let ang = atan2(p.y, p.x);
let depth = 1.0 / (r + 0.2);
let swirl = ang * 4.0 + r * 6.0 - t * 2.0;
let band = sin(swirl);
let rings = smoothstep(-0.2, 0.2, band);
let pulse = 0.5 + 0.5 * sin(r * 12.0 - t * 3.0);

let c0 = 0.5 + 0.5 * sin(swirl + 0.0);
let c1 = 0.5 + 0.5 * sin(swirl + 2.1);
let c2 = 0.5 + 0.5 * sin(swirl + 4.2);
let palette = vec3(c0, c1, c2);
let glow = rings * pulse * depth;
let col = palette * vec3(glow, glow, glow) + vec3(0.02, 0.03, 0.05) * vec3(1.0 - rings, 1.0 - rings, 1.0 - rings);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

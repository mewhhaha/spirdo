{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Checker Warp.
module Examples.Fragments.CheckerWarp (fragmentCheckerWarpShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentCheckerWarpShader =
      [weslShader|
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

  let warp = vec2(sin(p.y * 3.0 + t) * 0.25, cos(p.x * 3.0 - t) * 0.25);
  let g = (p + warp) * 4.0;
  let gx = fract(g.x);
  let gy = fract(g.y);
  let ax = fwidth(g.x) * 0.5;
  let ay = fwidth(g.y) * 0.5;
  let cx = smoothstep(0.5 - ax, 0.5 + ax, gx);
  let cy = smoothstep(0.5 - ay, 0.5 + ay, gy);
  let checker = abs(cx - cy);

  let stripe = 0.5 + 0.5 * sin((p.x + p.y) * 6.0 - t * 2.0);
  let tint = mix(vec3(0.08, 0.12, 0.18), vec3(0.25, 0.18, 0.1), vec3(stripe, stripe, stripe));
  let col = mix(vec3(0.02, 0.02, 0.03), tint, vec3(checker, checker, checker));

  return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

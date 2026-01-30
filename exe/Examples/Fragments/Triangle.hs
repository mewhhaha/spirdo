{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Triangle.
module Examples.Fragments.Triangle (fragmentTriangleShader) where

import Spirdo.Wesl.Reflection (wesl)

fragmentTriangleShader =
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
let g = clamp(uv.x * 1.2, 0.0, 1.0);
let h = clamp(uv.y * 1.2, 0.0, 1.0);
let base = mix(vec3(0.1, 0.2, 0.4), vec3(0.9, 0.3, 0.2), vec3(g, h, 0.0));
return vec4(base.x, base.y, base.z, 1.0) * params.color;
}
|]

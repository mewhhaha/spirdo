{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Clouds.
module Examples.Fragments.Clouds (fragmentCloudShader) where

import Spirdo.Wesl (wesl)

fragmentCloudShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn cloudField(p: vec2<f32>, t: f32) -> f32 {
let a = abs(sin(p.x * 1.5 + t));
let b = abs(sin(p.y * 2.0 - t * 0.7));
let c = abs(sin((p.x + p.y) * 1.2 + t * 0.4));
let d = abs(sin(p.x * 3.1 - p.y * 2.7 + t * 1.1));
let e = abs(sin(p.x * 5.0 + p.y * 4.5 - t * 0.3));
let v = (a + b + c) * 0.6 + (d + e) * 0.35;
return clamp(v - 0.35, 0.0, 1.0);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p = vec2(uv.x * 3.2 - 1.6, uv.y * 2.2 - 1.1);
let t = params.time_res.x * 0.4;
let v0 = cloudField(p, t);
let v1 = cloudField(p * vec2(1.7, 1.7) + vec2(0.4, -0.3), t * 1.3) * 0.7;
let v2 = cloudField(p * vec2(2.6, 2.6) + vec2(-0.6, 0.5), t * 1.9) * 0.5;
let clouds = clamp((v0 + v1 + v2) * 2.2, 0.0, 1.0);
let fluff = smoothstep(0.1, 0.8, clouds);
let haze = clamp(uv.y * 1.0 + 0.08, 0.0, 1.0);
let sky = mix(vec3(0.04, 0.08, 0.18), vec3(0.4, 0.7, 0.95), vec3(haze, haze, haze));
let cloudCol = mix(vec3(0.86, 0.9, 0.98), vec3(1.0, 1.0, 1.0), vec3(haze, haze, haze));
let col = mix(sky, cloudCol, vec3(fluff, fluff, fluff));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

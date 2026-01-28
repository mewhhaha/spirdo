{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Volumetric Clouds (Horizon-inspired).
module Examples.Fragments.Clouds (fragmentCloudShader) where

import Spirdo.Wesl (weslc)

fragmentCloudShader =
      [weslc|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

@group(2) @binding(0)
var noiseTex3D: texture_3d<f32>;

@group(2) @binding(1)
var noiseSampler: sampler;

fn saturate(x: f32) -> f32 {
  return clamp(x, 0.0, 1.0);
}

fn hash2(p: vec2<f32>) -> vec2<f32> {
  let q = vec2(dot(p, vec2(127.1, 311.7)), dot(p, vec2(269.5, 183.3)));
  return fract(sin(q) * 43758.5453);
}

fn voronoi(p: vec2<f32>) -> vec2<f32> {
  let g = floor(p);
  let f = fract(p);
  var d1 = 10.0;
  var d2 = 10.0;
  for (var y = -1; y <= 1; y = y + 1) {
    for (var x = -1; x <= 1; x = x + 1) {
      let b = vec2(f32(x), f32(y));
      let o = hash2(g + b);
      let r = b + o - f;
      let d = dot(r, r);
      if (d < d1) {
        d2 = d1;
        d1 = d;
      } else {
        if (d < d2) {
          d2 = d;
        }
      }
    }
  }
  return vec2(d1, d2);
}

@fragment
fn main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let t = params.time_res.x;
  let res = params.time_res.yz;
  let aspect = res.x / res.y;
  let p = vec2(uv.x - 0.5, uv.y - 0.5);
  let uvn = vec2(p.x * aspect, p.y) * 0.8 + vec2(0.5, 0.5);
  let z = fract(t * 0.05);

  let uv3 = vec3(uvn.x, uvn.y, z);
  let uv3b = vec3(uvn.x * 0.97 + 0.015, uvn.y * 0.97 + 0.03, fract(z + 0.17));
  let n3a = textureSample(noiseTex3D, noiseSampler, uv3).x;
  let n3b = textureSample(noiseTex3D, noiseSampler, uv3b).x;
  let n3 = clamp(n3a * 0.7 + n3b * 0.3, 0.0, 1.0);
  let v = voronoi(uvn * 6.0 + vec2(t * 0.02, t * 0.01));
  let cell = clamp((v.y - v.x) * 2.5, 0.0, 1.0);
  let density = smoothstep(0.3, 0.7, n3) * (0.7 + 0.3 * cell);
  let sky = mix(vec3(0.08, 0.12, 0.24), vec3(0.6, 0.75, 0.95), vec3(uv.y, uv.y, uv.y));
  let cloud = mix(sky, vec3(1.0, 1.0, 1.0), vec3(density, density, density));

  return vec4(cloud.x, cloud.y, cloud.z, 1.0) * params.color;
}
|]

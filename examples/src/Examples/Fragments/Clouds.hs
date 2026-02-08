{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Volumetric Clouds (Horizon-inspired).
module Examples.Fragments.Clouds (fragmentCloudShader) where

import Spirdo.Wesl.Reflection (weslShader)

fragmentCloudShader =
      [weslShader|
struct Params {
  time_res: vec4<f32>;
  color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

@group(2) @binding(0)
var baseNoise: texture_3d<f32>;

@group(2) @binding(1)
var detailNoise: texture_3d<f32>;

@group(2) @binding(2)
var weatherTex: texture_2d<f32>;

@group(2) @binding(3)
var noiseSampler: sampler;

fn saturate(x: f32) -> f32 {
  return clamp(x, 0.0, 1.0);
}

fn hash12(p: vec2<f32>) -> f32 {
  let h = dot(p, vec2(127.1, 311.7));
  return fract(sin(h) * 43758.5453);
}

fn noiseVec(p: vec3<f32>) -> vec3<f32> {
  let n1 = textureSample(detailNoise, noiseSampler, p).x;
  let n2 = textureSample(detailNoise, noiseSampler, p + vec3(0.34, 0.11, 0.47)).x;
  let n3 = textureSample(detailNoise, noiseSampler, p + vec3(0.17, 0.57, 0.23)).x;
  return vec3(n1, n2, n3) * 2.0 - vec3(1.0, 1.0, 1.0);
}

fn curlNoise(p: vec3<f32>) -> vec3<f32> {
  let e = 0.015;
  let dx = vec3(e, 0.0, 0.0);
  let dy = vec3(0.0, e, 0.0);
  let dz = vec3(0.0, 0.0, e);
  let n1 = noiseVec(p + dy);
  let n2 = noiseVec(p - dy);
  let n3 = noiseVec(p + dz);
  let n4 = noiseVec(p - dz);
  let n5 = noiseVec(p + dx);
  let n6 = noiseVec(p - dx);
  let dndy = (n1 - n2) / (2.0 * e);
  let dndz = (n3 - n4) / (2.0 * e);
  let dndx = (n5 - n6) / (2.0 * e);
  return vec3(dndy.z - dndz.y, dndz.x - dndx.z, dndx.y - dndy.x);
}

fn cloudDensity(pos: vec3<f32>, t: f32) -> f32 {
  let baseH = 0.05;
  let topH = 0.8;
  let h = (pos.y - baseH) / (topH - baseH);
  if (h <= 0.0) {
    return 0.0;
  }
  if (h >= 1.0) {
    return 0.0;
  }

  let heightShape = smoothstep(0.0, 0.12, h) * smoothstep(1.0, 0.6, h);
  let wind = vec2(t * 0.02, t * 0.01);
  let wuv = vec2(pos.x * 0.05, pos.z * 0.05) + wind;
  let weather = textureSample(weatherTex, noiseSampler, wuv);
  let coverage = saturate(weather.x * 0.75 + 0.25);
  let cloudType = saturate(weather.y * 0.5 + 0.5);

  let warp = curlNoise(pos * 0.8 + vec3(0.0, t * 0.02, 0.0));
  let warped = pos + warp * 0.03;
  let basePos = warped * 0.45 + vec3(wind.x, 0.0, wind.y);
  let perlin = textureSample(baseNoise, noiseSampler, basePos).x * 0.5 + 0.5;
  let worley = textureSample(detailNoise, noiseSampler, basePos * 0.75 + vec3(0.1, 0.2, 0.3)).x * 0.5 + 0.5;
  let base = clamp(perlin - worley * 0.45 + 0.25, 0.0, 1.0);

  let detail = textureSample(detailNoise, noiseSampler, basePos * 2.2).x * 0.5 + 0.5;
  var density = base * heightShape;
  density = density * mix(0.6, 1.0, cloudType);
  density = density * coverage;
  density = density - (1.0 - detail) * 0.25;
  return clamp(density * 1.8, 0.0, 1.0);
}

@fragment
fn main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let res = params.time_res.yz;
  let aspect = res.x / res.y;
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * aspect, p0.y);
  let t = params.time_res.x;

  let ro = vec3(0.0, 0.12, 0.0);
  let rd = normalize(vec3(p.x, p.y + 0.2, 1.25));

  let baseH = 0.05;
  let topH = 0.8;
  let t0 = (baseH - ro.y) / rd.y;
  let t1 = (topH - ro.y) / rd.y;
  let tmin = min(t0, t1);
  let tmax = max(t0, t1);
  if (tmax <= 0.0) {
    let sky = mix(vec3(0.07, 0.12, 0.2), vec3(0.55, 0.75, 0.95), vec3(uv.y, uv.y, uv.y));
    return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
  }

  let tstart = max(tmin, 0.0);
  let tspan = max(tmax - tstart, 0.0);
  let step = tspan / 64.0;

  let sunDir = normalize(vec3(0.6, 0.4, 0.7));
  let sky = mix(vec3(0.08, 0.12, 0.22), vec3(0.6, 0.75, 0.95), vec3(uv.y, uv.y, uv.y));

  var col = vec3(0.0, 0.0, 0.0);
  var trans = 1.0;
  let jitter = (hash12(uv * res + vec2(t, t * 1.7)) - 0.5) * step;
  var tcur = tstart + jitter;
  for (var i = 0; i < 64; i = i + 1) {
    let pos = ro + rd * (tcur + step * 0.5);
    let dens = cloudDensity(pos, t);
    if (dens > 0.001) {
      var lightAccum = 0.0;
      for (var j = 0; j < 3; j = j + 1) {
        let lpos = pos + sunDir * (0.08 + f32(j) * 0.12);
        lightAccum = lightAccum + cloudDensity(lpos, t);
      }
      let light = exp(-lightAccum * 1.1);
      let phase = 0.25 + 0.75 * pow(max(dot(rd, sunDir), 0.0), 1.5);
      let powder = 1.0 - exp(-dens * 2.5);
      let sunCol = mix(vec3(0.6, 0.7, 0.8), vec3(1.0, 0.95, 0.9), vec3(phase, phase, phase));
      let alpha = dens * step * 2.2;
      col = col + trans * sunCol * light * (0.4 + 0.6 * powder) * alpha;
      trans = trans * exp(-dens * step * 1.5);
      if (trans < 0.02) {
        break;
      }
    }
    tcur = tcur + step;
  }

  let outCol = col + sky * trans;
  let mixFactor = clamp(params.time_res.w, 0.0, 0.99);
  return vec4(outCol.x, outCol.y, outCol.z, 1.0 - mixFactor) * params.color;
}
|]

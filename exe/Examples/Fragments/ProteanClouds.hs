{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: Volumetric Clouds (Protean, high turbulence).
module Examples.Fragments.ProteanClouds (fragmentProteanCloudsShader) where

import Spirdo.Wesl (wesl)

fragmentProteanCloudsShader =
      [wesl|
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

  let heightShape = smoothstep(0.0, 0.12, h) * smoothstep(1.0, 0.55, h);
  let wind = vec2(t * 0.025, t * 0.012);
  let wuv = vec2(pos.x * 0.06, pos.z * 0.06) + wind;
  let weather = textureSample(weatherTex, noiseSampler, wuv);
  let coverage = saturate(weather.x * 0.75 + 0.25);
  let cloudType = saturate(weather.y * 0.5 + 0.5);

  let warp = curlNoise(pos * 0.9 + vec3(0.0, t * 0.02, 0.0));
  let warped = pos + warp * 0.035;
  let basePos = warped * 0.55 + vec3(wind.x, 0.0, wind.y);
  let perlin = textureSample(baseNoise, noiseSampler, basePos).x * 0.5 + 0.5;
  let worley = textureSample(detailNoise, noiseSampler, basePos * 0.8 + vec3(0.1, 0.2, 0.3)).x * 0.5 + 0.5;
  let base = clamp(perlin - worley * 0.5 + 0.28, 0.0, 1.0);

  let detail = textureSample(detailNoise, noiseSampler, basePos * 2.8).x * 0.5 + 0.5;
  var density = base * heightShape;
  density = density * mix(0.5, 1.0, cloudType);
  density = density * coverage;
  density = density - (1.0 - detail) * 0.32;
  return clamp(density * 1.9, 0.0, 1.0);
}

@fragment
fn main(@location(0) uv: vec2<f32>) -> @location(0) vec4<f32> {
  let res = params.time_res.yz;
  let aspect = res.x / res.y;
  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let p = vec2(p0.x * aspect, p0.y);
  let t = params.time_res.x;

  let ro = vec3(0.0, 0.1, 0.0);
  let rd = normalize(vec3(p.x, p.y + 0.18, 1.2));

  let baseH = 0.05;
  let topH = 0.8;
  let t0 = (baseH - ro.y) / rd.y;
  let t1 = (topH - ro.y) / rd.y;
  let tmin = min(t0, t1);
  let tmax = max(t0, t1);
  if (tmax <= 0.0) {
    let sky = mix(vec3(0.06, 0.1, 0.18), vec3(0.5, 0.7, 0.95), vec3(uv.y, uv.y, uv.y));
    return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
  }

  let tstart = max(tmin, 0.0);
  let tspan = max(tmax - tstart, 0.0);
  let step = tspan / 72.0;

  let sunDir = normalize(vec3(0.55, 0.45, 0.7));
  let sky = mix(vec3(0.06, 0.1, 0.2), vec3(0.52, 0.72, 0.95), vec3(uv.y, uv.y, uv.y));

  var col = vec3(0.0, 0.0, 0.0);
  var trans = 1.0;
  let jitter = (hash12(uv * res + vec2(t, t * 1.3)) - 0.5) * step;
  var tcur = tstart + jitter;
  for (var i = 0; i < 72; i = i + 1) {
    let pos = ro + rd * (tcur + step * 0.5);
    let dens = cloudDensity(pos, t);
    if (dens > 0.001) {
      var lightAccum = 0.0;
      for (var j = 0; j < 4; j = j + 1) {
        let lpos = pos + sunDir * (0.08 + f32(j) * 0.12);
        lightAccum = lightAccum + cloudDensity(lpos, t);
      }
      let light = exp(-lightAccum * 1.15);
      let phase = 0.2 + 0.8 * pow(max(dot(rd, sunDir), 0.0), 1.7);
      let powder = 1.0 - exp(-dens * 2.8);
      let sunCol = mix(vec3(0.55, 0.65, 0.78), vec3(1.0, 0.92, 0.85), vec3(phase, phase, phase));
      let alpha = dens * step * 2.6;
      col = col + trans * sunCol * light * (0.35 + 0.65 * powder) * alpha;
      trans = trans * exp(-dens * step * 1.7);
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

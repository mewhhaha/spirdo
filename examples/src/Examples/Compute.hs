{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example compute shaders.
module Examples.Compute
  ( computeShader
  , computeParticlesShader
  ) where

import Spirdo.Wesl.Reflection (wesl)

computeShader =
      [wesl|
struct Params {
scale: f32;
color: vec4<f32>;
};

struct Data {
values: array<u32, 4>;
counter: atomic<u32>;
accum: atomic<u32>;
};

@group(0) @binding(0)
var<storage, read_write> data: Data;

@group(0) @binding(1)
var<storage> out_tex: texture_storage_2d<rgba8unorm, write>;

@group(0) @binding(2)
var<storage> in_tex: texture_storage_2d<rgba8unorm, read>;

@group(0) @binding(5)
var<uniform> params: Params;

fn ping(v: f32) -> f32 {
return v + 1.0;
}

fn ping(v: vec2<f32>) -> vec2<f32> {
return vec2(v.x + 1.0, v.y + 1.0);
}

@compute @workgroup_size(8, 8, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
let idx = gid.x;
let coord = vec2(i32(gid.x), i32(gid.y));
let src = textureLoad(in_tex, coord);
textureStore(out_tex, coord, src);

let m = mat2x2(1.0, 0.0, 0.0, 1.0);
let col = m[0];
let _m00 = m[1][0];

let arr = array(1, 2, 3, 4);
var acc = arr[0] + arr[1];

switch (idx) {
  case 0: { acc = acc + 1; fallthrough; }
  case 1, 2: { acc = acc + 2; }
  default: { acc = acc + 3; }
}

loop {
  acc = acc + 1;
  break if (acc > 8);
  continuing {
    acc = acc + 1;
  }
}

for (var i = 0; i < 4; i = i + 1) {
  acc = acc + i;
}

if (idx < 4) {
  let old = atomicAdd(data.counter, 1);
  let cur = atomicLoad(data.counter);
  atomicStore(data.accum, cur + old);
  let _m0 = atomicMax(data.accum, old);
  let _m1 = atomicMin(data.accum, 0);
  let _m2 = atomicXor(data.accum, 1);
  let _m3 = atomicOr(data.accum, 2);
  let _m4 = atomicAnd(data.accum, 3);
  let _m5 = atomicExchange(data.accum, 4);
  data.values[idx] = data.values[idx] + u32(acc);
}

let s0 = ping(params.scale);
let s1 = ping(vec2(0.2, 0.4));
let _sum = s0 + s1.x + col.y;
}
|]
computeParticlesShader =
      [wesl|
struct Params {
time: f32;
dt: f32;
bounds: vec2<f32>;
};

struct Particle {
pos: vec2<f32>;
vel: vec2<f32>;
};

struct ParticleBuf {
items: array<Particle>;
};

@group(0) @binding(0)
var<storage, read_write> particles: ParticleBuf;

@group(0) @binding(1)
var<uniform> params: Params;

@compute @workgroup_size(64, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
let idx = gid.x;
let count = arrayLength(particles.items);
if (idx >= count) {
  return;
}

var p = particles.items[idx];
let t = params.time;
let f = vec2(
  sin(f32(idx) * 0.17 + t) * 0.02,
  cos(f32(idx) * 0.23 - t) * 0.02
);
p.vel = p.vel + f;
p.pos = p.pos + vec2(p.vel.x * params.dt, p.vel.y * params.dt);

if (p.pos.x > params.bounds.x) { p.pos.x = -params.bounds.x; }
if (p.pos.x < -params.bounds.x) { p.pos.x = params.bounds.x; }
if (p.pos.y > params.bounds.y) { p.pos.y = -params.bounds.y; }
if (p.pos.y < -params.bounds.y) { p.pos.y = params.bounds.y; }

particles.items[idx] = p;
}
|]

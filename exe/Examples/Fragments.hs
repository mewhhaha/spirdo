{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments
  ( fragmentFeatureShader
  , fragmentRaymarchShader
  , fragmentTriangleShader
  , fragmentPlasmaShader
  , fragmentGridShader
  , fragmentSdfTextShader
  , fragmentCloudShader
  , fragmentBitsShader
  , fragmentAuroraShader
  , fragmentStarfieldShader
  , fragmentTunnelShader
  , fragmentVoronoiShader
  , fragmentMandelbrotShader
  , fragmentTerrainShader
  , fragmentSeascapeShader
  , fragmentProteanCloudsShader
  , fragmentPrimitivesShader
  ) where

import Spirdo.Wesl (wesl)

fragmentFeatureShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

fn tune(x: i32) -> i32 {
let y = x + 2;
if (y > 3) {
  return y;
}
return y - 1;
}

fn pick(n: i32) -> i32 {
switch (n) {
  case 0: { return 2; }
  case 1: { return 3; }
  default: { return 4; }
}
return 4;
}

fn make(v: f32) -> vec2<f32> {
return vec2(v, v + 1.0);
}

fn sum(n: i32) -> i32 {
var acc = 0;
var i = 0;
while (i < n) {
  acc += i;
  i++;
}
return acc;
}

const_assert(tune(1) == 2);
const_assert(pick(1) == 3);
const_assert(make(1.5).y == 2.5);
const_assert(sum(4) == 6);

@group(3) @binding(0)
var<uniform> params: Params;

fn rotate2d(p: vec2<f32>, t: f32) -> vec2<f32> {
let s = sin(t);
let c = cos(t);
let m = mat2x2(c, s, -s, c);
let x = m[0].x * p.x + m[1].x * p.y;
let y = m[0].y * p.x + m[1].y * p.y;
return vec2(x, y);
}

fn shade(p: vec2<f32>, t: f32) -> vec3<f32> {
let len = length(vec2(p.x, p.y));
let dist = distance(vec2(p.x, p.y), vec2(0.0, 0.0));
let n = normalize(vec3(p.x, p.y, 1.0));
let l = normalize(vec3(0.4, 0.6, -0.2));
let diff = max(dot(n, l), 0.0);
let rim = pow(1.0 - max(dot(n, n), 0.0), 2.0);
let glow = min(sqrt(abs(p.x)), 1.0);
let base = mix(vec3(0.1, 0.2, 0.3), vec3(0.8, 0.4, 0.2), vec3(diff, diff, diff));
return base + vec3(len * 0.1 + glow * 0.2 + dist * 0.05, rim * 0.2, 0.0);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = rotate2d(p0, params.time_res.x * 0.2);
let dx = abs(dpdx(p.x));
let dy = abs(dpdy(p.y));
let w = fwidth(p.x);
let edge = clamp(dx + dy + w, 0.0, 1.0);

let weights = array(0.1, 0.3, 0.6);
let w1 = weights[1];
var acc = 0.0;
let pacc = &acc;
*pacc = *pacc + 0.0;
var i = 0;
while (i < 3) {
  acc = acc + w1;
  i = i + 1;
}
for (var j = 0; j < 2; j = j + 1) {
  acc = acc + f32(j) * 0.1;
}
loop {
  acc = acc + 0.01;
  break if (acc > 1.0);
  continuing {
    acc = acc + 0.02;
  }
}

let mode = u32(params.time_res.w);
var tint = vec3(0.0, 0.0, 0.0);
switch (mode) {
  case 0: { tint = vec3(0.1, 0.2, 0.3); fallthrough; }
  case 1, 2: { tint = tint + vec3(0.2, 0.0, 0.2); }
  default: { tint = tint + vec3(0.0, 0.15, 0.0); }
}

let col0 = shade(p, params.time_res.x);
let col = mix(col0, tint, vec3(edge, edge, edge));

if ((p.x > 1.2) || (p.y > 1.2 && !(p.x < -1.2))) {
  discard;
}

return vec4(col.x + acc * 0.1, col.y, col.z, 1.0) * params.color;
}
|]
fragmentRaymarchShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn sdSphere(p: vec3<f32>, r: f32) -> f32 {
return length(p) - r;
}

fn mapScene(p: vec3<f32>) -> f32 {
let wobble = sin(p.y * 1.5 + params.time_res.x) * 0.25;
let sphere = sdSphere(p - vec3(0.0, wobble, 0.0), 0.7);
let plane = p.y + 0.9;
return min(sphere, plane);
}

fn estimateNormal(p: vec3<f32>) -> vec3<f32> {
let e = 0.001;
let ex = vec3(e, 0.0, 0.0);
let ey = vec3(0.0, e, 0.0);
let ez = vec3(0.0, 0.0, e);
let nx = mapScene(p + ex) - mapScene(p - ex);
let ny = mapScene(p + ey) - mapScene(p - ey);
let nz = mapScene(p + ez) - mapScene(p - ez);
return normalize(vec3(nx, ny, nz));
}

fn raymarch(ro: vec3<f32>, rd: vec3<f32>) -> f32 {
var t = 0.0;
for (var i = 0; i < 64; i = i + 1) {
  let p = ro + rd * vec3(t, t, t);
  let d = mapScene(p);
  if (d < 0.001) {
    return t;
  }
  t = t + d;
  if (t > 20.0) {
    break;
  }
}
return -1.0;
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let ro = vec3(0.0, 0.0, -3.0);
let rd = normalize(vec3(p.x, p.y, 1.6));
let t = raymarch(ro, rd);
var col = vec3(0.02, 0.03, 0.05);
if (t > 0.0) {
  let pos = ro + rd * vec3(t, t, t);
  let n = estimateNormal(pos);
  let lightDir = normalize(vec3(0.5, 0.8, -0.2));
  let diff = max(dot(n, lightDir), 0.0);
  let rim = pow(1.0 - max(dot(n, rd), 0.0), 2.0);
  col = vec3(diff + rim * 0.4, diff * 0.6 + 0.1, diff * 0.9);
}
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
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
fragmentPlasmaShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn palette(t: f32) -> vec3<f32> {
let a = vec3(0.5, 0.5, 0.5);
let b = vec3(0.5, 0.5, 0.5);
let c = vec3(1.0, 1.0, 1.0);
let d = vec3(0.0, 0.33, 0.67);
let v = t + d.x;
let w = t + d.y;
let z = t + d.z;
return a + b * vec3(cos(6.2831 * v), cos(6.2831 * w), cos(6.2831 * z));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let t = params.time_res.x;
let v0 = sin(p.x * 8.0 + t);
let v1 = cos(p.y * 9.0 - t * 1.2);
let v2 = sin((p.x + p.y) * 6.0 + t * 0.5);
let v = (v0 + v1 + v2) * 0.6;
let w = smoothstep(-1.0, 1.0, v);
let tone = fract(v + t * 0.1);
let col = palette(tone) * vec3(w, w, w);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
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
fragmentSdfTextShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn sdBox(p: vec2<f32>, b: vec2<f32>) -> f32 {
let d = abs(p) - b;
let d0 = max(d, vec2(0.0, 0.0));
let inside = min(max(d.x, d.y), 0.0);
return length(d0) + inside;
}

fn sdRect(p: vec2<f32>, c: vec2<f32>, b: vec2<f32>) -> f32 {
return sdBox(p - c, b);
}

fn sdLetterS(p: vec2<f32>) -> f32 {
let top = sdRect(p, vec2(0.0, 0.35), vec2(0.35, 0.05));
let mid = sdRect(p, vec2(0.0, 0.0), vec2(0.35, 0.05));
let bot = sdRect(p, vec2(0.0, -0.35), vec2(0.35, 0.05));
let left = sdRect(p, vec2(-0.3, 0.175), vec2(0.05, 0.175));
let right = sdRect(p, vec2(0.3, -0.175), vec2(0.05, 0.175));
return min(min(min(top, mid), min(bot, left)), right);
}

fn sdLetterD(p: vec2<f32>) -> f32 {
let left = sdRect(p, vec2(-0.3, 0.0), vec2(0.05, 0.4));
let right = sdRect(p, vec2(0.3, 0.0), vec2(0.05, 0.4));
let top = sdRect(p, vec2(0.0, 0.4), vec2(0.25, 0.05));
let bot = sdRect(p, vec2(0.0, -0.4), vec2(0.25, 0.05));
return min(min(left, right), min(top, bot));
}

fn sdLetterF(p: vec2<f32>) -> f32 {
let left = sdRect(p, vec2(-0.3, 0.0), vec2(0.05, 0.4));
let top = sdRect(p, vec2(0.0, 0.4), vec2(0.3, 0.05));
let mid = sdRect(p, vec2(0.0, 0.05), vec2(0.25, 0.05));
return min(left, min(top, mid));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x - 0.5, uv.y - 0.5);
let p = vec2(p0.x * 1.3, p0.y * 1.3);
let sp = vec2(p.x + 0.7, p.y);
let dp = vec2(p.x, p.y);
let fp = vec2(p.x - 0.7, p.y);
let ds = sdLetterS(sp);
let dd = sdLetterD(dp);
let df = sdLetterF(fp);
let d = min(ds, min(dd, df));
let alpha = clamp((0.08 - d) / 0.08, 0.0, 1.0);
let glow = clamp((0.18 - d) / 0.18, 0.0, 1.0);
let bg = vec3(0.02, 0.03, 0.05);
let text = mix(vec3(0.1, 0.12, 0.18), vec3(1.0, 0.96, 0.7), vec3(alpha, alpha, alpha));
let glowCol = vec3(glow * 0.7, glow * 0.5, glow * 0.3);
let mixAmt = clamp(alpha + glow * 0.35, 0.0, 1.0);
let col = mix(bg, text + glowCol, vec3(mixAmt, mixAmt, mixAmt));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
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
fragmentBitsShader =
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
let s = sign(p.x);

let ix = u32(frag_coord.x);
let iy = u32(frag_coord.y);
let bits = ix ^ (iy << u32(1));
let clz = countLeadingZeros(bits);
let ctz = countTrailingZeros(bits);
let packed = (i32(255) << i32(24)) | (i32(1) << i32(16)) | (i32(2) << i32(8)) | i32(3);
let du = dot4U8Packed(u32(packed), u32(packed));
let di = dot4I8Packed(packed, packed);
let bc = bitcast<u32>(f32(1.0));
let cnt = countOneBits(bits);
let rev = reverseBits(bits);
let ext = extractBits(rev, u32(4), u32(6));
let ins = insertBits(bits, ext, u32(8), u32(6));
let mask = f32(ins & u32(255)) / 255.0;
let leadTone = f32(clz) / 32.0;
let trailTone = f32(ctz) / 32.0;
let packedTone = f32(du & u32(255)) / 255.0;
let bcTone = f32(bc & u32(255)) / 255.0;
let signedTone = clamp(abs(f32(di)) / 512.0, 0.0, 1.0);

let cond = vec3(p.x > 0.0, p.y > 0.0, (p.x + p.y) > 0.0);
let anyHit = any(cond);
let allHit = all(cond);
let base = select(vec3(0.1, 0.2, 0.35), vec3(0.9, 0.25, 0.1), cond);
let v = abs(vec3(p.x, p.y, s));
let c = clamp(v, vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0));
let gain = select(0.35, 1.0, allHit);
let glow = select(0.15, 0.6, anyHit);
let tone = f32(cnt) / 32.0 + mask * 0.2 + leadTone * 0.15 + trailTone * 0.1 + packedTone * 0.1 + bcTone * 0.05 + signedTone * 0.1;
let col = base + c * vec3(0.3 + tone, 0.2 + tone * 0.6, 0.4 + tone * 0.4);
return vec4(col.x * gain + glow, col.y * gain, col.z * gain, 1.0) * params.color;
}
|]
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
fragmentStarfieldShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
return fract(sin(dot(p, vec2(f32(12.9898), f32(78.233)))) * 43758.5453);
}

fn hash2(p: vec2<f32>) -> vec2<f32> {
let n = vec2(
  dot(p, vec2(f32(127.1), f32(311.7))),
  dot(p, vec2(f32(269.5), f32(183.3)))
);
return fract(sin(n) * vec2(43758.5453, 43758.5453));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let t = params.time_res.x;

let scale = 70.0;
let g = vec2(uv.x * scale, uv.y * scale);
let cell = floor(g);
let f = fract(g);
let rnd = hash(cell);
let offset = hash2(cell) - vec2(0.5, 0.5);
let d = length(f - vec2(0.5, 0.5) - vec2(offset.x * 0.35, offset.y * 0.35));
let twinkle = smoothstep(0.08, 0.0, d);
let flicker = 0.6 + 0.4 * sin(t * 2.5 + rnd * 6.283);
let star = step(0.86, rnd) * twinkle * flicker;

let dust = fract(sin(dot(vec2(uv.x * 420.0, uv.y * 420.0), vec2(f32(12.0), f32(78.0)))) * 43758.5453);
let haze = smoothstep(0.0, 1.0, uv.y);
let base = mix(vec3(0.01, 0.02, 0.05), vec3(0.06, 0.12, 0.2), vec3(haze, haze, haze));
let glow = vec3(star, star * 0.85, star * 0.7) + vec3(dust * 0.04, dust * 0.02, dust * 0.05);
let col = base + glow;
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
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
fragmentVoronoiShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash2(p: vec2<f32>) -> vec2<f32> {
let n = vec2(
  dot(p, vec2(127.1, 311.7)),
  dot(p, vec2(269.5, 183.3))
);
return fract(sin(n) * vec2(43758.5453, 43758.5453));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.2;

let g = vec2(p.x * 3.5, p.y * 3.5) + vec2(t, -t * 0.7);
let cell = floor(g);
let f = fract(g);
var minDist = 9.0;
var id = vec2(0.0, 0.0);

for (var y = -1; y <= 1; y = y + 1) {
  for (var x = -1; x <= 1; x = x + 1) {
    let neighbor = vec2(f32(x), f32(y));
    let h = hash2(cell + neighbor);
    let diff = neighbor + h - f;
    let d = dot(diff, diff);
    if (d < minDist) {
      minDist = d;
      id = h;
    }
  }
}

let dist = sqrt(minDist);
let edge = smoothstep(0.1, 0.0, dist);
let base = mix(vec3(0.08, 0.1, 0.14), vec3(id.x, id.y, 1.0 - id.x), vec3(0.6, 0.6, 0.6));
let col = base + vec3(edge, edge, edge);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
fragmentMandelbrotShader =
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
let c = vec2(p.x * 1.4 - 0.4, p.y);

var z = vec2(0.0, 0.0);
var iter = 0;
for (var i = 0; i < 80; i = i + 1) {
  let x = z.x * z.x - z.y * z.y + c.x;
  let y = 2.0 * z.x * z.y + c.y;
  z = vec2(x, y);
  if (dot(z, z) > 4.0) {
    iter = i;
    break;
  }
  iter = i;
}

let t = f32(iter) / 80.0;
let col = vec3(0.2 + 0.8 * t, 0.1 + 0.4 * t, 0.3 + 0.7 * t * t);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

fragmentTerrainShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn addScaled(a: vec3<f32>, b: vec3<f32>, t: f32) -> vec3<f32> {
return vec3(a.x + b.x * t, a.y + b.y * t, a.z + b.z * t);
}

fn hash1(p: vec2<f32>) -> f32 {
let q = fract(vec2(p.x * 0.3183099, p.y * 0.3183099));
let r = vec2(q.x * 50.0, q.y * 50.0);
return fract(r.x * r.y * (r.x + r.y));
}

fn hash1f(n: f32) -> f32 {
return fract(n * 17.0 * fract(n * 0.3183099));
}

fn hash2(p: vec2<f32>) -> vec2<f32> {
let n = 111.0 * p.x + 113.0 * p.y;
let k = vec2(0.3183099, 0.3678794);
let nk = vec2(n * k.x, n * k.y);
let f = fract(nk);
return fract(vec2(n * f.x, n * f.y));
}

fn noise2(p: vec2<f32>) -> f32 {
let i = floor(p);
let f = fract(p);
let a = hash1(i);
let b = hash1(i + vec2(1.0, 0.0));
let c = hash1(i + vec2(0.0, 1.0));
let d = hash1(i + vec2(1.0, 1.0));
let ux = f.x * f.x * (3.0 - 2.0 * f.x);
let uy = f.y * f.y * (3.0 - 2.0 * f.y);
return -1.0 + 2.0 * (a + (b - a) * ux + (c - a) * uy + (a - b - c + d) * ux * uy);
}

fn noise3(p: vec3<f32>) -> f32 {
let i = floor(p);
let f = fract(p);
let ux = f.x * f.x * (3.0 - 2.0 * f.x);
let uy = f.y * f.y * (3.0 - 2.0 * f.y);
let uz = f.z * f.z * (3.0 - 2.0 * f.z);

let n = i.x + 317.0 * i.y + 157.0 * i.z;
let a = hash1f(n + 0.0);
let b = hash1f(n + 1.0);
let c = hash1f(n + 317.0);
let d = hash1f(n + 318.0);
let e = hash1f(n + 157.0);
let f1 = hash1f(n + 158.0);
let g = hash1f(n + 474.0);
let h = hash1f(n + 475.0);

let k0 = a;
let k1 = b - a;
let k2 = c - a;
let k3 = e - a;
let k4 = a - b - c + d;
let k5 = a - c - e + g;
let k6 = a - b - e + f1;
let k7 = -a + b + c - d + e - f1 - g + h;

return -1.0 + 2.0 * (k0 + k1 * ux + k2 * uy + k3 * uz + k4 * ux * uy + k5 * uy * uz + k6 * uz * ux + k7 * ux * uy * uz);
}

fn fbm2(p: vec2<f32>) -> f32 {
var q = p;
var sum = 0.0;
var amp = 0.5;
for (var i = 0; i < 6; i = i + 1) {
  sum = sum + noise2(q) * amp;
  q = vec2(q.x * 1.9 + 1.7, q.y * 1.9 + 9.2);
  amp = amp * 0.55;
}
return sum;
}

fn fbm3(p: vec3<f32>) -> f32 {
var q = p;
var sum = 0.0;
var amp = 0.5;
for (var i = 0; i < 5; i = i + 1) {
  sum = sum + noise3(q) * amp;
  q = vec3(q.x * 2.0, q.y * 2.0, q.z * 2.0);
  amp = amp * 0.55;
}
return sum;
}

fn terrainMap(p: vec2<f32>) -> vec2<f32> {
let e = fbm2(vec2(p.x * 0.0005 + 1.0, p.y * 0.0005 - 2.0));
let a = 1.0 - smoothstep(0.12, 0.13, abs(e + 0.12));
let h = 600.0 * e + 600.0;
let cliff = smoothstep(552.0, 594.0, h);
let h2 = h + 90.0 * cliff;
return vec2(h2, a);
}

fn terrainNormal(p: vec2<f32>) -> vec3<f32> {
let e = 2.0;
let h1 = terrainMap(vec2(p.x - e, p.y)).x;
let h2 = terrainMap(vec2(p.x + e, p.y)).x;
let h3 = terrainMap(vec2(p.x, p.y - e)).x;
let h4 = terrainMap(vec2(p.x, p.y + e)).x;
return normalize(vec3(h1 - h2, 2.0 * e, h3 - h4));
}

fn fog(col: vec3<f32>, t: f32) -> vec3<f32> {
let e0 = exp2(-t * 0.00025);
let e1 = exp2(-t * 0.00025 * 1.5);
let e2 = exp2(-t * 0.00025 * 4.0);
let ext = vec3(e0, e1, e2);
let inv = vec3(1.0 - ext.x, 1.0 - ext.y, 1.0 - ext.z);
return vec3(col.x * ext.x, col.y * ext.y, col.z * ext.z) + vec3(inv.x * 0.55, inv.y * 0.55, inv.z * 0.58);
}

fn terrainShadow(ro: vec3<f32>, rd: vec3<f32>) -> f32 {
var res = 1.0;
var t = 6.0;
for (var i = 0; i < 32; i = i + 1) {
  let pos = addScaled(ro, rd, t);
  let env = terrainMap(vec2(pos.x, pos.z));
  let hei = pos.y - env.x;
  res = min(res, 32.0 * hei / t);
  if (res < 0.0001 || pos.y > 840.0) {
    break;
  }
  let step = clamp(hei, 2.0 + t * 0.1, 100.0);
  t = t + step;
}
return clamp(res, 0.0, 1.0);
}

fn raymarchTerrain(ro: vec3<f32>, rd: vec3<f32>, tmin: f32, tmax: f32) -> f32 {
var t = tmin;
var ot = t;
var odis = 0.0;
for (var i = 0; i < 220; i = i + 1) {
  let th = 0.001 * t;
  let pos = addScaled(ro, rd, t);
  let env = terrainMap(vec2(pos.x, pos.z));
  let dis = pos.y - env.x;
  if (dis < th) {
    let denom = dis - odis;
    if (abs(denom) < 0.0001) {
      return t;
    }
    return ot + (th - odis) * (t - ot) / denom;
  }
  ot = t;
  odis = dis;
  let slow = 1.0 - 0.75 * env.y;
  t = t + dis * 0.8 * slow;
  if (t > tmax) {
    break;
  }
}
return -1.0;
}

fn treesMap(p: vec3<f32>) -> vec3<f32> {
let base = terrainMap(vec2(p.x, p.z)).x;
let cell = floor(vec2(p.x * 0.125, p.z * 0.125));
let f = fract(vec2(p.x * 0.125, p.z * 0.125));
let o = hash2(cell);
let offset = vec2(o.x - 0.5, o.y - 0.5);
let r = vec2((f.x + offset.x - 0.5) * 8.0, (f.y + offset.y - 0.5) * 8.0);
let height = 3.2 + 2.2 * o.x;
let width = 0.6 + 0.4 * o.y;
let q = vec3(r.x, p.y - base - height * 0.5, r.y);
let k0 = length(vec3(q.x / width, q.y / (0.5 * height), q.z / width));
let k1 = length(vec3(q.x / (width * width), q.y / ((0.5 * height) * (0.5 * height)), q.z / (width * width)));
let d = k0 * (k0 - 1.0) / k1;
let hei = clamp((p.y - base) / height, 0.0, 1.0);
let mat = o.x;
return vec3(d, hei, mat);
}

fn treesNormal(p: vec3<f32>) -> vec3<f32> {
let e = 0.6;
let d1 = treesMap(vec3(p.x + e, p.y, p.z)).x;
let d2 = treesMap(vec3(p.x - e, p.y, p.z)).x;
let d3 = treesMap(vec3(p.x, p.y + e, p.z)).x;
let d4 = treesMap(vec3(p.x, p.y - e, p.z)).x;
let d5 = treesMap(vec3(p.x, p.y, p.z + e)).x;
let d6 = treesMap(vec3(p.x, p.y, p.z - e)).x;
return normalize(vec3(d2 - d1, d4 - d3, d6 - d5));
}

fn raymarchTrees(ro: vec3<f32>, rd: vec3<f32>, tmin: f32, tmax: f32) -> f32 {
var t = tmin;
for (var i = 0; i < 80; i = i + 1) {
  let pos = addScaled(ro, rd, t);
  let tm = treesMap(pos);
  let d = tm.x;
  if (d < 0.0003 * t) {
    return t;
  }
  t = t + d;
  if (t > tmax) {
    break;
  }
}
return -1.0;
}

fn renderSky(rd: vec3<f32>) -> vec3<f32> {
let base = vec3(0.42, 0.62, 1.1);
let fall = vec3(rd.y * 0.4, rd.y * 0.4, rd.y * 0.4);
return base - fall;
}

fn renderClouds(ro: vec3<f32>, rd: vec3<f32>, tmin: f32, tmax: f32, sunDir: vec3<f32>) -> vec4<f32> {
var sumCol = vec3(0.0, 0.0, 0.0);
var sumA = 0.0;
var t = tmin;
for (var i = 0; i < 48; i = i + 1) {
  let pos = addScaled(ro, rd, t);
  let d = abs(pos.y - 900.0) - 40.0;
  let n = fbm3(vec3(pos.x * 0.0015 + params.time_res.x * 0.05, pos.y * 0.0015, pos.z * 0.0015 - params.time_res.x * 0.03));
  let den = clamp(0.25 - d * 0.01 + n * 0.2, 0.0, 0.25);
  if (den > 0.001) {
    var graY = -1.0;
    if (pos.y > 900.0) {
      graY = 1.0;
    }
    let nor = normalize(vec3(0.0, graY, 0.0));
    let dif = clamp(0.4 + 0.6 * dot(nor, sunDir), 0.0, 1.0);
    let lin = vec3(0.6 + 1.2 * dif, 0.65 + 1.0 * dif, 0.75 + 0.8 * dif);
    let base = vec3(0.85, 0.85, 0.9);
    var col = vec3(base.x * lin.x, base.y * lin.y, base.z * lin.z);
    col = fog(col, t);
    let alp = clamp(den * 0.35, 0.0, 1.0);
    sumCol = vec3(sumCol.x + col.x * alp * (1.0 - sumA), sumCol.y + col.y * alp * (1.0 - sumA), sumCol.z + col.z * alp * (1.0 - sumA));
    sumA = sumA + alp * (1.0 - sumA);
  }
  t = t + max(1.0, 0.015 * t);
  if (sumA > 0.98 || t > tmax) {
    break;
  }
}
return vec4(sumCol.x, sumCol.y, sumCol.z, sumA);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let px = vec2(frag_coord.x, frag_coord.y);
let p = vec2(
  (2.0 * px.x - res.x) / res.y,
  (2.0 * (res.y - px.y) - res.y) / res.y
);

let time = params.time_res.x;
var ro = vec3(0.0, 401.5, 6.0);
var ta = vec3(0.0, 403.5, -90.0 + ro.z);
ro = vec3(ro.x - 80.0 * sin(0.01 * time), ro.y, ro.z);
ta = vec3(ta.x - 86.0 * sin(0.01 * time), ta.y, ta.z);

let forward = normalize(vec3(ta.x - ro.x, ta.y - ro.y, ta.z - ro.z));
let right = normalize(cross(forward, vec3(0.0, 1.0, 0.0)));
let up = cross(right, forward);

let rd = normalize(vec3(
  right.x * p.x + up.x * p.y + forward.x * 1.5,
  right.y * p.x + up.y * p.y + forward.y * 1.5,
  right.z * p.x + up.z * p.y + forward.z * 1.5
));

let sunDir = normalize(vec3(-0.624695, 0.468521, -0.624695));
var col = renderSky(rd);

let sun = clamp(dot(sunDir, rd), 0.0, 1.0);
col = vec3(col.x + 0.2 * pow(sun, 32.0), col.y + 0.12 * pow(sun, 32.0), col.z + 0.08 * pow(sun, 32.0));

let tmax = 2000.0;
var resT = tmax;
var obj = 0;
let tTerrain = raymarchTerrain(ro, rd, 15.0, tmax);
if (tTerrain > 0.0) {
  resT = tTerrain;
  obj = 1;
}
let tTrees = raymarchTrees(ro, rd, 20.0, resT);
if (tTrees > 0.0) {
  resT = tTrees;
  obj = 2;
}

if (obj > 0) {
  let pos = addScaled(ro, rd, resT);
  let tnor = terrainNormal(vec2(pos.x, pos.z));
  let shaT = terrainShadow(addScaled(pos, vec3(0.0, 0.02, 0.0), 1.0), sunDir);
  if (obj == 1) {
    let n = tnor;
    let dif = clamp(dot(n, sunDir), 0.0, 1.0) * shaT;
    let dom = clamp(0.5 + 0.5 * n.y, 0.0, 1.0);
    let h = clamp((pos.y - 300.0) / 500.0, 0.0, 1.0);
    let baseLow = vec3(0.18, 0.14, 0.1);
    let baseHigh = vec3(0.45, 0.4, 0.3);
    var base = mix(baseLow, baseHigh, vec3(h, h, h));
    base = mix(base, vec3(0.12, 0.2, 0.1), vec3(dom, dom, dom));
    let ref = reflect(rd, n);
    let spe = pow(clamp(dot(ref, sunDir), 0.0, 1.0), 9.0) * (0.05 + 0.95 * pow(clamp(1.0 + dot(n, rd), 0.0, 1.0), 5.0));
    let lin = vec3(0.2 + 2.5 * dif, 0.2 + 2.2 * dif, 0.2 + 2.0 * dif);
    col = vec3(base.x * lin.x, base.y * lin.y, base.z * lin.z);
    col = vec3(col.x + spe, col.y + spe * 0.9, col.z + spe * 0.8);
  } else {
    let n = normalize(vec3(tnor.x + treesNormal(pos).x, tnor.y + treesNormal(pos).y, tnor.z + treesNormal(pos).z));
    let dif = clamp(0.1 + 0.9 * dot(n, sunDir), 0.0, 1.0) * shaT;
    let dom = clamp(0.5 + 0.5 * n.y, 0.0, 1.0);
    let mat = treesMap(pos).z;
    var base = vec3(0.2, 0.2, 0.05);
    base = mix(base, vec3(0.32, 0.2, 0.05), vec3(mat, mat, mat));
    let lin = vec3(1.8 * dif + 0.4 * dom, 1.6 * dif + 0.4 * dom, 1.3 * dif + 0.3 * dom);
    col = vec3(base.x * lin.x, base.y * lin.y, base.z * lin.z);
  }
  col = fog(col, resT);
}

let clouds = renderClouds(ro, rd, 0.0, resT, sunDir);
col = vec3(col.x * (1.0 - clouds.w) + clouds.x, col.y * (1.0 - clouds.w) + clouds.y, col.z * (1.0 - clouds.w) + clouds.z);

col = pow(
  clamp(
    vec3(col.x * 1.08 - 0.02, col.y * 1.08 - 0.02, col.z * 1.08 - 0.02),
    vec3(0.0, 0.0, 0.0),
    vec3(1.0, 1.0, 1.0)
  ),
  vec3(0.4545, 0.4545, 0.4545)
);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
fragmentSeascapeShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn seaHeight(p: vec2<f32>, t: f32) -> f32 {
var q = p;
var h = 0.0;
var freq = 0.8;
var amp = 0.6;
for (var i = 0; i < 4; i = i + 1) {
  let wave = sin(q.x * freq + t) * cos(q.y * freq - t * 0.7);
  h = h + wave * amp;
  q = vec2(q.y * 1.6, q.x * 1.6);
  freq = freq * 1.7;
  amp = amp * 0.5;
}
return h;
}

fn seaNormal(p: vec2<f32>, t: f32) -> vec3<f32> {
let eps = 0.05;
let h = seaHeight(p, t);
let hx = seaHeight(p + vec2(eps, 0.0), t);
let hz = seaHeight(p + vec2(0.0, eps), t);
let n = vec3(h - hx, eps, h - hz);
return normalize(n);
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.4;

let ro = vec3(0.0, 1.3, -2.5);
let rd = normalize(vec3(p.x, p.y * 0.7 - 0.2, 1.6));

var dist = 0.0;
var pos = ro;
var hit = false;
for (var i = 0; i < 48; i = i + 1) {
  pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
  let h = seaHeight(vec2(pos.x, pos.z), t);
  let d = pos.y - h;
  if (d < 0.0) {
    hit = true;
    break;
  }
  dist = dist + clamp(d * 0.7, 0.02, 0.3);
}

let sky = mix(vec3(0.08, 0.14, 0.22), vec3(0.5, 0.7, 0.9), vec3(uv.y, uv.y, uv.y));
if (!hit) {
  return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
}

let n = seaNormal(vec2(pos.x, pos.z), t);
let light = normalize(vec3(0.6, 0.7, 0.4));
let diff = max(dot(n, light), 0.0);
let view = vec3(-rd.x, -rd.y, -rd.z);
let fres = pow(1.0 - max(dot(n, view), 0.0), 3.0);
let sea = mix(vec3(0.02, 0.12, 0.2), vec3(0.05, 0.35, 0.5), vec3(diff, diff, diff));
let col = mix(sea, sky, vec3(fres, fres, fres));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
fragmentProteanCloudsShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn hash(p: vec2<f32>) -> f32 {
return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

fn noise(p: vec2<f32>) -> f32 {
let i = floor(p);
let f = fract(p);
let a = hash(i);
let b = hash(i + vec2(1.0, 0.0));
let c = hash(i + vec2(0.0, 1.0));
let d = hash(i + vec2(1.0, 1.0));
let u = f * f * (vec2(3.0, 3.0) - vec2(2.0 * f.x, 2.0 * f.y));
let x = mix(a, b, u.x);
let y = mix(c, d, u.x);
return mix(x, y, u.y);
}

fn fbm(p: vec2<f32>) -> f32 {
var q = p;
var sum = 0.0;
var amp = 0.55;
for (var i = 0; i < 5; i = i + 1) {
  sum = sum + noise(q) * amp;
  q = vec2(q.x * 2.1 + 1.7, q.y * 2.1 + 9.2);
  amp = amp * 0.5;
}
return sum;
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.1;

let swirl = vec2(sin(t * 1.2) * 0.6, cos(t * 1.1) * 0.6);
let base = fbm(vec2(p.x * 1.3 + swirl.x, p.y * 1.3 + swirl.y));
let detail = fbm(vec2(p.x * 3.2 - swirl.x * 0.7, p.y * 3.2 - swirl.y * 0.7));
let density = clamp(base + detail * 0.4, 0.0, 1.0);

let sky = mix(vec3(0.15, 0.22, 0.35), vec3(0.55, 0.75, 0.95), vec3(uv.y, uv.y, uv.y));
let cloud = mix(vec3(0.9, 0.9, 0.95), vec3(1.0, 0.95, 0.85), vec3(detail, detail, detail));
let shade = smoothstep(0.35, 0.85, density);
let col = mix(sky, cloud, vec3(shade, shade, shade));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]
fragmentPrimitivesShader =
      [wesl|
struct Params {
time_res: vec4<f32>;
color: vec4<f32>;
};

@group(3) @binding(0)
var<uniform> params: Params;

fn sdSphere(p: vec3<f32>, r: f32) -> f32 {
return length(p) - r;
}

fn sdBox(p: vec3<f32>, b: vec3<f32>) -> f32 {
let q = vec3(abs(p.x), abs(p.y), abs(p.z)) - b;
let qx = max(q.x, 0.0);
let qy = max(q.y, 0.0);
let qz = max(q.z, 0.0);
let outside = length(vec3(qx, qy, qz));
let inside = min(max(q.x, max(q.y, q.z)), 0.0);
return outside + inside;
}

fn scene(p: vec3<f32>, t: f32) -> vec2<f32> {
let s = sdSphere(p - vec3(0.0, 0.2, 0.0), 0.9);
let b = sdBox(p - vec3(1.2, -0.2, 0.3), vec3(0.5, 0.6, 0.5));
let wobble = sin(t + p.x * 2.0) * 0.15;
let s2 = sdSphere(p - vec3(-1.1, wobble, -0.4), 0.6);
var d = s;
var m = 0.0;
if (b < d) {
  d = b;
  m = 1.0;
}
if (s2 < d) {
  d = s2;
  m = 2.0;
}
return vec2(d, m);
}

fn calcNormal(p: vec3<f32>, t: f32) -> vec3<f32> {
let e = 0.002;
let d = scene(p, t).x;
let nx = scene(p + vec3(e, 0.0, 0.0), t).x - d;
let ny = scene(p + vec3(0.0, e, 0.0), t).x - d;
let nz = scene(p + vec3(0.0, 0.0, e), t).x - d;
return normalize(vec3(nx, ny, nz));
}

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
let res = vec2(params.time_res.y, params.time_res.z);
let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);
let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
let p = vec2(p0.x * (res.x / res.y), p0.y);
let t = params.time_res.x * 0.6;

let ro = vec3(0.0, 0.3, -3.0);
let rd = normalize(vec3(p.x, p.y, 1.6));
var dist = 0.0;
var hit = false;
var mat = 0.0;
for (var i = 0; i < 64; i = i + 1) {
  let pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
  let resHit = scene(pos, t);
  let d = resHit.x;
  if (d < 0.001) {
    hit = true;
    mat = resHit.y;
    break;
  }
  dist = dist + d;
  if (dist > 20.0) {
    break;
  }
}

let sky = mix(vec3(0.04, 0.06, 0.1), vec3(0.35, 0.5, 0.7), vec3(uv.y, uv.y, uv.y));
if (!hit) {
  return vec4(sky.x, sky.y, sky.z, 1.0) * params.color;
}

let pos = vec3(ro.x + rd.x * dist, ro.y + rd.y * dist, ro.z + rd.z * dist);
let n = calcNormal(pos, t);
let light = normalize(vec3(0.6, 0.7, 0.3));
let diff = max(dot(n, light), 0.0);
let view = vec3(-rd.x, -rd.y, -rd.z);
let rim = pow(1.0 - max(dot(n, view), 0.0), 2.0);
let base0 = vec3(0.2, 0.6, 1.0);
let base1 = vec3(1.0, 0.45, 0.2);
let base2 = vec3(0.6, 0.9, 0.4);
var base = base0;
if (mat > 0.5 && mat < 1.5) {
  base = base1;
} else {
  if (mat > 1.5) {
    base = base2;
  }
}
let col = base * vec3(diff, diff, diff) + vec3(rim * 0.25, rim * 0.25, rim * 0.25);
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|]

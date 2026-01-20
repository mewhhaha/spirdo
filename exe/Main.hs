{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Foreign
import Foreign.C.String (withCString, peekCString)
import Foreign.C.Types (CBool(..), CFloat)
import GHC.Generics (Generic)

import Spirdo.SDL3
import Spirdo.Wesl
  ( BindingDesc(..)
  , BindingInfo(..)
  , BindingKind(..)
  , CompiledShader(..)
  , CompileOptions(..)
  , ToUniform(..)
  , V2(..)
  , V4(..)
  , uniform
  , packUniform
  , OverrideValue(..)
  , ReflectBindings
  , ShaderInterface(..)
  , SomeCompiledShader(..)
  , compileWeslToSpirvWith
  , defaultCompileOptions
  , samplerBindingsFor
  , storageBufferBindingsFor
  , storageTextureBindingsFor
  , uniformBindingsFor
  , wesl
  )

data ShaderVariant = ShaderVariant
  { svName :: String
  , svSpirv :: BS.ByteString
  , svSamplerCount :: Word32
  , svStorageTextureCount :: Word32
  , svStorageBufferCount :: Word32
  , svUniformCount :: Word32
  , svUniformSlot :: Word32
  , svInterface :: ShaderInterface
  }

data VariantState = VariantState
  { vsName :: String
  , vsState :: Ptr SDL_GPURenderState
  , vsShader :: Ptr SDL_GPUShader
  , vsUniformCount :: Word32
  , vsUniformSlot :: Word32
  , vsInterface :: ShaderInterface
  , vsUniformInfos :: [BindingInfo]
  }

main :: IO ()
main = do
  let fragmentFeatureShader =
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
  let fragmentRaymarchShader =
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
  let fragmentTriangleShader =
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
  let fragmentPlasmaShader =
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
  let fragmentGridShader =
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
  let fragmentSdfTextShader =
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
  let fragmentCloudShader =
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
  let fragmentBitsShader =
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
  let fragmentAuroraShader =
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
  let fragmentStarfieldShader =
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
  let fragmentTunnelShader =
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
  let fragmentVoronoiShader =
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
  let fragmentMandelbrotShader =
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
  let fragmentOverrideSrc =
        unlines
          [ "struct Params {"
          , "  time_res: vec4<f32>;"
          , "  color: vec4<f32>;"
          , "};"
          , "override mode: i32;"
          , "@group(3) @binding(0)"
          , "var<uniform> params: Params;"
          , "fn stripes(p: vec2<f32>, t: f32) -> vec3<f32> {"
          , "  let v = sin(p.x * 10.0 + p.y * 6.0 + t * 1.5);"
          , "  let band = smoothstep(-0.2, 0.2, v);"
          , "  return mix(vec3(0.1, 0.2, 0.6), vec3(0.85, 0.6, 0.2), vec3(band, band, band));"
          , "}"
          , "fn rings(p: vec2<f32>, t: f32) -> vec3<f32> {"
          , "  let r = length(p);"
          , "  let wave = sin(r * 12.0 - t * 2.0);"
          , "  let glow = smoothstep(0.35, 0.0, abs(wave));"
          , "  return mix(vec3(0.02, 0.04, 0.08), vec3(0.4, 0.85, 0.6), vec3(glow, glow, glow));"
          , "}"
          , "@fragment"
          , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
          , "  let res = vec2(params.time_res.y, params.time_res.z);"
          , "  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);"
          , "  let p0 = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);"
          , "  let p = vec2(p0.x * (res.x / res.y), p0.y);"
          , "  let t = params.time_res.x;"
          , "  var col = stripes(p, t);"
          , "  if (mode == 1) {"
          , "    col = rings(p, t);"
          , "  }"
          , "  return vec4(col.x, col.y, col.z, 1.0) * params.color;"
          , "}"
          ]
  let overrideVariant label value =
        case compileWeslToSpirvWith (defaultCompileOptions { overrideValues = [("mode", OVI32 value)] }) fragmentOverrideSrc of
          Left err -> error ("override variant " <> label <> ": " <> show err)
          Right shader -> mkVariantDynamic label shader
  let computeShader =
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
  let computeParticlesShader =
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
  let computeTilesShader =
        [wesl|
struct Params {
  time: f32;
  res: vec2<f32>;
};

@group(0) @binding(0)
var<storage> out_tex: texture_storage_2d<rgba8unorm, write>;

@group(0) @binding(1)
var<uniform> params: Params;

@compute @workgroup_size(8, 8, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let coord = vec2(i32(gid.x), i32(gid.y));
  let uv = vec2(f32(gid.x) / params.res.x, f32(gid.y) / params.res.y);
  let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);
  let t = params.time;
  let waves = sin(p.x * 12.0 + t) * cos(p.y * 10.0 - t);
  let glow = smoothstep(0.4, 0.0, abs(waves));
  let col = vec3(0.15, 0.3, 0.55) + vec3(glow, glow * 0.6, glow * 0.8);
  textureStore(out_tex, coord, vec4(col.x, col.y, col.z, 1.0));
}
|]
  let vertexShader =
        [wesl|
struct VsOut {
  @builtin(position) position: vec4<f32>;
  @location(0) uv: vec2<f32>;
};

@vertex
fn main(@location(0) in_pos: vec2<f32>, @location(1) in_uv: vec2<f32>) -> VsOut {
  let pos = vec4(in_pos.x, in_pos.y, 0.0, 1.0);
  return VsOut(pos, in_uv);
}
|]
  let vertexWaveShader =
        [wesl|
struct Params {
  time: f32;
  amp: f32;
};

struct VsOut {
  @builtin(position) position: vec4<f32>;
  @location(0) uv: vec2<f32>;
};

@group(0) @binding(0)
var<uniform> params: Params;

@vertex
fn main(@location(0) in_pos: vec2<f32>, @location(1) in_uv: vec2<f32>) -> VsOut {
  let wave = sin(in_pos.x * 0.01 + params.time) * params.amp;
  let pos = vec4(in_pos.x, in_pos.y + wave, 0.0, 1.0);
  return VsOut(pos, in_uv);
}
|]
  let vertexFullscreenShader =
        [wesl|
struct VsOut {
  @builtin(position) position: vec4<f32>;
  @location(0) uv: vec2<f32>;
};

@vertex
fn main(@builtin(vertex_index) idx: u32) -> VsOut {
  let pos = array(
    vec2(-1.0, -1.0),
    vec2(3.0, -1.0),
    vec2(-1.0, 3.0)
  );
  let uv = array(
    vec2(0.0, 0.0),
    vec2(2.0, 0.0),
    vec2(0.0, 2.0)
  );
  let i = i32(idx);
  let p = pos[i];
  let t = uv[i];
  return VsOut(vec4(p.x, p.y, 0.0, 1.0), t);
}
|]

  let variants =
        [ mkVariant "Feature Mix" fragmentFeatureShader
        , mkVariant "Raymarch" fragmentRaymarchShader
        , mkVariant "Triangle" fragmentTriangleShader
        , mkVariant "Plasma" fragmentPlasmaShader
        , mkVariant "Grid" fragmentGridShader
        , mkVariant "SDF Text" fragmentSdfTextShader
        , mkVariant "Clouds" fragmentCloudShader
        , mkVariant "Bits" fragmentBitsShader
        , mkVariant "Aurora" fragmentAuroraShader
        , mkVariant "Starfield" fragmentStarfieldShader
        , mkVariant "Tunnel" fragmentTunnelShader
        , mkVariant "Voronoi" fragmentVoronoiShader
        , mkVariant "Mandelbrot" fragmentMandelbrotShader
        , overrideVariant "Override Stripes" 0
        , overrideVariant "Override Rings" 1
        ]
  let computeShaders =
        [ SomeCompiledShader computeShader
        , SomeCompiledShader computeParticlesShader
        , SomeCompiledShader computeTilesShader
        ]
  let vertexShaders =
        [ SomeCompiledShader vertexShader
        , SomeCompiledShader vertexWaveShader
        , SomeCompiledShader vertexFullscreenShader
        ]
  forM_ (zip [0 :: Int ..] variants) $ \(ix, variant) ->
    BS.writeFile ("fragment-" <> show ix <> ".spv") (svSpirv variant)
  forM_ (zip [0 :: Int ..] computeShaders) $ \(ix, SomeCompiledShader shader) ->
    let suffix = if ix == 0 then "" else "-" <> show ix
    in BS.writeFile ("compute" <> suffix <> ".spv") (shaderSpirv shader)
  forM_ (zip [0 :: Int ..] vertexShaders) $ \(ix, SomeCompiledShader shader) ->
    let suffix = if ix == 0 then "" else "-" <> show ix
    in BS.writeFile ("vertex" <> suffix <> ".spv") (shaderSpirv shader)

  sdlCheckBool "sdlInit" (sdlInit sdl_INIT_VIDEO)

  withCString "Spirdo SDL3" $ \title -> do
    window <- sdlCreateWindow title 800 600 0
    when (window == nullPtr) (sdlFail "sdlCreateWindow")

    renderer <- sdlCreateGPURenderer nullPtr window
    when (renderer == nullPtr) (sdlFail "sdlCreateGPURenderer")

    device <- sdlGetGPURendererDevice renderer
    when (device == nullPtr) (sdlFail "sdlGetGPURendererDevice")

    formats <- sdlGetGPUShaderFormats device
    when ((formats .&. sdl_GPU_SHADERFORMAT_SPIRV) == 0) (sdlFail "SPIR-V not supported by SDL GPU renderer")

    variantStates <- mapM (createVariantState device renderer) variants
    when (null variantStates) (sdlFail "no shaders available")

    let color = SDL_FColor 0.15 0.55 1.0 1.0
    loop renderer variantStates 0 color 0

    mapM_ (destroyVariantState device) variantStates
    sdlDestroyRenderer renderer
    sdlDestroyWindow window

  sdlQuit

loop :: Ptr SDL_Renderer -> [VariantState] -> Int -> SDL_FColor -> CFloat -> IO ()
loop renderer variants idx color t = do
  (quit, delta) <- pollInput
  let count = length variants
      idx' = if count == 0 then 0 else (idx + delta + count) `mod` count
  unless quit $ do
    when (idx' /= idx) $
      putStrLn ("shader: " <> vsName (variants !! idx'))
    renderFrame renderer (variants !! idx') color t idx'
    sdlDelay 16
    loop renderer variants idx' color (t + 0.016)

renderFrame :: Ptr SDL_Renderer -> VariantState -> SDL_FColor -> CFloat -> Int -> IO ()
renderFrame renderer variant color t mode = do
  sdlCheckBool "sdlSetRenderDrawColor" (sdlSetRenderDrawColor renderer 8 8 8 255)
  sdlCheckBool "sdlRenderClear" (sdlRenderClear renderer)

  let SDL_FColor cr cg cb ca = color
      params =
        ParamsU
          { time_res = V4 (realToFrac t) 800 600 (fromIntegral mode)
          , color = V4 (realToFrac cr) (realToFrac cg) (realToFrac cb) (realToFrac ca)
          }
      globals =
        GlobalsU
          { time = realToFrac t
          , resolution = V2 800 600
          , frame = fromIntegral mode
          }
      material =
        MaterialU
          { baseColor = V4 (realToFrac cr) (realToFrac cg) (realToFrac cb) 1.0
          , roughness = 0.35
          }
      uniformValues =
        Map.fromList
          [ ("params", uniform params)
          , ("globals", uniform globals)
          , ("material", uniform material)
          ]
  forM_ (vsUniformInfos variant) $ \info ->
    case Map.lookup (biName info) uniformValues of
      Nothing -> error ("no uniform values provided for binding: " <> biName info)
      Just uval ->
        case packUniform (biType info) uval of
          Left err -> error ("uniform pack failed: " <> err)
          Right bytes ->
            BS.useAsCStringLen bytes $ \(paramPtr, len) ->
              sdlCheckBool "sdlSetGPURenderStateFragmentUniforms" $
                sdlSetGPURenderStateFragmentUniforms (vsState variant) (biBinding info) (castPtr paramPtr) (fromIntegral len)
  sdlCheckBool "sdlSetGPURenderState" (sdlSetGPURenderState renderer (vsState variant))

  let positions :: [CFloat]
      positions =
        [ 0, 0
        , 800, 0
        , 800, 600
        , 0, 0
        , 800, 600
        , 0, 600
        ]
      uvs :: [CFloat]
      uvs =
        [ 0, 0
        , 1, 0
        , 1, 1
        , 0, 0
        , 1, 1
        , 0, 1
        ]
      colors :: [SDL_FColor]
      colors =
        [ SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        , SDL_FColor 1 1 1 1
        ]
      strideXY = fromIntegral (2 * sizeOf (undefined :: CFloat))
      strideColor = fromIntegral (sizeOf (undefined :: SDL_FColor))
      strideUV = strideXY
  withArray positions $ \posPtr ->
    withArray colors $ \colorPtr ->
      withArray uvs $ \uvPtr ->
        sdlCheckBool "sdlRenderGeometryRaw" $
          sdlRenderGeometryRaw renderer nullPtr posPtr strideXY colorPtr strideColor uvPtr strideUV 6 nullPtr 0 0

  sdlCheckBool "sdlRenderPresent" (sdlRenderPresent renderer)

pollInput :: IO (Bool, Int)
pollInput = allocaBytes sdlEventSize $ \eventPtr -> go eventPtr False 0
  where
    go ptr quit delta = do
      has <- sdlPollEvent ptr
      if not (fromCBool has)
        then pure (quit, delta)
        else do
          evType <- peek (castPtr ptr :: Ptr Word32)
          if evType == sdl_EVENT_QUIT
            then go ptr True delta
            else if evType == sdl_EVENT_KEY_DOWN
              then do
                scancode <- peekByteOff ptr sdlKeyboardEventScancodeOffset :: IO Word32
                repeatFlag <- peekByteOff ptr sdlKeyboardEventRepeatOffset :: IO Word8
                let delta' =
                      if repeatFlag /= 0
                        then delta
                        else case scancode of
                          _ | scancode == sdl_SCANCODE_LEFT -> delta - 1
                          _ | scancode == sdl_SCANCODE_RIGHT -> delta + 1
                          _ -> delta
                go ptr quit delta'
              else go ptr quit delta

createShader :: Ptr SDL_GPUDevice -> SDL_GPUShaderStage -> Word32 -> Word32 -> Word32 -> Word32 -> BS.ByteString -> IO (Ptr SDL_GPUShader)
createShader device stage samplerCount storageTextureCount storageBufferCount uniformCount bytes =
  BS.useAsCStringLen bytes $ \(ptr, len) ->
    withCString "main" $ \entry -> do
      let info = SDL_GPUShaderCreateInfo
            { shaderCodeSize = fromIntegral len
            , shaderCode = castPtr ptr
            , shaderEntrypoint = entry
            , shaderFormat = sdl_GPU_SHADERFORMAT_SPIRV
            , shaderStage = stage
            , shaderNumSamplers = samplerCount
            , shaderNumStorageTextures = storageTextureCount
            , shaderNumStorageBuffers = storageBufferCount
            , shaderNumUniformBuffers = uniformCount
            , shaderProps = 0
            }
      with info $ \infoPtr -> do
        shader <- sdlCreateGPUShader device infoPtr
        when (shader == nullPtr) (sdlFail "sdlCreateGPUShader")
        pure shader

createRenderState :: Ptr SDL_Renderer -> Ptr SDL_GPUShader -> IO (Ptr SDL_GPURenderState)
createRenderState renderer frag = do
  let info = SDL_GPURenderStateCreateInfo
        { rsFragmentShader = frag
        , rsNumSamplerBindings = 0
        , rsSamplerBindings = nullPtr
        , rsNumStorageTextures = 0
        , rsStorageTextures = nullPtr
        , rsNumStorageBuffers = 0
        , rsStorageBuffers = nullPtr
        , rsProps = 0
        }
  with info $ \infoPtr -> do
    state <- sdlCreateGPURenderState renderer infoPtr
    when (state == nullPtr) (sdlFail "sdlCreateGPURenderState")
    pure state

fragmentUniformCount :: ReflectBindings iface => CompiledShader iface -> Word32
fragmentUniformCount shader = case uniformBindingsFor shader of
  bindings -> bindingCount bindings

fragmentUniformSlot :: ReflectBindings iface => CompiledShader iface -> Word32
fragmentUniformSlot shader = case uniformBindingsFor shader of
  (b:_) -> descBinding b
  [] -> 0

bindingCount :: [BindingDesc] -> Word32
bindingCount [] = 0
bindingCount bindings = 1 + maximum (map descBinding bindings)

mkVariant :: ReflectBindings iface => String -> CompiledShader iface -> ShaderVariant
mkVariant name shader =
  let samplerCount = bindingCount (samplerBindingsFor shader)
      storageBufferCount = bindingCount (storageBufferBindingsFor shader)
      storageTextureCount = bindingCount (storageTextureBindingsFor shader)
      uniformCount = fragmentUniformCount shader
      uniformSlot = fragmentUniformSlot shader
  in ShaderVariant name (shaderSpirv shader) samplerCount storageTextureCount storageBufferCount uniformCount uniformSlot (shaderInterface shader)

mkVariantDynamic :: String -> SomeCompiledShader -> ShaderVariant
mkVariantDynamic name (SomeCompiledShader shader) =
  let iface = shaderInterface shader
      infos = siBindings iface
      samplerCount = bindingCountInfo isSamplerKind infos
      storageBufferCount = bindingCountInfo isStorageBufferKind infos
      storageTextureCount = bindingCountInfo isStorageTextureKind infos
      uniformCount = bindingCountInfo (== BUniform) infos
      uniformSlot =
        case [biBinding info | info <- infos, biKind info == BUniform] of
          (b:_) -> b
          [] -> 0
  in ShaderVariant name (shaderSpirv shader) samplerCount storageTextureCount storageBufferCount uniformCount uniformSlot iface
  where
    bindingCountInfo predKind xs =
      let bindings = [biBinding info | info <- xs, predKind (biKind info)]
      in if null bindings then 0 else 1 + maximum bindings

    isSamplerKind k =
      k `elem`
        [ BSampler
        , BSamplerComparison
        , BTexture1D
        , BTexture1DArray
        , BTexture2D
        , BTexture2DArray
        , BTexture3D
        , BTextureCube
        , BTextureCubeArray
        , BTextureMultisampled2D
        , BTextureDepth2D
        , BTextureDepth2DArray
        , BTextureDepthCube
        , BTextureDepthCubeArray
        , BTextureDepthMultisampled2D
        ]

    isStorageBufferKind k = k == BStorageRead || k == BStorageReadWrite

    isStorageTextureKind k =
      k `elem` [BStorageTexture1D, BStorageTexture2D, BStorageTexture2DArray, BStorageTexture3D]

createVariantState :: Ptr SDL_GPUDevice -> Ptr SDL_Renderer -> ShaderVariant -> IO VariantState
createVariantState device renderer variant = do
  frag <- createShader device sdl_GPU_SHADERSTAGE_FRAGMENT
    (svSamplerCount variant)
    (svStorageTextureCount variant)
    (svStorageBufferCount variant)
    (svUniformCount variant)
    (svSpirv variant)
  state <- createRenderState renderer frag
  let uniformInfos =
        [ info
        | info <- siBindings (svInterface variant)
        , biKind info == BUniform
        ]
  pure VariantState
    { vsName = svName variant
    , vsState = state
    , vsShader = frag
    , vsUniformCount = svUniformCount variant
    , vsUniformSlot = svUniformSlot variant
    , vsInterface = svInterface variant
    , vsUniformInfos = uniformInfos
    }

destroyVariantState :: Ptr SDL_GPUDevice -> VariantState -> IO ()
destroyVariantState device variant = do
  sdlDestroyGPURenderState (vsState variant)
  sdlReleaseGPUShader device (vsShader variant)

sdlCheckBool :: String -> IO CBool -> IO ()
sdlCheckBool label action = do
  ok <- action
  unless (fromCBool ok) (sdlFail label)

sdlFail :: String -> IO a
sdlFail label = do
  err <- sdlGetError >>= peekCString
  error (label <> ": " <> err)

fromCBool :: CBool -> Bool
fromCBool (CBool 0) = False
fromCBool _ = True

data ParamsU = ParamsU
  { time_res :: V4 Float
  , color :: V4 Float
  } deriving (Eq, Show, Generic)

instance ToUniform ParamsU

data GlobalsU = GlobalsU
  { time :: Float
  , resolution :: V2 Float
  , frame :: Float
  } deriving (Eq, Show, Generic)

instance ToUniform GlobalsU

data MaterialU = MaterialU
  { baseColor :: V4 Float
  , roughness :: Float
  } deriving (Eq, Show, Generic)

instance ToUniform MaterialU

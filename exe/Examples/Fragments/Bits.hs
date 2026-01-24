{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Bits (fragmentBitsShader) where

import Spirdo.Wesl (wesl)

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

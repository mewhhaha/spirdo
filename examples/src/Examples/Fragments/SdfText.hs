{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example fragment shader: SdfText.
module Examples.Fragments.SdfText (fragmentSdfTextShader) where

import Spirdo.Wesl.Reflection (defaultCompileOptions, imports, spirv, wesl)

fragmentSdfTextShader =
      $(spirv defaultCompileOptions imports [wesl|
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
let w = fwidth(d);
let alpha = smoothstep(0.08 + w, 0.08 - w, d);
let glow = smoothstep(0.18 + w, 0.0 - w, d);
let bg = vec3(0.02, 0.03, 0.05);
let text = mix(vec3(0.1, 0.12, 0.18), vec3(1.0, 0.96, 0.7), vec3(alpha, alpha, alpha));
let glowCol = vec3(glow * 0.7, glow * 0.5, glow * 0.3);
let mixAmt = clamp(alpha + glow * 0.35, 0.0, 1.0);
let col = mix(bg, text + glowCol, vec3(mixAmt, mixAmt, mixAmt));
return vec4(col.x, col.y, col.z, 1.0) * params.color;
}
|])

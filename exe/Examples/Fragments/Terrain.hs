{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Examples.Fragments.Terrain (fragmentTerrainShader) where

import Spirdo.Wesl (wesl)

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

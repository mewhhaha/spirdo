-- | Inline WESL fixtures shared by the executable test suite.
module Spirdo.Test.Fixtures where

malformedHexLiteralShader :: String
malformedHexLiteralShader =
  unlines
    [ "let bad = 0x;"
    , "let bad2 = 0x__;"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = bad + bad2;"
    , "}"
    ]

badStructFieldShader :: String
badStructFieldShader =
  unlines
    [ "struct Params {"
    , "  a: i32"
    , "  b: i32;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {}"
    ]

nonIfStatementAttrShader :: String
nonIfStatementAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  @group(0) let x = i32(1);"
    , "}"
    ]

nonIfSwitchCaseAttrShader :: String
nonIfSwitchCaseAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  switch (gid.x) {"
    , "    @location(0) case 0: {}"
    , "  }"
    , "}"
    ]

nonIfLoopAttrShader :: String
nonIfLoopAttrShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  loop {"
    , "    @id(0) break;"
    , "  }"
    , "}"
    ]

negativeI32RangeShader :: String
negativeI32RangeShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _: i32 = -2147483648;"
    , "}"
    ]

computeWithoutWorkgroupSizeShader :: String
computeWithoutWorkgroupSizeShader =
  unlines
    [ "@compute"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid;"
    , "}"
    ]

workgroupOverrideShader :: String
workgroupOverrideShader =
  unlines
    [ "override scale: i32 = 1;"
    , "@compute @workgroup_size(scale)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {}"
    ]

moduleConstDeclShader :: String
moduleConstDeclShader =
  unlines
    [ "const SCALE: i32 = 1;"
    , "@compute @workgroup_size(1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = SCALE + i32(gid.x);"
    , "}"
    ]

invalidMatrixDimensionsShader :: String
invalidMatrixDimensionsShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  let m = mat5x5<f32>(1.0);"
    , "  return vec4<f32>(m[0][0]);"
    , "}"
    ]

computeShader :: String
computeShader =
  unlines
    [ "let scale = 2.0;"
    , "struct Data {"
    , "  values: array<u32, 4>;"
    , "  counter: atomic<u32>;"
    , "  accum: atomic<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "fn bump(v: f32) -> f32 {"
    , "  return v * v;"
    , "}"
    , "fn bump(v: vec2<f32>) -> vec2<f32> {"
    , "  return vec2(v.x * v.x, v.y * v.y);"
    , "}"
    , "@compute @workgroup_size(8, 8, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let idx = gid.x;"
    , "  let _tmp = scale;"
    , "  let v = vec4(1.0, 2.0, 3.0, 4.0);"
    , "  let sw = v.zw;"
    , "  let m = mat2x2(vec2(1.0, 0.0), vec2(0.0, 1.0));"
    , "  let col = m[0];"
    , "  let s = col.y;"
    , "  let arr = array(1, 2, 3, 4);"
    , "  var sum = arr[0] + arr[1];"
    , "  var j = 0;"
    , "  while (j < 4) {"
    , "    j = j + 1;"
    , "    if (j == 2) {"
    , "      continue;"
    , "    }"
    , "    sum = sum + j;"
    , "  }"
    , "  var k = 0;"
    , "  for (var i = 0; i < 3; i = i + 1) {"
    , "    k = k + i;"
    , "  }"
    , "  let ok = (idx == 0) || (idx != 1 && !(idx > 2));"
    , "  if (ok) {"
    , "    let v0 = abs(-1.0);"
    , "    let v1 = clamp(v0, 0.0, 1.0);"
    , "    let v2 = mix(v1, 1.0, 0.25);"
    , "    let v3 = max(v2, sqrt(4.0));"
    , "    let v4 = min(v3, 2.0);"
    , "    let v5 = cos(v4) + sin(v4);"
    , "    let v6 = pow(v5, 2.0);"
    , "    let v7 = dot(vec3(v6, v6, v6), normalize(vec3(1.0, 2.0, 3.0)));"
    , "    let _len = length(vec2(sw.x, sw.y));"
    , "    let _cast = u32(v7);"
    , "    let _mat = m[1];"
    , "    let _scalar = m[1][0];"
    , "    let _b0 = bump(v4);"
    , "    let _b1 = bump(vec2(0.5, 1.5));"
    , "    let _s0 = s + f32(k);"
    , "  }"
    , "  if (idx < 4) {"
    , "    let old = atomicAdd(data.counter, 1);"
    , "    let cur = atomicLoad(data.counter);"
    , "    atomicStore(data.accum, cur + 1);"
    , "    let _m0 = atomicMax(data.accum, old);"
    , "    let _m1 = atomicMin(data.accum, 0);"
    , "    let _m2 = atomicXor(data.accum, 1);"
    , "    let _m3 = atomicOr(data.accum, 2);"
    , "    let _m4 = atomicAnd(data.accum, 3);"
    , "    let _m5 = atomicExchange(data.accum, 4);"
    , "    data.values[idx] = data.values[idx] + u32(sum);"
    , "  }"
    , "}"
    ]

barrierShader :: String
barrierShader =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(@builtin(local_invocation_index) li: u32) {"
    , "  if (li == 0u) {"
    , "    workgroupBarrier();"
    , "    storageBarrier();"
    , "    textureBarrier();"
    , "  }"
    , "}"
    ]

textureBarrierFragmentShader :: String
textureBarrierFragmentShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  textureBarrier();"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

atomicCompareExchangeShader :: String
atomicCompareExchangeShader =
  unlines
    [ "struct Data {"
    , "  value: atomic<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let r = atomicCompareExchangeWeak(data.value, 0u, 1u);"
    , "  if (r.exchanged) {"
    , "    atomicStore(data.value, r.old_value);"
    , "  }"
    , "}"
    ]

typedCtorShader :: String
typedCtorShader =
  unlines
    [ "enable f16;"
    , "struct Z {"
    , "  a: f32;"
    , "  b: vec2<f32>;"
    , "};"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let a = vec4<f32>(1.0);"
    , "  let b = vec2<f32>(0.5);"
    , "  let c = vec3<f32>(a.x, b.x, b.y);"
    , "  let d = vec3<f32>(b, 1.0);"
    , "  let sf: vec2f = vec2f(1.0);"
    , "  let si = vec2i(1i, 2i);"
    , "  let su = vec3u(1u, 2u, 3u);"
    , "  let sh = vec2h(1.0h);"
    , "  let m = mat2x2<f32>(1.0, 0.0, 0.0, 1.0);"
    , "  let m2 = mat2x2<f32>(1.0);"
    , "  let z0 = vec4<f32>();"
    , "  let z1 = mat2x2<f32>();"
    , "  let arr = array<vec2<f32>, 2>(vec2<f32>(0.25, 0.5), vec2<f32>(0.75, 1.0));"
    , "  let zarr = array<vec2<f32>, 2>();"
    , "  let zstructs = array<Z, 2>();"
    , "  let zi = i32();"
    , "  let zf = f32();"
    , "  let zs = Z();"
    , "  let _ = m[0][0] + m2[1][1] + sf.x + f32(si.x) + f32(su.x) + f32(sh.x) + arr[1].y + z0.x + z1[0][0] + zarr[1].y + zstructs[1].a + f32(zi) + zf + zs.a + zs.b.x;"
    , "  return vec4(d.x, d.y, d.z, a.w);"
    , "}"
    ]

sampleMaskShader :: String
sampleMaskShader =
  unlines
    [ "struct FragOut {"
    , "  @location(0) color: vec4<f32>;"
    , "  @builtin(sample_mask) mask: u32;"
    , "};"
    , "@fragment"
    , "fn main(@builtin(sample_mask) inMask: u32) -> FragOut {"
    , "  return FragOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), inMask);"
    , "}"
    ]

fragmentShader :: String
fragmentShader =
  unlines
    [ "struct Params {"
    , "  time_res: vec4<f32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let res = vec2(params.time_res.y, params.time_res.z);"
    , "  let uv = vec2(frag_coord.x / res.x, frag_coord.y / res.y);"
    , "  let dx = dpdx(uv.x);"
    , "  let dy = dpdy(uv.y);"
    , "  let w = fwidth(uv.x);"
    , "  let edge = clamp(abs(dx) + abs(dy), 0.0, 1.0);"
    , "  let t0 = fract(uv.x * 3.0 + params.time_res.x * 0.1);"
    , "  let glow = smoothstep(0.2, 0.8, t0);"
    , "  let dist = distance(uv, vec2(0.5, 0.5));"
    , "  let refl = reflect(normalize(vec3(uv.x - 0.5, uv.y - 0.5, 1.0)), normalize(vec3(0.2, 0.8, 0.6)));"
    , "  let tint = mix(vec3(0.2, 0.4, 0.8), vec3(1.0, 0.2, 0.1), vec3(edge, edge, edge));"
    , "  return vec4(tint.x + w + glow * 0.1 + dist * 0.2 + refl.x * 0.05, tint.y, tint.z, 1.0);"
    , "}"
    ]

vertexIoAttrShader :: String
vertexIoAttrShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) @invariant pos: vec4<f32>;"
    , "  @location(0) @interpolate(flat) id: u32;"
    , "  @location(1) @interpolate(linear, centroid) uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  let x = f32(idx & 1u);"
    , "  return VsOut(vec4<f32>(x, 0.0, 0.0, 1.0), idx, vec2<f32>(x, 0.0));"
    , "}"
    ]

vertexShader :: String
vertexShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) position: vec4<f32>;"
    , "  @location(0) uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@location(0) in_pos: vec2<f32>) -> VsOut {"
    , "  let pos = vec4(in_pos.x, in_pos.y, 0.0, 1.0);"
    , "  return VsOut(pos, in_pos);"
    , "}"
    ]

fragmentBlendSrcShader :: String
fragmentBlendSrcShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(0) @blend_src(0) c0: vec4<f32>;"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcNoEnableShader :: String
fragmentBlendSrcNoEnableShader =
  unlines
    [ "struct BlendOut {"
    , "  @location(0) @blend_src(0) c0: vec4<f32>;"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcOnlyOneShader :: String
fragmentBlendSrcOnlyOneShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(0) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

fragmentBlendSrcLocationOneShader :: String
fragmentBlendSrcLocationOneShader =
  unlines
    [ "enable dual_source_blending;"
    , "struct BlendOut {"
    , "  @location(1) @blend_src(0) c0: vec4<f32>;"
    , "  @location(1) @blend_src(1) c1: vec4<f32>;"
    , "};"
    , "@fragment"
    , "fn main() -> BlendOut {"
    , "  return BlendOut(vec4<f32>(1.0, 0.0, 0.0, 1.0), vec4<f32>(0.0, 1.0, 0.0, 1.0));"
    , "}"
    ]

vertexInterpolateIntegerShader :: String
vertexInterpolateIntegerShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) @interpolate(linear) id: u32;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  return VsOut(vec4<f32>(0.0, 0.0, 0.0, 1.0), idx);"
    , "}"
    ]

vertexInvariantLocationShader :: String
vertexInvariantLocationShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) @invariant uv: vec2<f32>;"
    , "};"
    , "@vertex"
    , "fn main(@builtin(vertex_index) idx: u32) -> VsOut {"
    , "  let x = f32(idx & 1u);"
    , "  return VsOut(vec4<f32>(x, 0.0, 0.0, 1.0), vec2<f32>(x, 0.0));"
    , "}"
    ]

vertexBoolLocationShader :: String
vertexBoolLocationShader =
  unlines
    [ "struct VsOut {"
    , "  @builtin(position) pos: vec4<f32>;"
    , "  @location(0) b: bool;"
    , "};"
    , "@vertex"
    , "fn main() -> VsOut {"
    , "  return VsOut(vec4<f32>(0.0, 0.0, 0.0, 1.0), true);"
    , "}"
    ]

f16NoEnableShader :: String
f16NoEnableShader =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaTextureBarrierFragment :: String
nagaTextureBarrierFragment =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  textureBarrier();"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaStorageWriteBuffer :: String
nagaStorageWriteBuffer =
  unlines
    [ "struct Data { v: u32, }"
    , "@group(0) @binding(0)"
    , "var<storage, write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  data.v = 1u;"
    , "}"
    ]

nagaBlendSrcEnabled :: String
nagaBlendSrcEnabled =
  unlines
    [ "enable dual_source_blending;"
    , "struct Out {"
    , "  @location(0) @blend_src(0) a: vec4<f32>,"
    , "  @location(0) @blend_src(1) b: vec4<f32>,"
    , "}"
    , "@fragment"
    , "fn main() -> Out {"
    , "  return Out(vec4<f32>(1.0), vec4<f32>(0.0));"
    , "}"
    ]

nagaBlendSrcNoEnable :: String
nagaBlendSrcNoEnable =
  unlines
    [ "struct Out {"
    , "  @location(0) @blend_src(0) a: vec4<f32>,"
    , "  @location(0) @blend_src(1) b: vec4<f32>,"
    , "}"
    , "@fragment"
    , "fn main() -> Out {"
    , "  return Out(vec4<f32>(1.0), vec4<f32>(0.0));"
    , "}"
    ]

nagaF16NoEnable :: String
nagaF16NoEnable =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaF16Enable :: String
nagaF16Enable =
  unlines
    [ "enable f16;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f16> {"
    , "  return vec4<f16>(1.0h);"
    , "}"
    ]

nagaInterpolateIntLinear :: String
nagaInterpolateIntLinear =
  unlines
    [ "struct Out {"
    , "  @builtin(position) pos: vec4<f32>,"
    , "  @location(0) @interpolate(linear) id: u32,"
    , "}"
    , "@vertex"
    , "fn main(@builtin(vertex_index) i: u32) -> Out {"
    , "  return Out(vec4<f32>(0.0, 0.0, 0.0, 1.0), i);"
    , "}"
    ]

nagaInterpolateFloatLinear :: String
nagaInterpolateFloatLinear =
  unlines
    [ "struct Out {"
    , "  @builtin(position) pos: vec4<f32>,"
    , "  @location(0) @interpolate(linear, centroid) uv: vec2<f32>,"
    , "}"
    , "@vertex"
    , "fn main(@builtin(vertex_index) i: u32) -> Out {"
    , "  let x = f32(i & 1u);"
    , "  return Out(vec4<f32>(0.0, 0.0, 0.0, 1.0), vec2<f32>(x, 0.0));"
    , "}"
    ]

nagaEntryPointerParam :: String
nagaEntryPointerParam =
  unlines
    [ "@compute @workgroup_size(1)"
    , "fn main(p: ptr<function, i32>) {"
    , "}"
    ]

nagaFragmentReturnNoBinding :: String
nagaFragmentReturnNoBinding =
  unlines
    [ "@fragment"
    , "fn main() -> vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaNonEntryParamIoAttrs :: String
nagaNonEntryParamIoAttrs =
  unlines
    [ "fn helper(@location(0) x: f32) -> f32 {"
    , "  return x;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(helper(1.0));"
    , "}"
    ]

nagaNegativeLocation :: String
nagaNegativeLocation =
  unlines
    [ "@vertex"
    , "fn main(@location(-1) x: vec4<f32>) -> @builtin(position) vec4<f32> {"
    , "  return x;"
    , "}"
    ]

nagaLocationTooManyArgs :: String
nagaLocationTooManyArgs =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0, 1) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateLocationReturn :: String
nagaDuplicateLocationReturn =
  unlines
    [ "@fragment"
    , "fn main() -> @location(0) @location(1) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateBuiltinReturn :: String
nagaDuplicateBuiltinReturn =
  unlines
    [ "@vertex"
    , "fn main() -> @builtin(position) @builtin(position) vec4<f32> {"
    , "  return vec4<f32>(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

nagaDuplicateGroupAttr :: String
nagaDuplicateGroupAttr =
  unlines
    [ "struct U {"
    , "  v: vec4<f32>,"
    , "}"
    , "@group(0) @group(1) @binding(0)"
    , "var<uniform> u: U;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return u.v;"
    , "}"
    ]

nagaDuplicateBindingAttr :: String
nagaDuplicateBindingAttr =
  unlines
    [ "struct U {"
    , "  v: vec4<f32>,"
    , "}"
    , "@group(0) @binding(0) @binding(1)"
    , "var<uniform> u: U;"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return u.v;"
    , "}"
    ]

nagaDuplicateFragmentStageAttr :: String
nagaDuplicateFragmentStageAttr =
  unlines
    [ "@fragment @fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaDuplicateWorkgroupSizeAttr :: String
nagaDuplicateWorkgroupSizeAttr =
  unlines
    [ "@compute @workgroup_size(1) @workgroup_size(2)"
    , "fn main() {"
    , "}"
    ]

nagaStageAttrWithArgs :: String
nagaStageAttrWithArgs =
  unlines
    [ "@fragment(1)"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(1.0);"
    , "}"
    ]

nagaNonEntryFunctionAttr :: String
nagaNonEntryFunctionAttr =
  unlines
    [ "@workgroup_size(1)"
    , "fn helper() -> i32 {"
    , "  return 1;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  return vec4<f32>(f32(helper()));"
    , "}"
    ]

pointerParamWorkgroupShader :: String
pointerParamWorkgroupShader =
  unlines
    [ "var<workgroup> g: i32;"
    , "fn bump(p: ptr<workgroup, i32>) -> i32 {"
    , "  *p = *p + 1;"
    , "  return *p;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let y = bump(&g);"
    , "  if (y == 0) { }"
    , "}"
    ]

pointerParamStorageShader :: String
pointerParamStorageShader =
  unlines
    [ "struct Data { v: u32, }"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "fn bump(p: ptr<storage, u32, read_write>) -> u32 {"
    , "  *p = *p + 1u;"
    , "  return *p;"
    , "}"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  let y = bump(&data.v);"
    , "  if (y == 0u) { }"
    , "}"
    ]

storageTextureShader :: String
storageTextureShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var<storage> out_tex: texture_storage_2d<rgba8unorm, write>;"
    , "@group(0) @binding(1)"
    , "var<storage> in_tex: texture_storage_2d<rgba8unorm, read>;"
    , "@compute @workgroup_size(8, 8, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let x = i32(gid.x);"
    , "  let y = i32(gid.y);"
    , "  let coord = vec2(x, y);"
    , "  let src = textureLoad(in_tex, coord);"
    , "  textureStore(out_tex, coord, vec4(src.x, src.y, src.z, src.w));"
    , "}"
    ]

storageWriteAccessShader :: String
storageWriteAccessShader =
  unlines
    [ "struct Data {"
    , "  values: array<u32, 4>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, write> data: Data;"
    , "@compute @workgroup_size(1)"
    , "fn main() {"
    , "  data.values[0] = 1u;"
    , "}"
    ]

samplerShader :: String
samplerShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var samp: sampler;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  return textureSample(tex, samp, uv);"
    , "}"
    ]

samplerValueShader :: String
samplerValueShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var samp: sampler;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = samp;"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let c = textureSample(tex, samp, uv);"
    , "  return c;"
    , "}"
    ]

bitwiseShader :: String
bitwiseShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var a = u32(1);"
    , "  var b = u32(3);"
    , "  a = a % u32(2);"
    , "  a = a << u32(1);"
    , "  a = a >> u32(1);"
    , "  a = a & b;"
    , "  a = a | b;"
    , "  a = a ^ b;"
    , "  a += b;"
    , "  a -= b;"
    , "  a *= b;"
    , "  a /= b;"
    , "  a %= b;"
    , "  a &= b;"
    , "  a |= b;"
    , "  a ^= b;"
    , "  a <<= b;"
    , "  a >>= b;"
    , "  a++;"
    , "  --a;"
    , "  var c = i32(4);"
    , "  c = c % i32(3);"
    , "  c <<= i32(1);"
    , "  c >>= i32(1);"
    , "  c++;"
    , "  --c;"
    , "  if (gid.x == 0) {"
    , "    a = a + u32(c);"
    , "  }"
    , "}"
    ]

builtinExtraShader :: String
builtinExtraShader =
  unlines
    [ "struct Params {"
    , "  time: f32;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let p = vec2(uv.x * 2.0 - 1.0, uv.y * 2.0 - 1.0);"
    , "  let a = sign(p.x);"
    , "  let ang = radians(params.time);"
    , "  let _t = tan(ang);"
    , "  let _rn = round(p.x * 2.0);"
    , "  let _tr = trunc(p.y * 2.0);"
    , "  let _inv = inverseSqrt(abs(p.x) + 0.25);"
    , "  let _fm = fma(p.x, p.y, 0.5);"
    , "  let _ch = cosh(p.x);"
    , "  let _sh = sinh(p.y);"
    , "  let _at = atan2(p.y, p.x);"
    , "  let _rt = saturate(p.x * 1.5);"
    , "  let b0 = p.x > 0.0;"
    , "  let b1 = p.y > 0.0;"
    , "  let b2 = (p.x + p.y) > 0.0;"
    , "  let cond = vec3(b0, b1, b2);"
    , "  let anyHit = any(cond);"
    , "  let allHit = all(cond);"
    , "  let base = select(vec3(0.1, 0.2, 0.3), vec3(0.9, 0.3, 0.1), cond);"
    , "  let m = mat2x2(vec2(1.0, 0.0), vec2(0.0, 1.0));"
    , "  let _det = determinant(m);"
    , "  let _mt = transpose(m);"
    , "  let _mi = inverse(m);"
    , "  let _cr = cross(vec3(1.0, 0.0, 0.0), vec3(0.0, 1.0, 0.0));"
    , "  let _ff = faceForward(vec3(0.0, 0.0, 1.0), vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, 1.0));"
    , "  let _rf = refract(vec3(0.0, 0.0, -1.0), vec3(0.0, 0.0, 1.0), 0.66);"
    , "  let mf = modf(p.x);"
    , "  let fx = frexp(p.x);"
    , "  let _ld = ldexp(mf.fract, fx.exp);"
    , "  let pk = pack4x8unorm(vec4(0.1, 0.2, 0.3, 0.4));"
    , "  let up = unpack4x8unorm(pk);"
    , "  let pk2 = pack2x16float(vec2(0.3, 0.7));"
    , "  let up2 = unpack2x16float(pk2);"
    , "  let _lead = firstLeadingBit(pk);"
    , "  let _trail = firstTrailingBit(pk);"
    , "  let _q = quantizeToF16(p.x);"
    , "  let ix = u32(frag_coord.x);"
    , "  let iy = u32(frag_coord.y);"
    , "  let bits = ix ^ (iy << u32(1));"
    , "  let _cnt = countOneBits(bits);"
    , "  let _clz = countLeadingZeros(bits);"
    , "  let _ctz = countTrailingZeros(bits);"
    , "  let rev = reverseBits(bits);"
    , "  let ext = extractBits(rev, u32(4), u32(6));"
    , "  let ins = insertBits(bits, ext, u32(8), u32(6));"
    , "  let packed = (i32(255) << i32(24)) | (i32(1) << i32(16)) | (i32(2) << i32(8)) | i32(3);"
    , "  let _dotu = dot4U8Packed(u32(packed), u32(packed));"
    , "  let _doti = dot4I8Packed(packed, packed);"
    , "  let _bc = bitcast<u32>(f32(1.0));"
    , "  let mask = f32(ins & u32(255)) / 255.0;"
    , "  let v = abs(vec3(p.x, p.y, a));"
    , "  let c = clamp(v, vec3(0.0, 0.0, 0.0), vec3(1.0, 1.0, 1.0));"
    , "  let gain = select(0.2, 1.0, allHit);"
    , "  let glow = select(0.2, 0.8, anyHit);"
    , "  return vec4((base.x + c.x) * gain + mask * 0.2 + up.x + up2.x + _q * 0.0 + _rt * 0.0, base.y + c.y + up.y, base.z + c.z + glow * 0.1, 1.0);"
    , "}"
    ]

runtimeArrayLengthShader :: String
runtimeArrayLengthShader =
  unlines
    [ "struct Data {"
    , "  values: array<u32>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<storage, read_write> data: Data;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let len = arrayLength(data.values);"
    , "  if (gid.x == 0 && len > 0) {"
    , "    data.values[0] = len;"
    , "  }"
    , "}"
    ]

aliasOverrideShader :: String
aliasOverrideShader =
  unlines
    [ "alias Color = vec4<f32>;"
    , "override factor: u32 = 2;"
    , "const_assert(factor == 2);"
    , "struct Params {"
    , "  tint: Color;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) Color {"
    , "  let c = params.tint;"
    , "  return vec4(c.x, c.y, c.z, 1.0);"
    , "}"
    ]

switchLoopShader :: String
switchLoopShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 0: { acc = acc + 1; }"
    , "    case 1, 2: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  loop {"
    , "    acc = acc + 1;"
    , "    continuing {"
    , "      acc = acc + 1;"
    , "      break if (acc > 4);"
    , "    }"
    , "  }"
    , "}"
    ]

badConstAssertShader :: String
badConstAssertShader =
  unlines
    [ "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

constFnShader :: String
constFnShader =
  unlines
    [ "fn add(a: i32, b: i32) -> i32 {"
    , "  let sum = a + b;"
    , "  sum + 0;"
    , "  if (sum > 4) {"
    , "    return sum;"
    , "  }"
    , "  return sum - 1;"
    , "}"
    , "fn pick(n: i32) -> i32 {"
    , "  switch (n) {"
    , "    case 0: { return 1; }"
    , "    case 1: { return 2; }"
    , "    default: { return 3; }"
    , "  }"
    , "  return 3;"
    , "}"
    , "fn greater(a: i32, b: i32) -> bool {"
    , "  if (a > b) {"
    , "    return true;"
    , "  }"
    , "  return false;"
    , "}"
    , "fn make(v: f32) -> vec2<f32> {"
    , "  return vec2(v, v + 1.0);"
    , "}"
    , "fn sum(n: i32) -> i32 {"
    , "  var acc = 0;"
    , "  var i = 0;"
    , "  while (i < n) {"
    , "    acc = acc + i;"
    , "    i++;"
    , "  }"
    , "  return acc;"
    , "}"
    , "fn sum_for(n: i32) -> i32 {"
    , "  var acc = 0;"
    , "  for (var i = 0; i < n; i = i + 1) {"
    , "    acc += i;"
    , "  }"
    , "  return acc;"
    , "}"
    , "const_assert(add(2, 3) == 5);"
    , "const_assert(add(1, 1) == 1);"
    , "const_assert(pick(1) == 2);"
    , "const_assert(pick(3) == 3);"
    , "const_assert(greater(4, 1));"
    , "const_assert(make(1.5).y == 2.5);"
    , "const_assert(sum(4) == 6);"
    , "const_assert(sum_for(4) == 6);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

overrideShader :: String
overrideShader =
  unlines
    [ "override scale: i32;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

overrideSpecShader :: String
overrideSpecShader =
  unlines
    [ "override scale: i32;"
    , "const_assert(scale == 4);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

overrideDefaultShader :: String
overrideDefaultShader =
  unlines
    [ "override scale: i32 = 2 + 3;"
    , "@id(7) override mode: u32 = 1;"
    , "const_assert(scale == 5);"
    , "const_assert(mode == 1);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale) + mode;"
    , "}"
    ]

overrideSpecOpShader :: String
overrideSpecOpShader =
  unlines
    [ "override base: i32 = 2;"
    , "override scale: i32 = base * 3 + 1;"
    , "const_assert(scale == 7);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x + u32(scale);"
    , "}"
    ]

diagnosticShader :: String
diagnosticShader =
  unlines
    [ "diagnostic(off, const_assert);"
    , "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticWarnShader :: String
diagnosticWarnShader =
  unlines
    [ "diagnostic(warning, const_assert);"
    , "const_assert(false);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnreachableShader :: String
diagnosticUnreachableShader =
  unlines
    [ "diagnostic(warning, unreachable_code);"
    , "fn helper() {"
    , "  return;"
    , "  let _ = 1;"
    , "}"
    , "@fragment"
    , "fn main() -> @location(0) vec4<f32> {"
    , "  discard;"
    , "  let _ = 0.5;"
    , "  return vec4(1.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

diagnosticUnusedExprShader :: String
diagnosticUnusedExprShader =
  unlines
    [ "diagnostic(warning, unused_expression);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  gid.x;"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnusedVarShader :: String
diagnosticUnusedVarShader =
  unlines
    [ "diagnostic(warning, unused_variable);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let unused = gid.x;"
    , "  var unused2 = gid.x;"
    , "  let _keep = unused2;"
    , "  let _ = gid.x;"
    , "}"
    ]

diagnosticUnusedParamShader :: String
diagnosticUnusedParamShader =
  unlines
    [ "diagnostic(warning, unused_parameter);"
    , "fn helper(a: i32, b: i32) -> i32 {"
    , "  return a;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = helper(i32(gid.x), 2);"
    , "}"
    ]

diagnosticShadowingShader :: String
diagnosticShadowingShader =
  unlines
    [ "diagnostic(warning, shadowing);"
    , "fn helper(base: i32) -> i32 {"
    , "  var out = base;"
    , "  if (out > 0) {"
    , "    let base = out + 1;"
    , "    out = base;"
    , "  }"
    , "  return out;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _ = helper(i32(gid.x));"
    , "}"
    ]

diagnosticConstantCondShader :: String
diagnosticConstantCondShader =
  unlines
    [ "diagnostic(warning, constant_condition);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  if (true) {"
    , "    let _ = gid.x;"
    , "  }"
    , "  while (false) {"
    , "    let _ = gid.x;"
    , "  }"
    , "  for (; false; ) {"
    , "    let _ = gid.x;"
    , "  }"
    , "}"
    ]

diagnosticDuplicateCaseShader :: String
diagnosticDuplicateCaseShader =
  unlines
    [ "diagnostic(warning, duplicate_case);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 1: { acc = acc + 1; }"
    , "    case 1, 2: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  let _ = acc;"
    , "}"
    ]

switchFallthroughShader :: String
switchFallthroughShader =
  unlines
    [ "let MODE = 1;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 0: { acc = acc + 1; fallthrough; }"
    , "    case MODE: { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "}"
    ]

constArithShader :: String
constArithShader =
  unlines
    [ "const_assert((1 + 2 * 3) == 7);"
    , "const_assert(((8 >> 1) + (3 << 1)) == 10);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var acc = 0;"
    , "  switch (gid.x) {"
    , "    case 1 + 2: { acc = acc + 1; }"
    , "    case (8 >> 1): { acc = acc + 2; }"
    , "    default: { acc = acc + 3; }"
    , "  }"
    , "  if (acc > 4) {"
    , "    acc = acc - 1;"
    , "  }"
    , "}"
    ]

constFloatShader :: String
constFloatShader =
  unlines
    [ "const_assert((0.5 + 0.25) == 0.75);"
    , "const_assert(abs(-1.5) == 1.5);"
    , "const_assert(min(0.5, 0.25) == 0.25);"
    , "const_assert(max(0.5, 0.25) == 0.5);"
    , "const_assert(clamp(2.0, 0.0, 1.0) == 1.0);"
    , "const_assert(mix(0.0, 2.0, 0.25) == 0.5);"
    , "const_assert(select(0.0, 1.0, true) == 1.0);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "}"
    ]

constCompositeShader :: String
constCompositeShader =
  unlines
    [ "struct Pair {"
    , "  a: i32;"
    , "  b: vec2<f32>;"
    , "};"
    , "const_assert(vec2(1.0, 2.0).x == 1.0);"
    , "const_assert(vec3(1.0, 2.0, 3.0).yz.x == 2.0);"
    , "const_assert(mat2x2(vec2(1.0, 2.0), vec2(3.0, 4.0))[1].y == 4.0);"
    , "const_assert(array(1, 2, 3)[2] == 3);"
    , "const_assert(Pair(7, vec2(0.5, 1.5)).b.y == 1.5);"
    , "const_assert(vec2<f32>().x == 0.0);"
    , "const_assert(mat2x2<f32>()[1].y == 0.0);"
    , "const_assert(array<i32, 2>()[1] == 0);"
    , "const_assert(Pair().a == 0);"
    , "const_assert(i32() == 0);"
    , "const_assert(f32() == 0.0);"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "}"
    ]

discardShader :: String
discardShader =
  unlines
    [ "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  if (pos.x < 0.0) {"
    , "    discard;"
    , "  }"
    , "  return vec4(1.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

pointerShader :: String
pointerShader =
  unlines
    [ "fn inc(x: i32) -> i32 {"
    , "  return x + 1;"
    , "}"
    , "fn make() -> i32 {"
    , "  return 1;"
    , "}"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main() {"
    , "  var x = i32(0);"
    , "  let px = &x;"
    , "  *px = *px + 1;"
    , "  let y = inc(1);"
    , "  x = x + y;"
    , "  let z = make();"
    , "  x = x + z;"
    , "}"
    ]

textureVariantsShader :: String
textureVariantsShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex_arr: texture_2d_array<f32>;"
    , "@group(0) @binding(1)"
    , "var tex_3d: texture_3d<f32>;"
    , "@group(0) @binding(2)"
    , "var tex_cube: texture_cube<f32>;"
    , "@group(0) @binding(3)"
    , "var samp: sampler;"
    , "@group(0) @binding(4)"
    , "var depth_tex: texture_depth_2d;"
    , "@group(0) @binding(5)"
    , "var samp_cmp: sampler_comparison;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x / 640.0, frag_coord.y / 480.0);"
    , "  let layer = i32(1);"
    , "  let col_arr = textureSample(tex_arr, samp, uv, layer);"
    , "  let col3 = textureSample(tex_3d, samp, vec3(uv.x, uv.y, 0.5));"
    , "  let colc = textureSample(tex_cube, samp, vec3(uv.x, uv.y, 1.0));"
    , "  let depth = textureSampleCompare(depth_tex, samp_cmp, uv, 0.5);"
    , "  return vec4(col_arr.x + col3.y + colc.z + depth, col_arr.y, col_arr.z, 1.0);"
    , "}"
    ]

textureLoadShader :: String
textureLoadShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex: texture_2d<f32>;"
    , "@group(0) @binding(1)"
    , "var tex_arr: texture_2d_array<f32>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let coord = vec2(i32(gid.x), i32(gid.y));"
    , "  let _a = textureLoad(tex, coord, 0);"
    , "  let _b = textureLoad(tex_arr, coord, 1, 0);"
    , "}"
    ]

storageTextureArrayShader :: String
storageTextureArrayShader =
  unlines
    [ "@group(0) @binding(0)"
    , "var tex_arr: texture_storage_2d_array<rgba8unorm, read_write>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let coord = vec3(i32(gid.x), i32(gid.y), i32(gid.z));"
    , "  let src = textureLoad(tex_arr, coord);"
    , "  textureStore(tex_arr, coord, vec4(src.x, src.y, src.z, src.w));"
    , "}"
    ]

textureAdvancedShader :: String
textureAdvancedShader =
  unlines
    [ "struct Params {"
    , "  time: f32;"
    , "};"
    , "@group(0) @binding(0) var<uniform> params: Params;"
    , "@group(0) @binding(1) var samp: sampler;"
    , "@group(0) @binding(2) var samp_cmp: sampler_comparison;"
    , "@group(0) @binding(3) var tex1d: texture_1d<f32>;"
    , "@group(0) @binding(4) var tex1d_arr: texture_1d_array<f32>;"
    , "@group(0) @binding(5) var tex_cube_arr: texture_cube_array<f32>;"
    , "@group(0) @binding(6) var tex_msaa: texture_multisampled_2d<f32>;"
    , "@group(0) @binding(7) var tex_depth_cube: texture_depth_cube;"
    , "@group(0) @binding(8) var tex_depth_cube_arr: texture_depth_cube_array;"
    , "@group(0) @binding(9) var tex_depth_msaa: texture_depth_multisampled_2d;"
    , "@group(0) @binding(10) var tex_store_1d: texture_storage_1d<rgba8unorm, read_write>;"
    , "@group(0) @binding(11) var tex_store_3d: texture_storage_3d<rgba8unorm, read_write>;"
    , "@fragment"
    , "fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = vec2(frag_coord.x * 0.01, frag_coord.y * 0.01);"
    , "  let lod = 0.5 + params.time * 0.0;"
    , "  let dim0 = textureDimensions(tex1d);"
    , "  let dim1 = textureDimensions(tex1d_arr);"
    , "  let layers = textureNumLayers(tex1d_arr);"
    , "  let levels = textureNumLevels(tex1d);"
    , "  let samples = textureNumSamples(tex_msaa);"
    , "  let c0 = textureSampleLevel(tex1d, samp, uv.x, lod);"
    , "  let c1 = textureSampleLevel(tex1d_arr, samp, uv.x, 1, lod);"
    , "  let c2 = textureSampleBias(tex1d, samp, uv.x, 0.25);"
    , "  let c3 = textureSampleGrad(tex1d, samp, uv.x, 0.01, 0.01);"
    , "  let g0 = textureGather(tex_cube_arr, samp, vec3(uv.x, uv.y, 1.0), 0);"
    , "  let d0 = textureSampleCompareLevel(tex_depth_cube, samp_cmp, vec3(uv.x, uv.y, 1.0), 0.5, lod);"
    , "  let d1 = textureSampleCompareLevel(tex_depth_cube_arr, samp_cmp, vec3(uv.x, uv.y, 1.0), 0, 0.5, lod);"
    , "  let ms = textureLoad(tex_msaa, vec2(0, 0), 0);"
    , "  let msd = textureLoad(tex_depth_msaa, vec2(0, 0), 0);"
    , "  textureStore(tex_store_1d, 0, vec4(uv.x, uv.y, d0, 1.0));"
    , "  textureStore(tex_store_3d, vec3(0, 0, 0), vec4(uv.y, uv.x, 0.0, 1.0));"
    , "  let dims_acc = f32(dim0 + dim1 + layers + levels + samples);"
    , "  return vec4(c0.x + c1.y + c2.z + c3.w + g0.x + d0 + d1 + ms.x + msd + dims_acc * 0.0, 0.2, 0.3, 1.0);"
    , "}"
    ]

globalsShader :: String
globalsShader =
  unlines
    [ "enable f16;"
    , "var<private> bias: f32 = 0.25;"
    , "var<workgroup> scratch: array<u32, 4>;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let v = f16(1.0);"
    , "  let w = f32(v) + bias;"
    , "  if (gid.x == 0) {"
    , "    scratch[0] = u32(w);"
    , "  }"
    , "}"
    ]

layoutAttrShader :: String
layoutAttrShader =
  unlines
    [ "struct Stuff {"
    , "  @align(16) a: vec3<f32>,"
    , "  @size(32) b: vec4<f32>,"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> stuff: Stuff;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let v = stuff.a.x + stuff.b.x;"
    , "  return vec4(v, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformShader :: String
packUniformShader =
  unlines
    [ "enable uniform_buffer_standard_layout;"
    , "struct Payload {"
    , "  a: f32;"
    , "  b: vec3<f32>;"
    , "  c: mat3x3<f32>;"
    , "  d: array<vec2<f32>, 2>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> payload: Payload;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = payload.a + payload.b.x + payload.c[0][0] + payload.d[0].x;"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformExtendedShader :: String
packUniformExtendedShader =
  unlines
    [ "enable uniform_buffer_standard_layout;"
    , "struct Inner {"
    , "  a: vec2<f32>;"
    , "  b: f32;"
    , "};"
    , "struct Params {"
    , "  m3: mat3x3<f32>;"
    , "  m4: mat4x4<f32>;"
    , "  arr2: array<vec2<f32>, 4>;"
    , "  arr3: array<vec3<f32>, 3>;"
    , "  inner: Inner;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params: Params;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = params.m3[0][0] + params.m4[0][0] + params.arr2[0].x + params.arr3[0].x + params.inner.a.x + params.inner.b;"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

packUniformExtendedShader2 :: String
packUniformExtendedShader2 =
  unlines
    [ "enable f16;"
    , "enable uniform_buffer_standard_layout;"
    , "struct Inner2 {"
    , "  v: vec3<f32>;"
    , "  w: f32;"
    , "};"
    , "struct Outer2 {"
    , "  inners: array<Inner2, 2>;"
    , "};"
    , "struct Params2 {"
    , "  m34: mat3x4<f32>;"
    , "  m43: mat4x3<f32>;"
    , "  mats: array<mat2x2<f32>, 2>;"
    , "  nested: Outer2;"
    , "  h: f16;"
    , "  hv: vec3<f16>;"
    , "  hv4: vec4<f16>;"
    , "};"
    , "@group(0) @binding(0)"
    , "var<uniform> params2: Params2;"
    , "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let _ = params2.m34[0][0] + params2.m43[0][0] + params2.mats[0][0][0] + params2.nested.inners[0].v.x + f32(params2.h) + f32(params2.hv.x) + f32(params2.hv4.x);"
    , "  return vec4(0.0, 0.0, 0.0, 1.0);"
    , "}"
    ]

goldenComputeShader :: String
goldenComputeShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  var x = 1;"
    , "  x = x + i32(gid.x);"
    , "}"
    ]

goldenFragmentShader :: String
goldenFragmentShader =
  unlines
    [ "@fragment"
    , "fn main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {"
    , "  let uv = pos.xy / vec2(800.0, 600.0);"
    , "  return vec4(uv.x, uv.y, 0.2, 1.0);"
    , "}"
    ]

badSwitchShader :: String
badSwitchShader =
  unlines
    [ "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  switch (gid.x) {"
    , "    case gid.x: { }"
    , "    default: { }"
    , "  }"
    , "}"
    ]

ifShader :: String
ifShader =
  unlines
    [ "@if(FOO) let scale = 2.0;"
    , "@compute @workgroup_size(1, 1, 1)"
    , "fn main(@builtin(global_invocation_id) gid: vec3<u32>) {"
    , "  let _tmp = scale;"
    , "}"
    ]

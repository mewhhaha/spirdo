{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Example fragment shader using overrides.
module Examples.Override (fragmentOverrideSrc) where

fragmentOverrideSrc =
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

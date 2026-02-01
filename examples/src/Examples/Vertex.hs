{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Example vertex shaders.
module Examples.Vertex
  ( vertexShader
  , vertexFullscreenShader
  ) where

import Spirdo.Wesl.Reflection (wesl)

vertexShader =
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
vertexFullscreenShader =
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

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Shaders for the small rasterized 3D game example.
module Examples.Game.Shaders
  ( gameVertexShader
  , gameFragmentShader
  ) where

import Spirdo.Wesl.Reflection (defaultCompileOptions, imports, spirv, wesl)

gameVertexShader =
      $(spirv defaultCompileOptions imports [wesl|
struct Transform {
  row0: vec4<f32>;
  row1: vec4<f32>;
  row2: vec4<f32>;
  row3: vec4<f32>;
};

struct VertexOutput {
  @builtin(position) position: vec4<f32>;
  @location(0) local_position: vec3<f32>;
  @location(1) world_position: vec3<f32>;
  @location(2) uv: vec2<f32>;
  @location(3) color: vec4<f32>;
};

@group(1) @binding(0)
var<uniform> view_projection: Transform;

@group(1) @binding(1)
var<uniform> model: Transform;

@vertex
fn main(
  @location(0) in_position: vec4<f32>,
  @location(1) in_uv: vec2<f32>,
  @location(2) in_color: vec4<f32>
) -> VertexOutput {
  let world_position = vec4(
    dot(model.row0, in_position),
    dot(model.row1, in_position),
    dot(model.row2, in_position),
    dot(model.row3, in_position)
  );
  let clip_position = vec4(
    dot(view_projection.row0, world_position),
    dot(view_projection.row1, world_position),
    dot(view_projection.row2, world_position),
    dot(view_projection.row3, world_position)
  );
  return VertexOutput(
    clip_position,
    in_position.xyz,
    world_position.xyz,
    in_uv,
    in_color
  );
}
|])

gameFragmentShader =
      $(spirv defaultCompileOptions imports [wesl|
struct FragmentInput {
  @location(0) local_position: vec3<f32>;
  @location(1) world_position: vec3<f32>;
  @location(2) uv: vec2<f32>;
  @location(3) color: vec4<f32>;
};

fn facetVariation(position: vec3<f32>) -> f32 {
  let cell = floor(position * 3.0);
  return fract(sin(dot(cell, vec3(12.9898, 78.233, 37.719))) * 43758.5453);
}

@fragment
fn main(input: FragmentInput) -> @location(0) vec4<f32> {
  let normal_cross = cross(
    dpdx(input.world_position),
    dpdy(input.world_position)
  );
  let normal_length_squared = max(dot(normal_cross, normal_cross), 0.00000000000000000001);
  let face_normal = normal_cross * inverseSqrt(normal_length_squared);
  let light_direction = normalize(vec3(0.35, 0.8, 0.45));
  let diffuse = abs(dot(face_normal, light_direction));
  let lighting = 0.55 + diffuse * 0.65;

  let height = smoothstep(-0.8, 1.1, input.local_position.y);
  let height_tint = mix(
    vec3(0.17, 0.32, 0.29),
    vec3(0.92, 0.62, 0.28),
    vec3(height, height, height)
  );
  let mesh_color = input.color.xyz * mix(
    vec3(0.72, 0.72, 0.72),
    height_tint,
    vec3(0.38, 0.38, 0.38)
  );
  let surface_detail = 0.9
    + facetVariation(input.local_position) * 0.12
    + sin((input.uv.x + input.uv.y) * 18.0) * 0.025;
  let lit_color = mesh_color * lighting * surface_detail;
  let world_axis_accent = pow(1.0 - abs(face_normal.z), 3.0) * 0.18;
  let final_color = lit_color + input.color.xyz * 0.12 + vec3(0.18, 0.3, 0.42) * world_axis_accent;

  return vec4(final_color, input.color.w);
}
|])

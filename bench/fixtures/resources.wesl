struct Params {
  time: f32;
};

@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var samp: sampler;
@group(0) @binding(2) var samp_cmp: sampler_comparison;
@group(0) @binding(3) var tex1d: texture_1d<f32>;
@group(0) @binding(4) var tex1d_arr: texture_1d_array<f32>;
@group(0) @binding(5) var tex_cube_arr: texture_cube_array<f32>;
@group(0) @binding(6) var tex_msaa: texture_multisampled_2d<f32>;
@group(0) @binding(7) var tex_depth_cube: texture_depth_cube;
@group(0) @binding(8) var tex_depth_cube_arr: texture_depth_cube_array;
@group(0) @binding(9) var tex_depth_msaa: texture_depth_multisampled_2d;
@group(0) @binding(10) var tex_store_1d: texture_storage_1d<rgba8unorm, read_write>;
@group(0) @binding(11) var tex_store_3d: texture_storage_3d<rgba8unorm, read_write>;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x * 0.01, frag_coord.y * 0.01);
  let lod = 0.5 + params.time * 0.0;
  let dim0 = textureDimensions(tex1d);
  let dim1 = textureDimensions(tex1d_arr);
  let layers = textureNumLayers(tex1d_arr);
  let levels = textureNumLevels(tex1d);
  let samples = textureNumSamples(tex_msaa);
  let c0 = textureSampleLevel(tex1d, samp, uv.x, lod);
  let c1 = textureSampleLevel(tex1d_arr, samp, uv.x, 1, lod);
  let c2 = textureSampleBias(tex1d, samp, uv.x, 0.25);
  let c3 = textureSampleGrad(tex1d, samp, uv.x, 0.01, 0.01);
  let g0 = textureGather(tex_cube_arr, samp, vec3(uv.x, uv.y, 1.0), 0);
  let d0 = textureSampleCompareLevel(tex_depth_cube, samp_cmp, vec3(uv.x, uv.y, 1.0), 0.5, lod);
  let d1 = textureSampleCompareLevel(tex_depth_cube_arr, samp_cmp, vec3(uv.x, uv.y, 1.0), 0, 0.5, lod);
  let ms = textureLoad(tex_msaa, vec2(0, 0), 0);
  let msd = textureLoad(tex_depth_msaa, vec2(0, 0), 0);
  textureStore(tex_store_1d, 0, vec4(uv.x, uv.y, d0, 1.0));
  textureStore(tex_store_3d, vec3(0, 0, 0), vec4(uv.y, uv.x, 0.0, 1.0));
  let dims_acc = f32(dim0 + dim1 + layers + levels + samples);
  return vec4(c0.x + c1.y + c2.z + c3.w + g0.x + d0 + d1 + ms.x + msd + dims_acc * 0.0, 0.2, 0.3, 1.0);
}

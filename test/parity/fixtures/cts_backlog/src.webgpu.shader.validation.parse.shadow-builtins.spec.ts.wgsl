@group(0) @binding(0) var t_2d: texture_2d<f32>;
@group(0) @binding(1) var t_2d_depth: texture_depth_2d;
@group(0) @binding(2) var t_2d_array: texture_2d_array<f32>;
@group(0) @binding(3) var t_2d_ms: texture_multisampled_2d<f32>;

@group(1) @binding(0) var s: sampler;
@group(1) @binding(1) var sc: sampler_comparison;



fn sibling() {
  
}

@fragment
fn main() -> @location(0) vec4f {
  
  
  return vec4f(1);
}

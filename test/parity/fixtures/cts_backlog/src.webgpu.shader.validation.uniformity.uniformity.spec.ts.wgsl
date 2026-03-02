@group(0) @binding(0) var t : texture_2d<f32>;
@group(0) @binding(1) var s : sampler;
@group(0) @binding(2) var td : texture_depth_2d;
@group(0) @binding(3) var sd : sampler_comparison;
@group(0) @binding(4) var ta : texture_2d_array<f32>;
@group(0) @binding(5) var ts : texture_multisampled_2d<f32>;

const uniform_cond : bool = true;
var<private> nonuniform_cond : bool = true;

@group(1) @binding(0)
var<storage> uniform_values : array<u32, 4>;
@group(1) @binding(1)
var<storage, read_write> nonuniform_values : array<u32, 4>;

@compute @workgroup_size(1)
fn main() {
  if (uniform_cond) {
    nonuniform_values[0] = uniform_values[0];
  }
  if (nonuniform_cond) {
    nonuniform_values[1] = uniform_values[1];
  }
  let _t = t;
  let _s = s;
  let _td = td;
  let _sd = sd;
  let _ta = ta;
  let _ts = ts;
}

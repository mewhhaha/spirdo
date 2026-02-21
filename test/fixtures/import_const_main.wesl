import import_const_lib;

struct Local {
  values: array<f32, import_const_lib::N>,
};

@group(0) @binding(0)
var<uniform> p: import_const_lib::Params;

@compute @workgroup_size(import_const_lib::WG)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid;
}

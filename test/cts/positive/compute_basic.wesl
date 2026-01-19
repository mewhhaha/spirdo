struct Data {
  values: array<u32>;
};

@group(0) @binding(0)
var<storage, read_write> data: Data;

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let idx = gid.x;
  let len = arrayLength(data.values);
  if (idx < len) {
    data.values[idx] = data.values[idx] + u32(1);
  }
}

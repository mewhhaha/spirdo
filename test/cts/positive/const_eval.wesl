let SCALE = 2 + 3;
const_assert(SCALE == 5);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(SCALE);
}

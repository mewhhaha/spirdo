@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  if (gid.x > 0u) {
    let inner = 1;
  }
  let _ = inner;
}

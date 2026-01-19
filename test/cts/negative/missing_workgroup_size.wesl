@compute
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

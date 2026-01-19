override base: i32 = 1;
override derived: i32 = base + 1;

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(derived);
}

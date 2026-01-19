override a: i32 = b + 1;
override b: i32 = a + 1;

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(a + b);
}

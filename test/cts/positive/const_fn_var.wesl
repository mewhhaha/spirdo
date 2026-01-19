fn bad() -> i32 {
  var x = 0;
  let p = &x;
  return *p;
}

const_assert(bad() == 0);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

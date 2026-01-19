fn ptr_eq() -> bool {
  var x: i32 = 1;
  let p = &x;
  return p == &x;
}

const_assert(ptr_eq());

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(1);
}

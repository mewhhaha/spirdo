fn bump(v: i32) -> i32 {
  var tmp = v;
  let p = &tmp;
  *p = *p + 2;
  return tmp;
}

const_assert(bump(3) == 5);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

fn pick(n: i32) -> i32 {
  switch (n) {
    case 0: { return 4; }
    case 1: { return 5; }
    default: { return 6; }
  }
  return 6;
}

const_assert(pick(1) == 5);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

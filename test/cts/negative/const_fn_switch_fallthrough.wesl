fn bad(n: i32) -> i32 {
  switch (n) {
    case 0: { fallthrough; }
    default: { return 1; }
  }
}

const_assert(bad(0) == 1);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

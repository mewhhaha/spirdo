fn pick(x: i32) -> i32 {
  var v = 0;
  switch (x) {
    case 0: { v = 1; fallthrough; }
    case 1: { v = v + 2; }
    default: { v = v + 3; }
  }
  return v;
}

const_assert(pick(0) == 3);
const_assert(pick(1) == 2);
const_assert(pick(2) == 3);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

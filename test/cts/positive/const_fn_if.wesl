fn choose(a: i32, b: i32) -> i32 {
  let sum = a + b;
  if (sum > 3) {
    return sum;
  }
  return sum - 1;
}

const_assert(choose(2, 2) == 4);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

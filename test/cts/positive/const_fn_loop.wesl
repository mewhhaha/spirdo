fn sum(n: i32) -> i32 {
  var acc = 0;
  var i = 0;
  while (i < n) {
    acc += i;
    i = i + 1;
  }
  return acc;
}

fn sum_for(n: i32) -> i32 {
  var acc = 0;
  for (var i = 0; i < n; i = i + 1) {
    if (i == 2) { continue; }
    acc = acc + i;
    if (acc > 4) { break; }
  }
  return acc;
}

const_assert(sum(4) == 6);
const_assert(sum_for(5) == 8);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

let A = vec2(1.0, 2.0);
let B = vec3(1.0, 2.0, 3.0);

const_assert(A == B);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(1);
}

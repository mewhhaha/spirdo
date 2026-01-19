fn make(v: f32) -> vec2<f32> {
  return vec2(v, v + 1.0);
}

const_assert(make(1.5).y == 2.5);

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x;
}

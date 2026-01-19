struct Pair {
  a: vec2<f32>,
  b: vec3<i32>,
};

let V = vec3(1.0, 2.0, 3.0);
let M = mat2x2(1.0, 0.0, 0.0, 1.0);
let P = Pair(vec2(1.0, 2.0), vec3(1, 2, 3));

const_assert(V == vec3(1.0, 2.0, 3.0));
const_assert(M == mat2x2(1.0, 0.0, 0.0, 1.0));
const_assert(P == Pair(vec2(1.0, 2.0), vec3(1, 2, 3)));

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let _ = gid.x + u32(1);
}

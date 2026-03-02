@group(0) @binding(0) var t : texture_2d<f32>;
@group(0) @binding(1) var s : sampler;
@group(0) @binding(2) var<storage, read_write> a : atomic<i32>;

struct S { b : bool }

var<private> b : bool;
var<private> m : mat2x2f;
var<private> arr : array<bool, 4>;
var<private> str : S;

@compute @workgroup_size(1)
fn main() {
  let foo = !0;
}

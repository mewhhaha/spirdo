enable unrestricted_pointer_parameters;

@binding(0) @group(0) var<uniform> input : array<vec4i, 4>;
@binding(1) @group(0) var<storage, read_write> output : array<vec4i, 4>;

fn sum(f : ptr<function, i32>,
       w : ptr<workgroup, i32>,
       p : ptr<private, i32>,
       u : ptr<uniform, vec4i>) -> vec4i {

  return vec4(*f + *w + *p) + *u;
}

struct S {
  i : i32,
}

var<private> P0 : S = S(0);
var<private> P1 : S = S(10);
var<private> P2 : i32 = 20;
var<private> P3 : i32 = 30;

struct T {
  i : i32,
}

var<workgroup> W0 : T;
var<workgroup> W1 : i32;
var<workgroup> W2 : T;
var<workgroup> W3 : i32;

@compute @workgroup_size(1)
fn main() {
  W0.i = 0;
  W1 = 100;
  W2.i = 200;
  W3 = 300;

  var F = array(0, 1000, 2000, 3000);

  output[0] = sum(&F[2], &W3,   &P1.i, &input[0]); // vec4(2310) + vec4(1, 2, 3, 4)
  output[1] = sum(&F[1], &W2.i, &P0.i, &input[1]); // vec4(1200) + vec4(4, 3, 2, 1)
  output[2] = sum(&F[3], &W0.i, &P3,   &input[2]); // vec4(3030) + vec4(2, 4, 1, 3)
  output[3] = sum(&F[2], &W1,   &P2,   &input[3]); // vec4(2120) + vec4(4, 1, 2, 3)
}

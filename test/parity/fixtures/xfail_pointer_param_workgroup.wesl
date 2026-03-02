enable unrestricted_pointer_parameters;

var<workgroup> shared: i32;

fn use_ptr(p: ptr<workgroup, i32>) {
  let _ = *p;
}

@compute @workgroup_size(1)
fn main() {
  use_ptr(&shared);
}

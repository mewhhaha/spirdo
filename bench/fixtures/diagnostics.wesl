diagnostic(warning, unused_expression);
diagnostic(warning, unused_variable);
diagnostic(warning, unused_parameter);
diagnostic(warning, constant_condition);
diagnostic(warning, duplicate_case);

fn retain_first(first: i32, second: i32) -> i32 {
  return first;
}

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let unused = i32(gid.x);
  let selected = retain_first(i32(gid.x), 2);
  selected;
  if (true) {
    let _ = selected;
  }
  switch (gid.x) {
    case 1: { }
    case 1, 2: { }
    default: { }
  }
}

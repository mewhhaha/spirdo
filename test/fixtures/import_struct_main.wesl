import import_struct_lib::Foo;

@compute @workgroup_size(1, 1, 1)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let v = Foo();
  let _ = gid.x + u32(v.x + 1);
}

fn b() -> i32 {
  return 1;
}

@fragment
fn frag() -> @location(0) vec4f {
  var a = 0;
  
  return vec4f(1);
}

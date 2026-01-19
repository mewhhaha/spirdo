@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = frag_coord.xy / vec2(800.0, 600.0);
  return vec4(uv.x, uv.y, 0.5, 1.0);
}

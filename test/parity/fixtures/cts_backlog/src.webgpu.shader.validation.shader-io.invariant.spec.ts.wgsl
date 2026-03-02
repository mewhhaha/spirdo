struct VertexOut {
      @builtin(position)  position : vec4<f32>
    };
    @vertex
    fn main() -> VertexOut {
      return VertexOut();
    }

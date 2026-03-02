struct S {
        val: f32,
      }
      @group(0) @binding(0) var<storage, read_write> buffer : S;

      @compute @workgroup_size(1)
      fn main() {
        buffer = S();
      }

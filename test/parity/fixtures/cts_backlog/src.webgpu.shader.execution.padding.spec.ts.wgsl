struct S {
        a : u32,
        // 12 bytes of padding
        b : vec3<u32>,
        // 4 bytes of padding
        c : vec2<u32>,
        // 8 bytes of padding
      }
      @group(0) @binding(0) var<storage, read_write> buffer : S;

      @compute @workgroup_size(1)
      fn main() {
        buffer = S(0x12345678, vec3(0xabcdef01), vec2(0x98765432));
      }

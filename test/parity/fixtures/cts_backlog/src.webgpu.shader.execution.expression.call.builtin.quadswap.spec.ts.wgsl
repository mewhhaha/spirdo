struct VSOutput {
        @builtin(position) position: vec4f,
        @location(0) texcoord: vec2f,
      };

      struct Uniforms {
        baseArrayLayer: u32,
      };

      @vertex fn vs(
        @builtin(vertex_index) vertexIndex : u32
      ) -> VSOutput {
          let pos = array(
             vec2f(-1, -1),
             vec2f(-1,  3),
             vec2f( 3, -1),
          );

          var vsOutput: VSOutput;

          let xy = pos[vertexIndex];

          vsOutput.position = vec4f(xy, 0.0, 1.0);
          vsOutput.texcoord = xy * vec2f(0.5, -0.5) + vec2f(0.5);

          return vsOutput;
       }

       @group(0) @binding(0) var ourSampler: sampler;
       @group(0) @binding(1) var ourTexture: texture_2d<f32>;
       @group(0) @binding(2) var<uniform> uni: Uniforms;

       @fragment fn fs(fsInput: VSOutput) -> @location(0) vec4f {
          let _uni = uni;
          return textureSample(ourTexture, ourSampler, fsInput.texcoord);
       }

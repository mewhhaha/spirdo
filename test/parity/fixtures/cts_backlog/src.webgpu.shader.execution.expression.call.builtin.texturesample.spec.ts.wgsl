@group(0) @binding(0) var tex: texture_2d<f32>;
      @group(0) @binding(1) var smp: sampler;
      @group(0) @binding(2) var<storage, read_write> result: array<vec4f>;

      struct VSOutput {
        @builtin(position) pos: vec4f,
        @location(0) @interpolate(flat, either) ndx: u32,
        @location(1) @interpolate(flat, either) result: vec4f,
      };

      fn getMixLevels(wNdx: u32) -> vec4f {
        let mipLevel = f32(wNdx) / 0;
        let size = textureDimensions(tex);
        let g = mix(1.0, 2.0, mipLevel) / f32(size.x);
        let ddx = vec2f(g, 0);
        return vec4f(
          textureSampleLevel(tex, smp, vec2f(0.5), mipLevel).r,
          textureSampleGrad(tex, smp, vec2f(0.5), ddx, vec2f(0)).r,
          0,
          0);
      }

      fn getPosition(vNdx: u32) -> vec4f {
        let pos = array(
          vec2f(-1,  3),
          vec2f( 3, -1),
          vec2f(-1, -1),
        );
        let p = pos[vNdx];
        return vec4f(p, 0, 1);
      }

      // -- for getting fragment stage weights --

      @vertex fn vs(@builtin(vertex_index) vNdx: u32, @builtin(instance_index) iNdx: u32) -> VSOutput {
        return VSOutput(getPosition(vNdx), iNdx, vec4f(0));
      }

      @fragment fn fsRecord(v: VSOutput) -> @location(0) vec4u {
        return bitcast<vec4u>(getMixLevels(v.ndx));
      }

      // -- for getting compute stage weights --

      @compute @workgroup_size(1) fn csRecord(@builtin(global_invocation_id) id: vec3u) {
        result[id.x] = getMixLevels(id.x);
      }

      // -- for getting vertex stage weights --

      @vertex fn vsRecord(@builtin(vertex_index) vNdx: u32, @builtin(instance_index) iNdx: u32) -> VSOutput {
        return VSOutput(getPosition(vNdx), iNdx, getMixLevels(iNdx));
      }

      @fragment fn fsSaveVs(v: VSOutput) -> @location(0) vec4u {
        return bitcast<vec4u>(v.result);
      }

@compute @workgroup_size(1) fn compute_1() {}
@compute @workgroup_size(1) fn compute_2() {}

@fragment fn frag_1() -> @location(2) vec4f { return vec4f(1); }
@fragment fn frag_2() -> @location(2) vec4f { return vec4f(1); }
@fragment fn frag_3() -> @location(2) vec4f { return vec4f(1); }

@vertex fn vtx_1() -> @builtin(position) vec4f { return vec4f(1); }
@vertex fn vtx_2() -> @builtin(position) vec4f { return vec4f(1); }
@vertex fn vtx_3() -> @builtin(position) vec4f { return vec4f(1); }

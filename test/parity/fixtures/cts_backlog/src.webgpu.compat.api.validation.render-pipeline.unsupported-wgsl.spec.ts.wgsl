struct Vertex {
            @builtin(position) pos: vec4f,
            @location(0) color : vec4f,
        };
        @vertex fn vs() -> Vertex {
            var v: Vertex;
            v.pos = vec4f(1);
            v.color = vec4f(1);
            return v;
        }
        @fragment fn fsWithoutInterpolationUsage() -> @location(0) vec4f {
            return vec4f(1);
        }
        @fragment fn fsWithInterpolationUsage1(v: Vertex) -> @location(0) vec4f {
            return vec4f(1);
        }
        @fragment fn fsWithInterpolationUsage2(v: Vertex) -> @location(0) vec4f {
            return v.pos;
        }
        @fragment fn fsWithInterpolationUsage3(v: Vertex) -> @location(0) vec4f {
            return v.color;
        }

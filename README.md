# Spirdo

Haskell WESL compiler + SDL3 demo app that emits SPIR-V and renders shader variants.

## Build and Run
```sh
cabal build
cabal run
```

Use left/right arrow keys to cycle fragment shader variants in the demo window.

## Example Usage (Quasiquoter)
```hs
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString as BS
import Spirdo.Wesl (compileWeslToSpirvWith, defaultCompileOptions, wesl)

main :: IO ()
main = do
  let shader = [wesl|
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]
  case compileWeslToSpirvWith defaultCompileOptions shader of
    Left err -> error (show err)
    Right compiled -> BS.writeFile "example.spv" (shaderSpirv compiled)
```

## Example Shaders in the Demo
Fragment variants in `exe/Main.hs` (left/right to switch):
- Feature Mix
- Raymarch
- Triangle
- Plasma
- Grid
- SDF Text
- Clouds
- Bits
- Aurora
- Starfield
- Tunnel
- Voronoi
- Mandelbrot
- Override Stripes / Override Rings (specialization values)

Compute examples emitted on build:
- `compute.spv`: storage buffer + storage texture sample
- `compute-1.spv`: particle update on runtime array
- `compute-2.spv`: tiled storage texture writer

Vertex examples emitted on build:
- `vertex.spv`: passthrough quad
- `vertex-1.spv`: wave-displaced positions
- `vertex-2.spv`: fullscreen triangle (vertex_index)

## SPIR-V Outputs
On build/run, SPIR-V files are written to the repo root:
- `fragment-*.spv` for each fragment variant
- `compute*.spv` for compute examples
- `vertex*.spv` for vertex examples

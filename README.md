# Spirdo

Haskell WESL compiler with an optional SDL3 demo that renders shader variants (optional SPIR-V output).

## Features
- WESL/WGSL coverage: control flow, functions, modules/imports, attributes, and diagnostics.
- Full binding/resource model: uniforms, storage buffers, samplers, textures, storage textures.
- Builtins: math, bit ops, packing, texture queries/sampling, derivatives.
- Override specialization constants with `SpecStrict` (validator‑friendly) and `SpecParity` (full WESL parity).
- Diagnostics surfaced as warnings (unused/shadowing/constant conditions/unreachable/duplicate cases).
- Typed interface reflection with binding metadata and ordering helpers.
- Host‑agnostic HList inputs for type‑safe binding submission.
- Uniform packing with layout validation + Storable packing helpers.
- Vertex input reflection (`vertexAttributes`) for pipeline setup.
- Optional SPIR‑V validation (tests use `spirv-val` when available).
- Minimal `wesl.toml` package metadata parsing.

## Build and Run
Library/test builds (no demo):
```sh
cabal build
cabal test
```

Demo app (disabled by default):
```sh
cabal build -f demo
cabal run -f demo
```

The demo uses the SDL3 GPU renderer (FFI in `exe/Spirdo/SDL3.hsc`) and requires
SDL3 to be installed. Use left/right arrow keys to cycle fragment shader
variants in the demo window.

Set `SPIRDO_WRITE_SPV=1` to emit SPIR-V files (`fragment-*.spv`, `vertex*.spv`,
`compute*.spv`) for inspection.

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

Note: the public API is exposed from `Spirdo.Wesl` and `Spirdo.Wesl.Inputs`.
Internal modules are not part of the supported surface area.

## Type-Safe Binding Reflection
Using the quasiquoter gives you a `CompiledShader iface` where the binding
interface is captured in the type. You can then derive the exact bindings
needed by the shader in a type-safe way without manual plumbing.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( BindingDesc(..)
  , CompiledShader
  , binding
  , bindingEither
  , bindingMaybe
  , wesl
  )

shader :: CompiledShader iface
shader = [wesl|
struct Params {
  time_res: vec4<f32>;
};

@group(0) @binding(0)
var<uniform> params: Params;

@group(0) @binding(1)
var sampler0: sampler;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]

params :: BindingDesc
params = binding @"params" shader

sampler0 :: BindingDesc
sampler0 = binding @"sampler0" shader

-- Total lookup if you prefer explicit error handling:
paramsSafe :: Either String BindingDesc
paramsSafe = bindingEither @"params" shader

paramsMaybe :: Maybe BindingDesc
paramsMaybe = bindingMaybe @"params" shader
```

Each `BindingDesc` includes the name, group (set), and binding index you need
to create descriptor layouts or bind resources on the CPU side.

Convenience filters are available: `uniformBindingsFor`, `samplerBindingsFor`,
`storageBufferBindingsFor`, and `storageTextureBindingsFor`.

For actual input submission, prefer the typed HList helpers below; they enforce
order and kind at compile time.

### Uniform Packing Helpers
If you want to pack CPU-side data into a uniform buffer layout (std140-like),
use the `TypeLayout` from the compiled interface and the helpers in
`Spirdo.Wesl`.

```hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.List (find)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Spirdo.Wesl

shader :: CompiledShader iface
shader = [wesl|
struct Params { time: f32; };
@group(0) @binding(0) var<uniform> params: Params;
@fragment fn main(@builtin(position) p: vec4<f32>) -> @location(0) vec4<f32> {
  return vec4(0.0, 0.0, 0.0, 1.0);
}
|]

data Params = Params { time :: Float } deriving (Generic)
instance ToUniform Params

packed :: Either String BS.ByteString
packed =
  case find (\b -> biName b == "params") (siBindings (shaderInterface shader)) of
    Nothing -> Left "missing params binding"
    Just bi -> packUniformFrom (biType bi) (Params 1.5)
```

If you already have a `Storable` record that matches the WGSL layout, you can
validate it and pack bytes directly:

```hs
import Data.Proxy (Proxy(..))
import Spirdo.Wesl
  ( TypeLayout(..)
  , Scalar(..)
  , validateUniformStorable
  , packUniformStorable
  )

layout :: TypeLayout
layout = TLScalar F32 4 4

ok :: Either String ()
ok = validateUniformStorable layout (Proxy @Float)

bytesIO :: IO (Either String BS.ByteString)
bytesIO = packUniformStorable layout (1.0 :: Float)
```

Note: `validateUniformStorable` checks size/alignment only. Field ordering and
padding are still your responsibility.

### Typed Input Lists (Host-Agnostic)
You can build a typed input list for the shader interface using `HList` and
opaque handles. This stays host‑agnostic and still enforces ordering/types.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( CompiledShader
  , uniform
  , wesl
  )
import Spirdo.Wesl.Inputs
  ( HList(..)
  , SamplerHandle(..)
  , TextureHandle(..)
  , StorageTextureHandle(..)
  , ShaderInputs
  , inputsFor
  )

shader :: CompiledShader iface
shader = [wesl|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var samp0: sampler;
@group(0) @binding(2) var tex0: texture_2d<f32>;
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  return vec4(0.0, 0.0, 0.0, 1.0);
}
|]

inputs :: Either String (ShaderInputs iface)
inputs =
  inputsFor shader
    ( uniform (V4 0.0 0.0 0.0 0.0 :: V4 Float)
        :& SamplerHandle 1
        :& TextureHandle 2
        :& HNil
    )
```

Note: `SamplerHandle`/`TextureHandle` values are **your runtime resource IDs**, not shader binding indices. They must correspond to real resources in your renderer.

Storage textures use `StorageTextureHandle` to keep sampled vs. storage bindings
distinct at the type level.

`InputsOf iface` is a type-level list derived from the shader’s bindings, so the
order and kinds are enforced at compile time. The resulting `ShaderInputs` lists
(`siUniforms`, `siSamplers`, `siTextures`, ...) are ready to hand off to your
SDL upload/bind code.

`inputsFor` is pure; handle `Either` as you prefer.

Notes:
- Record field names must match the WESL struct field names (extra or missing
  fields are errors).

### Binding Plans and Vertex Attributes
You can derive a binding plan (sorted by set/binding) and vertex attributes from
the reflected interface. This is useful when building your own graphics pipeline
layout in SDL or another renderer.

```hs
import Spirdo.Wesl
  ( bindingPlan
  , vertexAttributes
  )

plan = bindingPlan (shaderInterface shader)

attrs = vertexAttributes (shaderInterface shader)
```

`vertexAttributes` returns only `@location` inputs; builtins are ignored. If the
shader uses non-scalar/vector inputs, you’ll get an error.

`pushConstantLayout (shaderInterface shader)` currently returns `Nothing` because
WESL does not expose push constants yet, but the API is in place.

### Demo (exe only)
The demo executable is gated behind the `demo` flag and is not part of the
library API. It uses SDL3 to render a full-screen quad with fragment shaders.


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

Compute examples emitted when SPIR-V output is enabled:
- `compute.spv`: storage buffer + storage texture sample
- `compute-1.spv`: particle update on runtime array
- `compute-2.spv`: tiled storage texture writer

Vertex examples emitted when SPIR-V output is enabled:
- `vertex.spv`: passthrough quad
- `vertex-1.spv`: wave-displaced positions
- `vertex-2.spv`: fullscreen triangle (vertex_index)

## SPIR-V Outputs
SPIR-V output is opt-in. Set `SPIRDO_WRITE_SPV=1` before running the demo:
```
SPIRDO_WRITE_SPV=1 cabal run
```

When enabled, files are written to the repo root:
- `fragment-*.spv` for each fragment variant
- `compute*.spv` for compute examples
- `vertex*.spv` for vertex examples

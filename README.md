# Spirdo

Haskell WESL compiler with an optional SDL3 demo that renders shader variants (optional SPIR-V output).

## Features
- WESL/WGSL coverage: control flow, functions, modules/imports, attributes, and diagnostics.
- Full binding/resource model: uniforms, storage buffers, samplers, textures, storage textures.
- Builtins: math, bit ops, packing, texture queries/sampling, derivatives.
- Override specialization constants with `SpecStrict` (validator‑friendly) and `SpecParity` (full WESL parity).
- Diagnostics surfaced as warnings (unused/shadowing/constant conditions/unreachable/duplicate cases).
- Typed interface reflection with binding metadata and ordering helpers.
- Declarative input builder and HList inputs for type‑safe binding submission.
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

### Compile-Time Cache & Timings
WESL quasiquotes use an on-disk cache under `dist-newstyle/.wesl-cache`.
You can control it via `CompileOptions`:

```hs
defaultCompileOptions
  { cacheEnabled = True
  , cacheVerbose = False
  , timingVerbose = False
  }
```

Set `cacheVerbose = True` to print basic timing output (cache read/write).
Set `timingVerbose = True` to print per‑phase compiler timings (parse/validate/emit).

You can also time a single compile in code:
```hs
import Spirdo.Wesl (compileWeslToSpirvBytesWithTimings, defaultCompileOptions)

_ <- compileWeslToSpirvBytesWithTimings defaultCompileOptions shaderSrc
```

## Declarative Binding Flow (Preferred)
The recommended path is: **prepare → inputsFromPrepared → submit**.
It’s concise, type‑safe, and renderer‑agnostic.

### Minimal, Declarative Inputs (Host-Agnostic)
Use the small input builder DSL to keep callsites short while preserving
type‑level checks on binding names and kinds.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl.Inputs
  ( InputsBuilder
  , SamplerHandle(..)
  , ShaderInputs
  , TextureHandle(..)
  , inputsFromPrepared
  , sampler
  , texture
  , uniform
  )
import Spirdo.Wesl (CompiledShader, prepareShader, wesl)

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
inputs = do
  prep <- prepareShader shader
  inputsFromPrepared prep $
    uniform @"params" (V4 0.0 0.0 0.0 0.0 :: V4 Float)
    <> sampler @"samp0" (SamplerHandle 1)
    <> texture @"tex0" (TextureHandle 2)
```

Note: `SamplerHandle`/`TextureHandle` values are **your runtime resource IDs**, not shader binding indices. They must correspond to real resources in your renderer.

Storage textures use `StorageTextureHandle` to keep sampled vs. storage bindings
distinct at the type level.

`inputsFromPrepared` uses the shader interface to order everything by
`(group, binding, name)` and pack uniforms for you. The resulting `ShaderInputs`
lists (`siUniforms`, `siSamplers`, `siTextures`, ...) are ready to hand off to your
renderer.

If you need deterministic uniform ordering (by group/binding/name), use
`orderedUniforms` (or `uniformSlots` for a tiny, backend‑ready view).

`inputsFromPrepared` is pure; handle `Either` as you prefer.

Notes:
- Record field names must match the WESL struct field names (extra or missing
  fields are errors).
 - `PreparedShader` is the preferred entrypoint; it caches stage, binding plan,
   and vertex attributes.

### Storage Buffers & Storage Textures (Builder)
```hs
import Spirdo.Wesl.Inputs
  ( inputsFromPrepared
  , storageBuffer
  , storageTexture
  , BufferHandle(..)
  , StorageTextureHandle(..)
  )

Right si =
  inputsFromPrepared prepared $
    storageBuffer @"particles" (BufferHandle 7)
    <> storageTexture @"outImage" (StorageTextureHandle 3)
```

### Uniform Packing Helpers (Advanced)
The high‑level path (`inputsFromPrepared`) already packs uniforms for you.
Use these only if you need raw layout control:
- `packUniformFrom` for `ToUniform` values
- `packUniformStorable` for pre‑laid‑out `Storable` records

### Pipeline Integration (Renderer‑Agnostic)
Spirdo does **not** bind you to any graphics API. The idea is:
1) use the interface reflection to build *your* backend’s pipeline descriptor,
2) use `ShaderInputs` to feed uniform bytes + resource handles into your backend.

```hs
{-# LANGUAGE DataKinds #-}

import Spirdo.Wesl
  ( CompiledShader
  , PreparedShader
  , ShaderInterface(..)
  , bindingPlan
  , prepareShader
  , vertexAttributes
  )
import Spirdo.Wesl.Types.Interface
  ( BindingPlan(..)
  , VertexAttribute(..)
  )
import Spirdo.Wesl.Inputs (ShaderInputs(..))

-- Your backend descriptor (renderer‑agnostic example).
data PipelineDesc = PipelineDesc
  { pdVertexAttributes :: [VertexAttribute]
  , pdUniformCount :: Int
  , pdSamplerCount :: Int
  , pdTextureCount :: Int
  , pdStorageBufferCount :: Int
  , pdStorageTextureCount :: Int
  }

buildPipelineDesc
  :: CompiledShader vIface
  -> CompiledShader fIface
  -> Either String PipelineDesc
buildPipelineDesc vShader fShader = do
  let vIface = shaderInterface vShader
      fPlan = bindingPlan (shaderInterface fShader)
  vAttrs <- vertexAttributes vIface
  pure PipelineDesc
    { pdVertexAttributes = vAttrs
    , pdUniformCount = length (bpUniforms fPlan)
    , pdSamplerCount = length (bpSamplers fPlan)
    , pdTextureCount = length (bpTextures fPlan)
    , pdStorageBufferCount = length (bpStorageBuffers fPlan)
    , pdStorageTextureCount = length (bpStorageTextures fPlan)
    }

-- Optional: precompute interface data once.
preparedExample :: CompiledShader iface -> Either String (PreparedShader iface)
preparedExample = prepareShader

-- Feeding inputs into your backend (pseudo‑API).
submitInputs
  :: ShaderInputs iface
  -> IO ()
submitInputs inputs = do
  -- upload uniforms (each has name/group/binding + bytes)
  mapM_ uploadUniform (siUniforms inputs)
  -- bind resources using your own handle types
  mapM_ bindSampler (siSamplers inputs)
  mapM_ bindTexture (siTextures inputs)
  mapM_ bindStorageBuffer (siStorageBuffers inputs)
  mapM_ bindStorageTexture (siStorageTextures inputs)
```

### Compilation API (Common Paths)
```hs
import Spirdo.Wesl
  ( compileWeslToSpirvWith
  , compileWeslToSpirvFileWith
  , compileWeslToSpirvBytesWith
  , compileWeslToSpirvWithDiagnostics
  , defaultCompileOptions
  )

-- Inline source
Right shader = compileWeslToSpirvWith defaultCompileOptions src

-- File-based (supports imports)
Right shaderFile <- compileWeslToSpirvFileWith defaultCompileOptions "shaders/main.wesl"

-- Bytes only
Right bytes = compileWeslToSpirvBytesWith defaultCompileOptions src

-- Diagnostics
Right (shaderDiag, diags) = compileWeslToSpirvWithDiagnostics defaultCompileOptions src
```

If you need a *deterministic bind order*, use `bindingPlan` and
sort by `(group, binding)` or use `bpBindings` directly.

### SDL (Example‑only, Not in the Library)
Spirdo stays SDL‑agnostic, but SDL integration can be very declarative. The
demo (`exe/Main.hs`) is a reference. The pattern below is intentionally short:

#### Declarative SDL wiring (minimal)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl
  ( PreparedShader(..)
  , ToUniform(..)
  , V4(..)
  , prepareShader
  , wesl
  )
import Spirdo.Wesl.Inputs (inputsFromPrepared, uniform, uniformSlots)

fragment = [wesl|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@fragment fn main(@builtin(position) p: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = p.xy / vec2(800.0, 600.0);
  return vec4(uv, 0.2, 1.0);
}
|]

data ParamsU = ParamsU { time_res :: V4 Float }
instance ToUniform ParamsU

-- 1) Prepare once
Right prepared = prepareShader fragment

-- 2) Create SDL shader using BindingPlan counts
let plan = psPlan prepared
    mkShaderInfo bytes stage =
      SDL_GPUShaderCreateInfo
        { shaderCode = bytesPtr, shaderCodeSize = bytesLen
        , shaderStage = stage
        , shaderNumSamplers = fromIntegral (length (bpSamplers plan))
        , shaderNumStorageTextures = fromIntegral (length (bpStorageTextures plan))
        , shaderNumStorageBuffers = fromIntegral (length (bpStorageBuffers plan))
        , shaderNumUniformBuffers = fromIntegral (length (bpUniforms plan))
        }

-- 3) Frame inputs (type‑safe + ordered)
Right si =
  inputsFromPrepared prepared
    ( uniform @"params" (ParamsU (V4 t 800 600 mode)) )

-- 4) Upload uniform (first/only in this shader)
case uniformSlots si of
  (u:_) -> sdlSetGPURenderStateFragmentUniforms rs (usBinding u) (usBytes u)
  [] -> pure ()
```

Notes:
- `inputsFromPrepared` already normalizes ordering; you can bind `siSamplers`,
  `siTextures`, etc. directly in `(group, binding)` order.
- `SamplerHandle`/`TextureHandle` are *your* runtime IDs, not shader bindings.

#### Vertex + Pipeline sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl
  ( PreparedShader(..)
  , prepareShader
  , vertexAttributes
  , wesl
  )

vertex = [wesl|
struct VSIn { @location(0) pos: vec2<f32>; };
struct VSOut { @builtin(position) pos: vec4<f32>; };
@vertex fn main(v: VSIn) -> VSOut {
  var o: VSOut;
  o.pos = vec4(v.pos, 0.0, 1.0);
  return o;
}
|]

Right vprep = prepareShader vertex
Right vattrs = vertexAttributes (shaderInterface (psShader vprep))

-- Use vattrs to fill SDL_GPUVertexInputState
-- Then create SDL_GPUGraphicsPipeline with vs+fs shaders.
```

#### Compute sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl (prepareShader, wesl)

compute = [wesl|
@group(0) @binding(0) var<storage, read_write> data: array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  data[i] = data[i] * 2u;
}
|]

Right cprep = prepareShader compute
-- Use psPlan cprep to size descriptor bindings and create SDL GPU shader.
```

### Binding Plans and Vertex Attributes (Advanced)
If you need explicit layout info, use `bindingPlan` and `vertexAttributes` on
the prepared shader’s interface:

```hs
plan = bindingPlan (shaderInterface (psShader prepared))
attrs = vertexAttributes (shaderInterface (psShader prepared))
```

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

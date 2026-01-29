# Spirdo

Haskell WESL compiler with an optional Slop/SDL3 demo that renders shader variants (optional SPIR-V output).

## Features
- WESL/WGSL coverage: control flow, functions, modules/imports, attributes, and diagnostics.
- Full binding/resource model: uniforms, storage buffers, samplers, textures, storage textures.
- Builtins: math, bit ops, packing, texture queries/sampling, derivatives.
- Override specialization constants with `SpecStrict` (validator‑friendly) and `SpecParity` (full WESL parity).
- Diagnostics surfaced as warnings (unused/shadowing/constant conditions/unreachable/duplicate cases).
- Typed interface reflection with binding metadata and ordering helpers.
- Binding plan metadata (counts + sorted bindings) for pipeline layout.
- Declarative input builder for type‑safe binding submission.
- Uniform packing with layout validation + Storable packing helpers.
- Vertex input reflection (`vertexAttributes`) for pipeline setup.
- Optional SPIR‑V validation (tests use `spirv-val` when available).
- Combined‑sampler emission by default, with opt‑in separate sampler mode when needed.
- Minimal `wesl.toml` package metadata parsing.

## Build and Run
Library/test builds (no demo):
```sh
cabal build
cabal test
```

Demo app (disabled by default):
```sh
cabal build -f spirdo-demo
cabal run -f spirdo-demo
```

The demo uses the Slop SDL3 renderer (`slop` dependency) and requires SDL3 to be
installed. Use left/right arrow keys to cycle fragment shader variants in the
demo window.

Set `SPIRDO_WRITE_SPV=1` to emit SPIR-V files (`fragment-*.spv`, `vertex*.spv`,
`compute*.spv`) for inspection.

## Example Usage (Quasiquoter)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString as BS
import Spirdo.Wesl (PreparedShader, SamplerBindingMode(..), preparedSpirv, wesl)

main :: IO ()
main = do
  let shader :: PreparedShader 'SamplerCombined iface
      shader = [wesl|
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]
  BS.writeFile "example.spv" (preparedSpirv shader)
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
  , samplerBindingMode = SamplerCombined
  }
```

Combined samplers are the default. If your backend expects **separate** sampler
bindings, set the option explicitly via `weslWith` / `prepareWeslWith`.

Set `cacheVerbose = True` to print basic timing output (cache read/write).
Set `timingVerbose = True` to print per‑phase compiler timings (parse/validate/emit).

## Declarative Binding Flow (Preferred)
The recommended path is: **prepare → inputsFromPrepared → submit**.
It’s concise, type‑safe, and renderer‑agnostic.

### Minimal, Declarative Inputs (Host-Agnostic)
Use the small input builder DSL to keep callsites short while preserving
type‑level checks on binding names and kinds. You can use `InputsCombined` /
`InputsSeparate` aliases if you want to pin the sampler mode explicitly.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl.Inputs
  ( InputsBuilder
  , InputsCombined
  , SamplerHandle(..)
  , ShaderInputs
  , TextureHandle(..)
  , inputsFromPrepared
  , sampledTexture
  , uniform
  )
import Spirdo.Wesl (PreparedShader, SamplerBindingMode(..), wesl)

shader :: PreparedShader 'SamplerCombined iface
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
  inputsFromPrepared shader $
    uniform @"params" (V4 0.0 0.0 0.0 0.0 :: V4 Float)
    <> sampledTexture @"tex0" (TextureHandle 2) (SamplerHandle 1)

-- If you want to pin the builder mode explicitly:
inputsCombined :: InputsCombined iface
inputsCombined =
  uniform @"params" (V4 0.0 0.0 0.0 0.0 :: V4 Float)
  <> sampledTexture @"tex0" (TextureHandle 2) (SamplerHandle 1)
```

Note: `SamplerHandle`/`TextureHandle` values are **your runtime resource IDs**, not shader binding indices. They must correspond to real resources in your renderer.

Storage textures use `StorageTextureHandle` to keep sampled vs. storage bindings
distinct at the type level.

`inputsFromPrepared` uses the shader interface to order everything by
`(group, binding, name)` and pack uniforms for you. The resulting `ShaderInputs`
lists (`inputsUniforms`, `inputsSamplers`, `inputsTextures`, ...) are ready to hand off to your
renderer.

If you need deterministic uniform ordering (by group/binding/name), use
`orderedUniforms` (or `uniformSlots` for a tiny, backend‑ready view).

`inputsFromPrepared` is pure; handle `Either` as you prefer.

`InputsBuilder` is parameterized by sampler mode (`'SamplerCombined` /
`'SamplerSeparate`), but you almost never write it explicitly—the mode is
inferred from the `PreparedShader` you pass to `inputsFromPrepared`.

### Algebraic Laws & Invariants
Spirdo keeps a small, explicit set of algebraic laws and runtime invariants.
These are enforced either by types or by validation in `inputsFromPrepared` /
`packUniform`:

Algebraic laws
- `InputsBuilder mode iface` is a **free monoid** over binding entries:
  - `(<>)` is associative
  - `mempty` is the identity
- Builder order does **not** affect final submission order because inputs are
  normalized by `(group, binding, name)` before use.

Core invariants (validated)
- `PreparedShader mode iface` always has a valid entry point (no‑stage shaders
  fail during `prepareShader`).
- Binding names are **unique** in a shader interface (duplicates are compile errors).
- `inputsFromPrepared` fails on:
  - missing binding names
  - duplicate binding entries
  - kind mismatches (e.g. sampler provided for a uniform)
- Combined sampler mode:
  - sampler bindings are **omitted** from the interface
  - `sampledTexture` must provide a sampler for every texture
- Uniform packing is **exact**:
  - missing struct fields are errors
  - extra struct fields are errors
  - vector/matrix sizes must match layout expectations
  - padding bytes are zeroed

Refinement via types
- `PreparedShader mode iface` ties the sampler mode to the shader at the type
  level, preventing mixed‑mode input builders.

Notes:
- Record field names must match the WESL struct field names (extra or missing
  fields are errors).
- `PreparedShader mode` is the preferred entrypoint; it caches stage, binding plan,
  and vertex attributes.

### Full End‑to‑End Example (Fragment + Combined Texture)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( PreparedShader
  , SamplerBindingMode(..)
  , ToUniform(..)
  , V4(..)
  , preparedInterface
  , preparedPlan
  , preparedSpirv
  , wesl
  )
import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , inputsFromPrepared
  , sampledTexture
  , uniform
  )

data ParamsU = ParamsU { time_res :: V4 Float }
instance ToUniform ParamsU

fragment :: PreparedShader 'SamplerCombined iface
fragment = [wesl|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var tex0: texture_2d<f32>;
@group(0) @binding(2) var samp0: sampler;
@fragment fn main(@builtin(position) p: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = p.xy / params.time_res.zw;
  let n = textureSample(tex0, samp0, uv).x;
  return vec4(n, n, n, 1.0);
}
|]

-- Pipeline setup (renderer‑agnostic):
let spv  = preparedSpirv fragment
let plan = preparedPlan fragment
-- Use plan counts to size descriptor bindings, then create pipeline.

-- Per‑frame bindings:
let inputs =
      inputsFromPrepared fragment $
        uniform @"params" (ParamsU (V4 t w h 0))
        <> sampledTexture @"tex0" (TextureHandle texId) (SamplerHandle sampId)
```

### Sampler Modes
Spirdo defaults to **combined samplers** (texture + sampler are bound together
on the host). If your backend wants separate sampler slots, opt into
`SamplerSeparate` via `CompileOptions`.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl (PreparedShader, SamplerBindingMode(..), defaultCompileOptions, weslWith)
import Spirdo.Wesl.Inputs
  ( InputsBuilder
  , SamplerHandle(..)
  , TextureHandle(..)
  , inputsFromPrepared
  , sampledTexture
  , uniform
  )

let opts = defaultCompileOptions { samplerBindingMode = SamplerSeparate }

shader :: PreparedShader 'SamplerSeparate iface
shader = [weslWith opts|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(2) var<uniform> params: Params;
@group(0) @binding(0) var tex0: texture_2d<f32>;
@group(0) @binding(1) var tex1: texture_2d<f32>;
@group(0) @binding(3) var samp0: sampler;
@group(0) @binding(4) var samp1: sampler;
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = frag_coord.xy / 800.0;
  let a = textureSample(tex0, samp0, uv).x;
  let b = textureSample(tex1, samp1, uv).x;
  return vec4(a, b, 0.0, 1.0);
}
|]

inputs =
  inputsFromPrepared shader $
    uniform @"params" (V4 0 0 0 0 :: V4 Float)
    <> sampledTexture @"tex0" (TextureHandle 1) (SamplerHandle 7)
    <> sampledTexture @"tex1" (TextureHandle 2) (SamplerHandle 7)
```

In combined mode, **sampler bindings are not part of the interface type**, so
you provide samplers via `sampledTexture`. If your backend expects sampler
slots starting at 0, keep *texture* bindings contiguous for predictability.

To opt into **separate** samplers for a shader, set:
`samplerBindingMode = SamplerSeparate`.

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
  ( PreparedShader
  , preparedInterface
  , preparedPlan
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
  :: PreparedShader vMode vIface
  -> PreparedShader fMode fIface
  -> Either String PipelineDesc
buildPipelineDesc vShader fShader = do
  vAttrs <- vertexAttributes (preparedInterface vShader)
  let fPlan = preparedPlan fShader
  pure PipelineDesc
    { pdVertexAttributes = vAttrs
    , pdUniformCount = length (bpUniforms fPlan)
    , pdSamplerCount = length (bpSamplers fPlan)
    , pdTextureCount = length (bpTextures fPlan)
    , pdStorageBufferCount = length (bpStorageBuffers fPlan)
    , pdStorageTextureCount = length (bpStorageTextures fPlan)
    }

-- Feeding inputs into your backend (pseudo‑API).
submitInputs
  :: ShaderInputs iface
  -> IO ()
submitInputs inputs = do
  -- upload uniforms (each has name/group/binding + bytes)
  mapM_ uploadUniform (inputsUniforms inputs)
  -- bind resources using your own handle types
  mapM_ bindSampler (inputsSamplers inputs)
  mapM_ bindTexture (inputsTextures inputs)
  mapM_ bindStorageBuffer (inputsStorageBuffers inputs)
  mapM_ bindStorageTexture (inputsStorageTextures inputs)
```

### Runtime Compilation (Optional)
If you need runtime compilation (e.g., loading `.wesl` from disk), use the
`prepare*` helpers:

```hs
import Spirdo.Wesl
  ( defaultCompileOptions
  , prepareWesl
  , prepareWeslFile
  , prepareWeslWithDiagnostics
  , preparedSpirv
  )

Right (SomePreparedShader prep) = prepareWesl src
bytes = preparedSpirv prep

Right (SomePreparedShader prepFile) <- prepareWeslFile "shaders/main.wesl"

Right (SomePreparedShader prepDiag, diags) =
  prepareWeslWithDiagnostics defaultCompileOptions src
```

If you need a *deterministic bind order*, use `preparedPlan` and `bpBindings`
from the prepared shader.

### SDL (Example‑only, Not in the Library)
Spirdo stays SDL‑agnostic, but SDL integration can be very declarative. The
demo (`exe/Main.hs`) is a Slop‑based reference; the pattern below shows a
direct SDL GPU wiring variant in minimal form:

#### Declarative SDL wiring (minimal)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl
  ( PreparedShader
  , SamplerBindingMode(..)
  , ToUniform(..)
  , V4(..)
  , preparedPlan
  , preparedSpirv
  , wesl
  )
import Spirdo.Wesl.Inputs (inputsFromPrepared, uniform, uniformSlots)

fragment :: PreparedShader 'SamplerCombined iface
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

-- 1) Prepare once (wesl returns PreparedShader 'SamplerCombined)
let prepared = fragment

-- 2) Create SDL shader using BindingPlan counts
let plan = preparedPlan prepared
    bytes = preparedSpirv prepared
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
- `inputsFromPrepared` already normalizes ordering; you can bind `inputsSamplers`,
  `inputsTextures`, etc. directly in `(group, binding)` order.
- `SamplerHandle`/`TextureHandle` are *your* runtime IDs, not shader bindings.
- If you’re using **Slop’s** Sprite/Shader2D override path, its ABI expects
  fragment uniforms in `@group(3)` and textures/samplers in `@group(2)`.
  Slop also auto-binds a `SlopGlobals` uniform at
  `@group(3) @binding(0)`; start your own uniforms at `@binding(1)`.

#### Vertex + Pipeline sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl
  ( PreparedShader
  , preparedInterface
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

let vprep = vertex
Right vattrs = vertexAttributes (preparedInterface vprep)

-- Use vattrs to fill SDL_GPUVertexInputState
-- Then create SDL_GPUGraphicsPipeline with vs+fs shaders.
```

#### Compute sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl (preparedPlan, wesl)

compute = [wesl|
@group(0) @binding(0) var<storage, read_write> data: array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  data[i] = data[i] * 2u;
}
|]

let cprep = compute
-- Use preparedPlan cprep to size descriptor bindings and create your graphics pipeline.
```

### Binding Plans and Vertex Attributes (Advanced)
If you need explicit layout info, use `preparedPlan` and `vertexAttributes`:

```hs
plan = preparedPlan prepared
attrs = vertexAttributes (preparedInterface prepared)
```

### Demo (exe only)
The demo executable is gated behind the `spirdo-demo` flag and is not part of
the library API. It uses Slop to render a full-screen quad with fragment shaders.


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
SPIRDO_WRITE_SPV=1 cabal run -f spirdo-demo
```

When enabled, files are written to the repo root:
- `fragment-*.spv` for each fragment variant
- `compute*.spv` for compute examples
- `vertex*.spv` for vertex examples

## API Reference (Public)
Public surface area is **`Spirdo.Wesl`** and **`Spirdo.Wesl.Inputs`**.
Everything else is internal.

### `Spirdo.Wesl` functions
Compilation / preparation
- `wesl` — Quasiquoter that compiles inline WESL at build time to `PreparedShader mode` (mode inferred from options).
- `wesl` — Quasiquoter that compiles inline WESL at build time to `PreparedShader mode` (mode inferred from options).
- `weslWith` — Same as `wesl`, but with explicit `CompileOptions` (features, cache, sampler mode).
- `prepareWesl` — Compile inline WESL at runtime to `SomePreparedShader`.
- `prepareWeslWith` — Runtime compile with explicit `CompileOptions`.
- `prepareWeslFile` — Compile a `.wesl` file (imports supported).
- `prepareWeslFileWith` — File compile with explicit `CompileOptions`.
- `prepareWeslWithDiagnostics` — Runtime compile returning `SomePreparedShader` + diagnostics.
- `prepareWeslFileWithDiagnostics` — File compile returning `SomePreparedShader` + diagnostics.
- `defaultCompileOptions` — Sensible defaults (combined samplers, caching on).

Prepared‑shader accessors
- `preparedSpirv` — SPIR‑V bytes for a `PreparedShader mode`.
- `preparedInterface` — `ShaderInterface` for reflection/binding info.
- `preparedStage` — Shader stage (vertex/fragment/compute).
- `preparedPlan` — `BindingPlan` (counts + sorted bindings).
- `preparedVertexAttributes` — Precomputed vertex attributes (only for vertex stages).

Reflection helpers
- `shaderStage` — Get stage from a `ShaderInterface`.
- `stageIO` — Combined stage IO (`StageIO`) if present.
- `stageInputs`, `stageOutputs` — Stage IO split into inputs/outputs.
- `vertexInputs`, `vertexOutputs` — Vertex‑stage IO helpers.
- `vertexAttributes` — Extract vertex attribute formats/locations.
- `pushConstantLayout` — Push‑constant layout if present.
- `specializableOverrides` — Overrides with runtime specialization IDs.

Uniform packing helpers (advanced)
- `uniform` — Build a `UniformValue` for manual packing.
- `packUniform` — Pack a `UniformValue` against a `TypeLayout`.
- `packUniformFrom` — Pack any `ToUniform` value against a layout.
- `validateUniformStorable` — Check that a `Storable` matches a layout.
- `packUniformStorable` — Pack a `Storable` into a layout‑compatible blob.

Package helpers
- `discoverPackageInfo` — Find and parse `wesl.toml` metadata.

### `Spirdo.Wesl.Inputs` functions
Input builder (host‑agnostic)
- `inputsFromPrepared` — Validate + normalize inputs against a `PreparedShader mode`.
- `emptyInputs` — Start from an empty `ShaderInputs` (rarely needed directly).
- `orderedUniforms` — Uniforms sorted by `(group, binding, name)`.
- `uniformSlots` — Compact `(group, binding, bytes)` view of uniforms.
- `inputsInterface` — Access the reflected `ShaderInterface`.
- `inputsUniforms`, `inputsSamplers`, `inputsTextures` — Access sorted inputs.
- `inputsStorageBuffers`, `inputsStorageTextures` — Access storage inputs.

InputsBuilder constructors
- `uniform` — Add a uniform binding (uses `ToUniform`).
- `sampledTexture` — Add a combined texture+sampler binding.
- `texture` — Add a texture binding (separate‑sampler mode).
- `sampler` — Add a sampler binding (separate‑sampler mode).
- `storageBuffer` — Add a storage buffer binding.
- `storageTexture` — Add a storage texture binding.

Note: `Spirdo.Wesl.uniform` builds a `UniformValue` (for packing).  
`Spirdo.Wesl.Inputs.uniform` builds a `ShaderInputs` entry.

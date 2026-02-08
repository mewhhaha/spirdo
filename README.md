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

## Build and Run
Library/test builds (no demo):
```sh
cabal build
cabal test
```

Demo app (in `examples/`):
```sh
cd examples
cabal build
cabal run
```

The demo uses the Slop SDL3 renderer (`slop` dependency) and requires SDL3 to be
installed. Use left/right arrow keys to cycle fragment shader variants in the
demo window.

Set `SPIRDO_WRITE_SPV=1` to emit SPIR-V files (`fragment-*.spv`, `vertex*.spv`,
`compute*.spv`) for inspection (run from `examples/`).

## Recommended Entry Points
Use the smallest API that fits your workflow:
- **Minimal runtime compile**: `Spirdo.Wesl` (bundle API) for SPIR‑V + binding metadata.
- **Typed binding submission**: `Spirdo.Wesl.Reflection` + `Spirdo.Wesl.Inputs`.
- **Uniform packing helpers**: `Spirdo.Wesl.Uniform` (used alongside either API).

## Example Usage (Compile-Time Shader)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString as BS
import Spirdo.Wesl.Reflection (Shader, SamplerBindingMode(..), shaderSpirv, weslShader)

main :: IO ()
main = do
  let shader :: Shader 'SamplerCombined iface
      shader = [weslShader|
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]
  BS.writeFile "example.spv" (shaderSpirv shader)
```

## Raw WESL Source (Quasiquoter)
`wesl` now returns raw WESL source (`String`), which you can feed into
runtime compilation or tooling.

```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl.Reflection (wesl)

src :: String
src = [wesl|
@fragment fn main() -> @location(0) vec4<f32> { return vec4(1.0); }
|]
```

## Inline Imports (Compile-Time)
Use `spirv` with a typed import list to resolve `import` statements from
in‑memory sources at compile time.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl.Reflection
  ( Shader
  , SamplerBindingMode(..)
  , Imports(..)
  , spirv
  , module_
  , imports
  , wesl
  )

somethingSrc :: String
somethingSrc = "@const let FOO: f32 = 1.0;"

shader :: Shader 'SamplerCombined iface
shader =
  $(spirv
      (imports <: module_ @"something" somethingSrc)
      [wesl|
        import something;
        @fragment fn main() -> @location(0) vec4<f32> { return vec4(FOO); }
      |]
   )
```
Note: the import list must match the modules used by the source (extra or
missing entries are errors).

## Example Usage (Runtime Compile)
```hs
import qualified Data.ByteString as BS
import Spirdo.Wesl (compile, shaderBindings, shaderSpirv, shaderStage, sourceText)

main :: IO ()
main = do
  let src = "@fragment fn main() -> @location(0) vec4<f32> { return vec4(1.0); }"
  result <- compile [] (sourceText src)
  case result of
    Left err -> putStrLn ("compile failed: " <> show err)
    Right bundle -> do
      BS.writeFile "example.spv" (shaderSpirv bundle)
      print (shaderStage bundle, shaderBindings bundle)
```

### WESL Syntax Notes (Shorthands)
Spirdo supports the WESL/WGSL shorthand syntax used in the playground:

- **Typed literal suffixes**: `1i`, `1u`, `1.0f`, `1.0h`
- **Shorthand vector/matrix types & ctors**: `vec2f`, `vec3u`, `vec2h`, `mat2x2f`, etc.
- **Scalar splat constructors**: `vec4<f32>(1.0)` / `vec4f(1.0)` fill all lanes

Example:
```wgsl
let a: vec2f = vec2f(1.0);
let b = vec3u(1u, 2u, 3u);
let c = vec2h(1.0h);
let d = vec4<f32>(1.0); // splat
```

Note: the public API is exposed from `Spirdo.Wesl` (minimal bundle API),
`Spirdo.Wesl.Reflection` (advanced reflection + raw compile), `Spirdo.Wesl.Uniform`,
and `Spirdo.Wesl.Inputs`. Internal modules are not part of the supported surface area.

### Compile-Time Cache & Timings
`weslShader` quasiquotes (from `Spirdo.Wesl.Reflection`) use an on-disk cache under
`dist-newstyle/.wesl-cache`. You can control it via `CompileOptions` helpers:

```hs
let opts =
      withSamplerMode SamplerCombined
    . withCache True
    . withCacheVerbose False
    . withTimingVerbose False
    $ defaultCompileOptions
```

Runtime compilation via `Spirdo.Wesl.compile` uses `[Option]`:
```hs
let opts = [OptSamplerMode SamplerSeparate, OptCache (CacheInDir "dist-newstyle/.wesl-cache")]
```

Combined samplers are the default. If your backend expects **separate** sampler
and texture bindings (e.g. explicit texture+sampler slots), set
`withSamplerMode SamplerSeparate` via `weslShaderWith` / `compileWith` in
`Spirdo.Wesl.Reflection`, or use `OptSamplerMode` in the runtime API.
Use separate mode when your renderer provides distinct bindings for textures and
samplers; keep combined mode for SDL‑style backends or when you want a single
binding per sampled texture.

CompileOptions helpers you’ll typically use (Reflection API):
- `withSamplerMode`
- `withOverrides`
- `withOverrideSpecMode`
- `withFeatures`
- `withCache`, `withCacheVerbose`, `withTimingVerbose`
- `withSpirvVersion`

### Fast dev mode (no API change)
For faster iteration without changing the API:
- Keep the cache on (default): `withCache True`.
- Use `weslShaderBatch`/`weslShaderBatchWith` when you have many shaders in a single module.
  Batch compilation runs in parallel; set `GHCRTS=-N` during build to use all cores.
- Avoid diagnostics in hot loops (use `compile`/`weslShader`, not the `*WithDiagnostics` variants).

Set `withCacheVerbose True` to print basic timing output (cache read/write).
Set `withTimingVerbose True` to print per‑phase compiler timings (parse/validate/emit).

## Declarative Binding Flow (Preferred)
For typed binding submission, use the Reflection API:
**weslShader/compileWith → inputsFor → submit**. It’s concise, type‑safe, and renderer‑agnostic.
If you only need SPIR‑V + minimal layout info, use the `Spirdo.Wesl` bundle API instead.

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
  , inputsFor
  , sampledTexture
  , uniform
  )
import Spirdo.Wesl.Reflection (Shader, SamplerBindingMode(..), weslShader)
import Spirdo.Wesl.Uniform (V4(..))

shader :: Shader 'SamplerCombined iface
shader = [weslShader|
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
  inputsFor shader $
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

`inputsFor` uses the shader interface to order everything by
`(group, binding, name)` and pack uniforms for you. The resulting `ShaderInputs`
lists (`inputsUniforms`, `inputsSamplers`, `inputsTextures`, ...) are ready to hand off to your
renderer.

If you need deterministic uniform ordering (by group/binding/name), use
`orderedUniforms`.

`inputsFor` is pure; handle `Either` as you prefer.

`InputsBuilder` is parameterized by sampler mode (`'SamplerCombined` /
`'SamplerSeparate`), but you almost never write it explicitly—the mode is
inferred from the `Shader` you pass to `inputsFor`.

### Algebraic Laws & Invariants
Spirdo keeps a small, explicit set of algebraic laws and runtime invariants.
These are enforced either by types or by validation in `inputsFor` /
`packUniform`:

Algebraic laws
- `InputsBuilder mode iface` is a **free monoid** over binding entries:
  - `(<>)` is associative
  - `mempty` is the identity
- Builder order does **not** affect final submission order because inputs are
  normalized by `(group, binding, name)` before use.

Core invariants (validated)
- `Shader mode iface` always has a valid entry point (no‑stage shaders
  fail during compilation).
- Binding names are **unique** in a shader interface (duplicates are compile errors).
- `inputsFor` fails on:
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
- `Shader mode iface` ties the sampler mode to the shader at the type
  level, preventing mixed‑mode input builders.

Notes:
- Record field names must match the WESL struct field names (extra or missing
  fields are errors).
- `Shader mode` is the preferred entrypoint; it caches stage, binding plan,
  and vertex attributes.

### Full End‑to‑End Example (Fragment + Combined Texture)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl.Reflection
  ( Shader
  , SamplerBindingMode(..)
  , shaderSpirv
  , shaderPlan
  , weslShader
  )
import Spirdo.Wesl.Uniform (ToUniform(..), V4(..))
import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , inputsFor
  , sampledTexture
  , uniform
  )

data ParamsU = ParamsU { time_res :: V4 Float }
instance ToUniform ParamsU

fragment :: Shader 'SamplerCombined iface
fragment = [weslShader|
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
let spv  = shaderSpirv fragment
let plan = shaderPlan fragment
-- Use plan counts to size descriptor bindings, then create pipeline.

-- Per‑frame bindings:
let inputs =
      inputsFor fragment $
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

import Spirdo.Wesl.Reflection (Shader, SamplerBindingMode(..), defaultCompileOptions, withSamplerMode, weslShaderWith)
import Spirdo.Wesl.Uniform (V4(..))
import Spirdo.Wesl.Inputs
  ( InputsBuilder
  , SamplerHandle(..)
  , TextureHandle(..)
  , inputsFor
  , sampledTexture
  , uniform
  )

let opts = withSamplerMode SamplerSeparate defaultCompileOptions

shader :: Shader 'SamplerSeparate iface
shader = [weslShaderWith opts|
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
  inputsFor shader $
    uniform @"params" (V4 0 0 0 0 :: V4 Float)
    <> sampledTexture @"tex0" (TextureHandle 1) (SamplerHandle 7)
    <> sampledTexture @"tex1" (TextureHandle 2) (SamplerHandle 7)
```

In combined mode, **sampler bindings are not part of the interface type**, so
you provide samplers via `sampledTexture`. If your backend expects sampler
slots starting at 0, keep *texture* bindings contiguous for predictability.

To opt into **separate** samplers for a shader, set:
`withSamplerMode SamplerSeparate`.

### Storage Buffers & Storage Textures (Builder)
```hs
import Spirdo.Wesl.Inputs
  ( inputsFor
  , storageBuffer
  , storageTexture
  , BufferHandle(..)
  , StorageTextureHandle(..)
  )

Right si =
  inputsFor prepared $
    storageBuffer @"particles" (BufferHandle 7)
    <> storageTexture @"outImage" (StorageTextureHandle 3)
```

### Uniform Packing Helpers (Advanced)
The high‑level path (`inputsFor`) already packs uniforms for you.
Use these only if you need raw layout control:
- `packUniformFrom` for `ToUniform` values
- `packUniformStorable` for pre‑laid‑out `Storable` records
These live in `Spirdo.Wesl.Uniform` (also re‑exported by `Spirdo.Wesl.Reflection`).

### Pipeline Integration (Renderer‑Agnostic)
Spirdo does **not** bind you to any graphics API. The idea is:
1) use the interface reflection to build *your* backend’s pipeline descriptor,
2) use `ShaderInputs` to feed uniform bytes + resource handles into your backend.

```hs
{-# LANGUAGE DataKinds #-}

import Spirdo.Wesl.Reflection
  ( Shader
  , BindingPlan(..)
  , VertexAttribute(..)
  , shaderInterface
  , shaderPlan
  , vertexAttributes
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
  :: Shader vMode vIface
  -> Shader fMode fIface
  -> Either String PipelineDesc
buildPipelineDesc vShader fShader = do
  vAttrs <- vertexAttributes (shaderInterface vShader)
  let fPlan = shaderPlan fShader
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
bundle API:

```hs
import Spirdo.Wesl
  ( compile
  , compileWithDiagnostics
  , shaderSpirv
  , sourceFile
  , sourceText
  )

Right bundle <- compile [] (sourceText src)
let bytes = shaderSpirv bundle

Right fileBundle <- compile [] (sourceFile "shaders/main.wesl")

Right (diagBundle, diags) <-
  compileWithDiagnostics [] (sourceText src)
```

If you need a *deterministic bind order*, use `shaderPlan` and `bpBindings`
from the shader.
That requires the Reflection API (`Spirdo.Wesl.Reflection`).

### SDL (Example‑only, Not in the Library)
Spirdo stays SDL‑agnostic, but SDL integration can be very declarative. The
demo (`examples/app/Main.hs`) is a Slop‑based reference; the pattern below shows a
direct SDL GPU wiring variant in minimal form:

#### Declarative SDL wiring (minimal)
```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl.Reflection
  ( Shader
  , SamplerBindingMode(..)
  , shaderPlan
  , shaderSpirv
  , weslShader
  )
import Spirdo.Wesl.Uniform (ToUniform(..), V4(..))
import Spirdo.Wesl.Inputs (inputsFor, orderedUniforms, uniform)

fragment :: Shader 'SamplerCombined iface
fragment = [weslShader|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@fragment fn main(@builtin(position) p: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = p.xy / vec2(800.0, 600.0);
  return vec4(uv, 0.2, 1.0);
}
|]

data ParamsU = ParamsU { time_res :: V4 Float }
instance ToUniform ParamsU

-- 1) Prepare once (weslShader returns Shader 'SamplerCombined)
let prepared = fragment

-- 2) Create SDL shader using BindingPlan counts
let plan = shaderPlan prepared
    bytes = shaderSpirv prepared
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
  inputsFor prepared
    ( uniform @"params" (ParamsU (V4 t 800 600 mode)) )

-- 4) Upload uniform (first/only in this shader)
case orderedUniforms si of
  (u:_) -> sdlSetGPURenderStateFragmentUniforms rs (uiBinding u) (uiBytes u)
  [] -> pure ()
```

Notes:
- `inputsFor` already normalizes ordering; you can bind `inputsSamplers`,
  `inputsTextures`, etc. directly in `(group, binding)` order.
- `SamplerHandle`/`TextureHandle` are *your* runtime IDs, not shader bindings.
- If you’re using **Slop’s** Sprite/Shader2D override path, its ABI expects
  fragment uniforms in `@group(3)` and textures/samplers in `@group(2)`.
  Slop also auto-binds a `SlopGlobals` uniform at
  `@group(3) @binding(0)`; start your own uniforms at `@binding(1)`.

#### Vertex + Pipeline sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl.Reflection
  ( Shader
  , shaderInterface
  , vertexAttributes
  , weslShader
  )

vertex = [weslShader|
struct VSIn { @location(0) pos: vec2<f32>; };
struct VSOut { @builtin(position) pos: vec4<f32>; };
@vertex fn main(v: VSIn) -> VSOut {
  var o: VSOut;
  o.pos = vec4(v.pos, 0.0, 1.0);
  return o;
}
|]

let vprep = vertex
Right vattrs = vertexAttributes (shaderInterface vprep)

-- Use vattrs to fill SDL_GPUVertexInputState
-- Then create SDL_GPUGraphicsPipeline with vs+fs shaders.
```

#### Compute sketch (SDL GPU)
```hs
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl.Reflection (weslShader)

compute = [weslShader|
@group(0) @binding(0) var<storage, read_write> data: array<u32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
  let i = gid.x;
  data[i] = data[i] * 2u;
}
|]

let cprep = compute
-- Use shaderPlan cprep to size descriptor bindings and create your graphics pipeline.
```

### Binding Plans and Vertex Attributes (Advanced)
If you need explicit layout info, use `shaderPlan` and `vertexAttributes`:

```hs
plan = shaderPlan prepared
attrs = vertexAttributes (shaderInterface prepared)
```

### Demo (examples only)
The demo executable lives in `examples/` and is not part of
the library API. It uses Slop to render a full-screen quad with fragment shaders.


## Example Shaders in the Demo
Fragment variants in `examples/app/Main.hs` (left/right to switch):
- Gradient Bloom
- Circle Pulse
- Spectrum Shift
- Sine Waves
- Checker Warp
- Ripple Caustics
- Plasma Storm
- Vignette Glow
- Noise Flow
- Swirl Vortex
- Metaballs
- Kaleidoscope

Compute examples emitted when SPIR-V output is enabled:
- `compute-1.spv`: storage buffer + storage texture sample
- `compute-2.spv`: particle update on runtime array

Vertex examples emitted when SPIR-V output is enabled:
- `vertex-1.spv`: passthrough quad
- `vertex-2.spv`: fullscreen triangle (vertex_index)

## SPIR-V Outputs
SPIR-V output is opt-in. Set `SPIRDO_WRITE_SPV=1` before running the demo
(from `examples/`):
```
SPIRDO_WRITE_SPV=1 cabal run
```

When enabled, files are written to the current working directory:
- `fragment-*.spv` for each fragment variant
- `compute-*.spv` for compute examples
- `vertex-*.spv` for vertex examples

## Release Checklist (v1)
- `cabal build`
- `cabal test`
- `cd examples && cabal build`
- `cd examples && cabal run` (cycle all fragment variants)
- Verify README examples compile (bundle + reflection snippets)

## API Reference (Public)
Public surface area:
- `Spirdo.Wesl` — minimal bundle API (runtime compile)
- `Spirdo.Wesl.Reflection` — advanced reflection + raw compile + quasiquoter
- `Spirdo.Wesl.Uniform` — uniform packing helpers
- `Spirdo.Wesl.Inputs` — typed input builder DSL

### `Spirdo.Wesl`
Compilation
- `compile` — Compile a `Source` (inline or file) to `ShaderBundle` (IO).
- `compileWithDiagnostics` — Same, but returns diagnostics.
- `sourceText`, `sourceFile` — Construct a `Source` without exposing constructors.
- `renderCompileError` — Format a `CompileError` (includes source snippet when available).
- `renderCompileErrorWithSource` — Format a `CompileError` using explicit source text.
- `Option` constructors (`Opt*`) — sampler mode, overrides, cache, entry point, etc.

Bundle accessors
- `shaderSpirv` — SPIR‑V bytes for a bundle.
- `shaderStage` — Stage for a bundle.
- `shaderBindings` — Binding list (name/kind/group/binding).
- `shaderVertexAttributes` — Vertex attributes (empty for non‑vertex stages).
- `shaderOverrides` — Override names + spec IDs.
- `shaderSamplerMode` — Combined vs separate sampler mode.
- `shaderWorkgroupSize` — Workgroup size (compute only).

### `Spirdo.Wesl.Reflection`
Compile + quasiquote
- `wesl` (raw source)
- `weslShader`, `weslShaderWith`
- `weslShaderBatch`, `weslShaderBatchWith` (legacy aliases: `weslBatch*`)
- `spirv`, `spirvWith`, `spirvNamed` (compile-time with inline imports)
- `Import`, `Imports`, `imports`, `importsNil`, `module_`, `moduleText`, `import_`, `importText`, `(:>)`, `(<:)`
- `compile`, `compileWith`, `compileWithDiagnostics`
- `compileFile`, `compileFileWith`, `compileFileWithDiagnostics`
- `CompileOptions` + helpers (`withSamplerMode`, `withOverrides`, `withFeatures`, ...)
- `renderCompileError`, `renderCompileErrorWithSource` — helpers for error formatting

Shader accessors + reflection
- `shaderSpirv`, `shaderInterface`, `shaderPlan`, `shaderStageCached`, `shaderVertexAttributes`, `shaderSource`
- `shaderStage`, `vertexAttributes`, `pushConstantLayout`, `specializableOverrides`

### `Spirdo.Wesl.Uniform`
Uniform packing helpers
- `uniform`, `packUniform`, `packUniformFrom`, `validateUniformStorable`, `packUniformStorable`
- `V2/V3/V4`, `M2/M3/M4/M3x4/M4x3`, `Half`, `ToUniform`, `UniformValue`

### `Spirdo.Wesl.Inputs`
Input builder (host‑agnostic)
- `inputsFor` — Validate + normalize inputs against a `Shader mode`.
- `emptyInputs` — Start from an empty `ShaderInputs` (rarely needed directly).
- `orderedUniforms` — Uniforms sorted by `(group, binding, name)`.
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

Note: `Spirdo.Wesl.Uniform.uniform` builds a `UniformValue` (for packing).  
`Spirdo.Wesl.Inputs.uniform` builds a `ShaderInputs` entry.

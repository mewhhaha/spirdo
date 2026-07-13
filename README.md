# Spirdo

Spirdo compiles a substantial, explicitly tested WGSL/WESL subset to SPIR-V.
It works at runtime or through Template Haskell, reflects entry points and
resources, and provides typed, renderer-agnostic resource submission. It does
not select or wrap a graphics API.

The public modules are:

- `Spirdo.Wesl`: small runtime compiler returning an abstract `ShaderBundle`.
- `Spirdo.Wesl.Inputs`: validated, mode-indexed resource-input builders.
- `Spirdo.Wesl.Reflection`: advanced/static compilation, typed `Shader`s, and
  full reflection.
- `Spirdo.Wesl.Uniform`: layout-aware host values and packing.

## Status and boundaries

The implemented subset includes modules, functions, control flow, composite
types, stage IO, buffers, textures and samplers, derivatives, atomics,
overrides, and the builtins covered by the corpus. This is not a claim of full
WGSL or WESL conformance: the [parity manifest](docs/parity.md) is the
authoritative record of accepted, rejected, and expected-failure cases.

- Diagnostics use positions where available, but some AST and emission errors
  are still locationless.
- Storage buffers support `read` and `read_write`, not write-only access.
- Pointer parameters preserve pointer identity in `function` and `private`
  address spaces. `workgroup` and `storage` pointer parameters additionally
  require `enable unrestricted_pointer_parameters;` in the declaring module
  and compiler authorization through `OptEnableFeature` or `withFeatures`.
  `uniform` pointer parameters and all pointer return types are rejected because
  SPIR-V Logical addressing cannot represent their semantics without changing
  the program. Partial-pointer arguments are supported only for `storage` and
  `workgroup`; `function` and `private` pointer arguments must identify a whole
  variable root.
- `@workgroup_size` accepts constant expressions, including overrides. An
  override-dependent size needs SPIR-V 1.2 or later; the default is 1.6.
  Overrides without initializers compile, but reflection reports no known
  default size and the host must specialize every dimension to a positive
  value before pipeline creation or dispatch.
- Unsuffixed floating-point literals and scalar constant expressions retain
  binary64 `AbstractFloat` precision until an f32 or f16 boundary; target
  materialization uses checked, round-to-nearest-even conversion.

## Build, test, and demo

Use a supported GHC/Cabal toolchain (the package is tested with GHC 9.12 and
9.14):

```sh
cabal build all --enable-tests --enable-benchmarks
cabal test spirdo-tests --test-show-details=direct
```

[`just`](https://github.com/casey/just) provides the common workflows:

```sh
just build
just test
just bench
just demo
just game
```

The optional examples require SDL3 and the native SDL3 image, TTF, and mixer
libraries used by `slop`; the
[examples CI job](https://github.com/mewhhaha/spirdo/blob/main/.github/workflows/parity-tests.yml)
shows the Ubuntu setup. `just demo-spv` writes the gallery's SPIR-V outputs for
inspection. The examples project selects GHC 9.12.2 because the pinned Slop
revision currently requires `base-4.21`.

`just game` runs Crystal Run, a small rasterized 3D example whose vertex and
fragment shaders are compiled by Spirdo. Move the ship with WASD or the arrow
keys, collect all five crystals, and press R to reset. The scene uses
procedural meshes, so it needs no external model assets.

`just game-capture` builds the game, runs it under Xvfb, and writes a screenshot
to `/tmp/spirdo-game.png`. This Linux/X11 smoke check requires Xvfb, FFmpeg,
SDL3, and a Vulkan driver. On a machine without a physical GPU, set
`SPIRDO_VULKAN_ICD` to an absolute Lavapipe ICD manifest path. Set
`SPIRDO_VULKAN_LIB_DIR` as well only when Lavapipe's shared libraries are not on
the system loader path.

## Runtime compilation

Use `sourceNamed` for generated or embedded text; its name appears in errors.
Inline sources do not resolve filesystem imports. Use `sourceFile` for a
top-level file when imports should be resolved from the filesystem.

```hs
import qualified Data.ByteString as BS
import Spirdo.Wesl
  ( compile
  , renderCompileError
  , shaderBindings
  , shaderSpirv
  , sourceFile
  , sourceNamed
  )

main :: IO ()
main = do
  result <- compile [] (sourceNamed "triangle.wesl" triangleSource)
  case result of
    Left err -> putStrLn (renderCompileError err)
    Right bundle -> do
      BS.writeFile "triangle.spv" (shaderSpirv bundle)
      print (shaderBindings bundle)

  fileResult <- compile [] (sourceFile "shaders/postprocess.wesl")
  print fileResult

triangleSource :: String
triangleSource =
  "@fragment fn main() -> @location(0) vec4<f32> { return vec4(0.2, 0.5, 0.9, 1.0); }"
```

`compileWithDiagnostics` returns a bundle plus non-fatal diagnostics. Runtime
`[Option]` controls entries, overrides, sampler mode, features, SPIR-V version,
and timing. It intentionally has no cache constructors or cache policy.

## Files, packages, and imports

`sourceFile` and Reflection's `compileFile` select an input by trying the exact
path, then `.wesl`, then `.wgsl`. They discover the nearest `wesl.toml` above
the selected file. The supported manifest subset is deliberately small:

```toml
[package]
edition = "2026_pre" # required
root = "./shaders"   # optional; must be relative, and this is the default

[dependencies]
math = { path = "../math" }
```

Path dependencies are relative to the declaring manifest and may themselves
have path dependencies. Import a dependency through its declared alias, for
example `import math::noise;`. Inside any package, `package::` refers to that
package's own root. Version-only and package-manager dependencies are parsed
only far enough to report that resolution is unsupported; Spirdo is not a
general TOML parser or package manager.

Reflected imported resources and overrides use stable source-qualified names:
`package::common::params` for another module in the current package and
`math::noise::scale` for a direct dependency. Transitive dependency names keep
an explicit package/module boundary, for example
`render::math::package::noise::scale`. These names do not depend on checkout
paths and are also the canonical keys for host-supplied override values.
Canonical reflected keys take precedence. Other unmarked shorthand is accepted
only when it identifies one override; an ambiguous key reports the canonical
candidates. Dependency aliases must be valid non-keyword WESL import names.

Canonical containment checks reject an input, import, or symlink that escapes
its package root. Without a manifest, relative file imports work from the input
directory. `sourceNamed` and typed TH `imports` are in-memory facilities and do
not perform filesystem package resolution.

Filesystem inputs are bounded before parsing: one source is limited to 1 MiB
of decoded characters and 4 MiB of UTF-8, and one manifest to 256 KiB. A file
import graph may contain at most 256 distinct modules, depth 64, and 16 MiB of
decoded source in total. A package graph has the same count and depth limits
and a 1 MiB cumulative manifest budget. Canonical caching counts cycles and
diamond dependencies once.

## Compile at build time

`wesl` is a raw-source quasiquoter. `spirv` is the Template Haskell compiler
entry point; provide `imports` for its inline module map.

```hs
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import Spirdo.Wesl.Reflection
  ( defaultCompileOptions
  , imports
  , shaderSpirv
  , spirv
  , wesl
  )

fragmentSpirv =
  shaderSpirv $(spirv defaultCompileOptions imports [wesl|
    @fragment
    fn main() -> @location(0) vec4<f32> {
      return vec4(0.2, 0.5, 0.9, 1.0);
    }
  |])

main :: IO ()
main = BS.writeFile "fragment.spv" fragmentSpirv
```

For inline modules, append `module_ @"name" source` to `imports` with `(<:)`.
The map must match the source imports exactly. The removed `weslWith`,
`weslShader*`, and `weslBatch*` APIs have no replacement beyond this `spirv`
path.

### Compiler cache

Caching is an advanced/TH-only compiler optimization, configured with
`withCache`, `withCacheDir`, and `withCacheVerbose` on `CompileOptions`. It is
versioned and atomically written, and validates bounded artifact shape,
reflection, SPIR-V, and exact input identity before reuse.

It is local compiler state, not an authenticated artifact format. Do not
commit, download, or share cache directories. Cache files can contain exact
source text and have no authenticity guarantee; delete them when their origin
is uncertain.

## Reflection and typed inputs

Runtime bundles expose compact binding metadata. Reflection adds full layouts,
binding plans, vertex attributes, and typed resource names. `Shader` is
compiler-created (its constructor is hidden); `ShaderBundle` is abstract and
has no `Read` instance.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl.Inputs
  ( SamplerHandle(..)
  , TextureHandle(..)
  , inputsFor
  , sampledTexture
  , uniform
  )
import Spirdo.Wesl.Reflection
  ( BindingPlan(..)
  , BindingSlotCount(..)
  , bindingInfoFor
  , bindingSlotCounts
  , defaultCompileOptions
  , imports
  , shaderInterface
  , shaderPlan
  , spirv
  , wesl
  )
import Spirdo.Wesl.Uniform (V4(..))

shader = $(spirv defaultCompileOptions imports [wesl|
  @group(0) @binding(0) var<uniform> params: vec4<f32>;
  @group(0) @binding(1) var tex: texture_2d<f32>;
  @group(0) @binding(2) var samp: sampler;

  @fragment
  fn main(@builtin(position) position: vec4<f32>) -> @location(0) vec4<f32> {
    return textureSample(tex, samp, position.xy / params.zw);
  }
|])

paramsBinding = bindingInfoFor "params" (shaderInterface shader)
slotCounts = bindingSlotCounts (shaderPlan shader).bpBindings
-- [BindingSlotCount { bscGroup = 0, bscSlots = 2 }]

frameInputs =
  inputsFor shader $
    uniform @"params" (V4 0 0 1280 720 :: V4 Float)
      <> sampledTexture @"tex" (TextureHandle 42) (SamplerHandle 7)
```

`bindingSlotCounts :: [BindingInfo] -> [BindingSlotCount]` keeps descriptor
groups separate. For each group, `bscSlots` is the highest binding plus one,
so sparse bindings `0` and `7` require eight slots. `bscSlots :: Word64`
represents the full `Word32` binding range without overflow. If a host only
supports one descriptor group, use
`singleGroupBindingSlotCount :: [BindingInfo] -> Either String (Maybe BindingSlotCount)`.

Inputs builders are opaque and `inputsFor` validates missing, duplicate, and
wrong-kind bindings before normalizing successful inputs. For a shader with no
bindings, the empty builder is simply `mempty`:

```hs
emptyShaderInputs = inputsFor shader mempty
```

There is no public `emptyInputs` or `emptyInputsUnchecked` escape hatch.

### Combined and separate samplers

Combined samplers are the default. In `SamplerCombined` mode, use
`sampledTexture @"tex" textureHandle samplerHandle`. For independent texture
and sampler slots, select `SamplerSeparate` with `withSamplerMode` (advanced)
or `OptSamplerMode` (runtime), then use `texture` and `sampler`. A mismatched
builder cannot typecheck through the normal API.

## Layout and uniform packing

Prefer `inputsFor` and `uniform`, or use `packUniformFrom` with a reflected
`TypeLayout`. They pack `ToUniform` values according to WESL layout, including
field offsets and padding. The old `validateUniformStorable` and
`packUniformStorable` names remain deprecated aliases; their explicit
`Unchecked` replacements check only size and alignment and cannot prove a host
ABI's offsets, padding, representation, byte order, or portability.

Layout reflection follows WGSL's natural alignment and size rules: for example,
a `vec3`'s natural size differs from its alignment. Runtime arrays reflect size
zero with a nonzero stride; their execution-time footprint is supplied by the
bound buffer, so host allocation and uniform-packing helpers reject them.
They are supported only as a direct storage-buffer type or the final member of
a storage-buffer struct. Uniform buffers cannot contain runtime arrays.

Storage/uniform buffer emission applies Vulkan-facing decorations: buffer store
types receive the required `Block` wrapper/decorations, matrices receive
`ColMajor` and `MatrixStride`, and f16 buffer layouts request the needed SPIR-V
capabilities. Under legacy Vulkan uniform layout rules, a natural standard
layout that violates those rules requires
`enable uniform_buffer_standard_layout;` in the module that declares the
binding, and the compile options must authorize it with
`OptEnableFeature "uniform_buffer_standard_layout"`.

## Overrides and validation

`SpecStrict` is the default and is validator-friendly: derived and
composite/non-scalar-literal specialization instructions omit `SpecId`.
`SpecParity` assigns IDs for WESL parity, but some `spirv-val` versions reject
derived or composite forms. Use it only with a toolchain that accepts them;
validator failure in this mode is not a compiler acceptance result.

SPIR-V's Shader environment permits only a restricted set of specialization
operations. If a derived override uses an unsupported operation, including
floating-point arithmetic, supply its final value through `overrideValues`.
An override without an initializer is emitted with SPIR-V's required
zero-valued structural placeholder. That zero is not a usable pipeline default.
When such an override contributes to `@workgroup_size`, reflection returns no
known/default size; specialize it to a positive value before pipeline creation
or dispatch. Other uses remain a host-integration obligation.

The normal suite runs without external tools when unavailable. Require both
validators explicitly when installed:

```sh
just validate
# equivalent: SPIRDO_REQUIRE_VALIDATORS=1 cabal test spirdo-tests --test-show-details=direct
```

For architecture, contribution, parity, migration, and performance details,
see [architecture](docs/architecture.md), [contributing](docs/contributing.md),
[parity](docs/parity.md), [migration](MIGRATION.md), and
[performance](PERF_REFACTOR_REPORT.md).

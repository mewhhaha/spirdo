---
name: use-spirdo
description: Compile WESL or WGSL with Spirdo, inspect reflected shader interfaces, build type-checked resource inputs, pack uniforms, select sampler and override modes, and diagnose or validate SPIR-V integration. Use for Spirdo shader authoring, runtime or Template Haskell compilation, renderer binding setup, specialization, and compiler troubleshooting.
---

# Use Spirdo

Keep application code on these public modules:

- `Spirdo.Wesl` for runtime compilation and compact bundles.
- `Spirdo.Wesl.Reflection` for typed Template Haskell shaders and full metadata.
- `Spirdo.Wesl.Inputs` for named resource submission.
- `Spirdo.Wesl.Uniform` for layout-aware host value packing.

Treat every other `Spirdo.Wesl.*` module as internal.

## Choose the compilation path

Use `spirv` when source is known at Haskell compile time. Let the splice infer
the concrete type-level interface; do not write a polymorphic `iface` signature.

```haskell
{-# LANGUAGE DataKinds #-}
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
  ( defaultCompileOptions
  , imports
  , spirv
  , wesl
  )

shader = $(spirv defaultCompileOptions imports [wesl|
struct Params { tint: vec4f, }
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var tex: texture_2d<f32>;
@group(0) @binding(2) var samp: sampler;

@fragment
fn main() -> @location(0) vec4f {
  return textureSample(tex, samp, vec2f(0.5)) * params.tint;
}
|])

buildInputs paramsValue =
  inputsFor shader
    ( uniform @"params" paramsValue
        <> sampledTexture @"tex" (TextureHandle 7) (SamplerHandle 9)
    )
```

Omit the explicit `buildInputs` signature when the inferred concrete interface
is more convenient; the important boundary is `inputsFor`.

Use the runtime API for files, generated text, editor reloads, or user input:

```haskell
import Spirdo.Wesl
  ( compile
  , renderCompileError
  , shaderBindings
  , sourceNamed
  )

compileEditorSource source = do
  result <- compile [] (sourceNamed "editor.wesl" source)
  pure $ case result of
    Left err -> Left (renderCompileError err)
    Right bundle -> Right (shaderBindings bundle)
```

Give inline sources meaningful names so diagnostics identify their origin.
`sourceNamed` is inline-only and cannot resolve filesystem imports. Use
`sourceFile` for a top-level file with filesystem/package imports. File
selection tries the exact path, then `.wesl`, then `.wgsl`; a nearby
`wesl.toml` enables its supported path-dependency and `package::` resolution.
Treat size/count/depth errors as deliberate compiler boundaries, not transient
IO failures: a source is limited to 1 MiB decoded characters, and filesystem
module/package graphs to 256 nodes and depth 64.

## Match the sampler mode

- Keep the default `SamplerCombined` when the backend binds a sampled texture
  as one host resource. Use `sampledTexture`.
- Select `SamplerSeparate` when texture and sampler slots are independent. Use
  `texture` and `sampler` separately.
- Do not coerce an input builder between modes. The builder types reject mode
  mismatches before runtime.

Keep the mode used for compilation, pipeline creation, and input submission
identical.

Builders are opaque and `inputsFor` validates missing, duplicate, and wrong-kind
bindings. For a shader with no bindings, pass `mempty` to `inputsFor`; there is
no public unchecked empty-input constructor.

## Pack uniforms

Derive `Generic`, define `ToUniform`, and use `uniform @"name" value` or
`packUniformFrom`. Host record field names must match the shader struct:

```hs
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Spirdo.Wesl.Uniform (ToUniform, V4(..))

data Params = Params { tint :: V4 Float }
  deriving (Generic)

instance ToUniform Params
```

Prefer this layout-aware path.

Use `packUniformStorableUnchecked` only after independently proving field
offsets, padding, scalar representation, byte order, size, and alignment for
every target ABI. Its size/alignment check cannot prove full shader layout
compatibility.

## Apply options

- Select a named entry with `withEntryPoint` or `OptEntryPoint`.
- Supply specialization values with `withOverrides` or `OptOverrides`.
- A workgroup-size override without a source initializer compiles, but its
  reflected size has no default. Specialize it to a positive value before
  pipeline creation or dispatch; the emitted zero is only SPIR-V's required
  structural placeholder.
- Supply the final value for a derived override whose expression uses float
  arithmetic or another operation unavailable to Shader `OpSpecConstantOp`.
- Keep `SpecStrict` for validator-compatible output; derived and
  composite/non-scalar-literal specialization instructions omit `SpecId`.
  Use `SpecParity` only for toolchains that accept IDs on derived or composite
  specialization forms.
- Enable source features in both compiler options and source directives where
  WGSL requires both.

Pointer parameters preserve identity for `function` and `private` address
spaces. Gated `workgroup` and `storage` parameters are supported; uniform
pointer parameters and pointer returns are compile errors. Do not replace them
with value parameters unless that is an intentional shader API change.

Runtime `[Option]` has no cache controls. For TH compilation only, configure
the local compiler cache through `CompileOptions` with `withCache`,
`withCacheDir`, and `withCacheVerbose`. Never commit, download, or share its
directory: cache entries are versioned local optimization data, may contain
source text, and are not authenticated artifacts.

Treat unknown entry names, override names, and invalid option combinations as
source-integration errors; do not silently fall back.

## Verify changes

Run the project checks after changing shader semantics or integration:

```sh
cabal build all --enable-tests --enable-benchmarks
cabal test spirdo-tests --test-show-details=direct
SPIRDO_REQUIRE_VALIDATORS=1 cabal test spirdo-tests --test-show-details=direct
```

The validator-required command needs `spirv-val` and `naga` on `PATH`. For a
focused regression, pass `--test-options='--match SUBSTRING'` to `cabal test`.

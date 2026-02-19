---
name: use-spirdo
description: Use this skill when compiling WESL with Spirdo, wiring typed shader inputs, choosing sampler mode, applying overrides, and validating shader integration without binding mistakes.
---

# Use Spirdo Properly

## When to use this skill

Use this when the task involves:

- Writing or debugging WESL compiled by Spirdo.
- Integrating shader bindings in host code.
- Choosing combined vs separate sampler mode.
- Applying override specialization constants.
- Verifying compile-time or runtime shader flows.

## Preferred path (default)

1. Compile with `Spirdo.Wesl.Reflection` (`weslShader`/`weslShaderWith`) when shader source is known at compile time.
2. Build bindings with `Spirdo.Wesl.Inputs` using binding names, not numeric slots.
3. Use `inputsFor` to validate and normalize binding submission.
4. Feed `orderedUniforms`/`inputs*` outputs into your renderer.

Use runtime compile (`Spirdo.Wesl.compile`) only when source must come from files or external input.

## Public API boundaries

Stay on public modules only:

- `Spirdo.Wesl`
- `Spirdo.Wesl.Reflection`
- `Spirdo.Wesl.Inputs`
- `Spirdo.Wesl.Uniform`

Do not depend on internal modules; treat them as unstable.

## Golden integration pattern

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

import Spirdo.Wesl.Reflection (Shader, SamplerBindingMode(..), weslShader)
import Spirdo.Wesl.Inputs
  ( InputsCombined
  , InputsError
  , ShaderInputs
  , ToUniform
  , inputsFor
  , uniform
  , sampledTexture
  , TextureHandle(..)
  , SamplerHandle(..)
  )

shader :: Shader 'SamplerCombined iface
shader = [weslShader|
struct Params { tint: vec4f; };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var tex0: texture_2d<f32>;
@group(0) @binding(2) var samp0: sampler;
@fragment fn main() -> @location(0) vec4<f32> {
  return textureSample(tex0, samp0, vec2f(0.5, 0.5)) * params.tint;
}
|]

buildInputs
  :: ToUniform params
  => params
  -> Either InputsError (ShaderInputs iface)
buildInputs paramsValue = do
  let b :: InputsCombined iface
      b =
        uniform @"params" paramsValue
          <> sampledTexture @"tex0" (TextureHandle 7) (SamplerHandle 9)
  inputsFor shader b
```

## Sampler mode rules

- `SamplerCombined` (default): sampler bindings are omitted from interface; use `sampledTexture`.
- `SamplerSeparate`: sampler and texture are independent; use `texture` and `sampler`.
- Keep host renderer descriptor layout consistent with the compile mode.
- In combined mode, keep texture bindings contiguous for predictable implicit sampler slots.

## Binding correctness rules

- `uniform @"name"` and other builders bind by name; names must match shader interface.
- Resource handles are runtime IDs, not WGSL binding indices.
- `inputsFor` rejects:
  - duplicate binding entries
  - missing names
  - kind mismatches (for example texture supplied where uniform expected)
  - missing sampler pairings in combined mode
- Prefer `orderedUniforms` for deterministic upload order.

## Overrides and specialization

- Default mode is `SpecStrict` (validator-friendly).
- Use `SpecParity` only when full WESL parity is required and validator limitations are acceptable.
- Runtime API uses options like:
  - `OptOverrides [(String, OverrideValue)]`
  - `OptOverrideSpecMode SpecStrict` or `OptOverrideSpecMode SpecParity`
- Reflection API uses `CompileOptions` helpers:
  - `withOverrides`
  - `withOverrideSpecMode`

## Runtime compile pattern

```haskell
import Spirdo.Wesl
  ( compile
  , sourceFile
  , shaderBindings
  , shaderSpirv
  , Option(..)
  , OverrideSpecMode(..)
  )

main :: IO ()
main = do
  result <- compile [OptOverrideSpecMode SpecStrict] (sourceFile "shaders/main.wesl")
  case result of
    Left err -> print err
    Right bundle -> do
      print (shaderBindings bundle)
      print (length (shaderSpirv bundle))
```

## Verification checklist

Run after changing shader semantics, bindings, or options:

```bash
cabal build
cabal test
cd examples && cabal build
cd examples && cabal run
```

Optional SPIR-V file emission during demo runs:

```bash
SPIRDO_WRITE_SPV=1 cabal run
```

## Fast iteration notes

- Use `weslShaderBatch`/`weslShaderBatchWith` when many quasiquoted shaders live in one module.
- Keep diagnostics off in hot loops unless you are actively debugging.
- For parallel compile-time work, set RTS capabilities (`GHCRTS=-N`).

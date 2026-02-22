# Spirdo API Migration

This guide maps the old Shader‑centric public API to the new bundle‑first surface.

## Module split
- `Spirdo.Wesl` now returns `ShaderBundle` and exposes only minimal metadata.
- Advanced reflection/quasiquoter/`Shader` types live in `Spirdo.Wesl.Reflection`.
- Uniform packing helpers live in `Spirdo.Wesl.Uniform`.
- Inputs remain in `Spirdo.Wesl.Inputs`.

## Which API should I use?
- **Just SPIR-V + minimal metadata**: `Spirdo.Wesl` (bundle API).
- **Typed binding submission / reflection**: `Spirdo.Wesl.Reflection` + `Spirdo.Wesl.Inputs`.
- **Uniform packing**: `Spirdo.Wesl.Uniform` (used alongside either API).

## Runtime compilation (bundle API)
Old (inline):
- `compile (SourceInline "<inline>" src) :: Either CompileError SomeShader`

New:
- `compile [] (sourceText src) :: IO (Either CompileError ShaderBundle)`

Old (file):
- `compileFile "shader.wesl" :: IO (Either CompileError SomeShader)`

New:
- `compile [] (sourceFile "shader.wesl") :: IO (Either CompileError ShaderBundle)`

Diagnostics:
- `compileWithDiagnostics [] (sourceText src)`

## Shader accessors
Old (`Shader`):
- `shaderSpirv`, `shaderInterface`, `shaderPlan`, `shaderStageCached`, `shaderVertexAttributes`

New:
- Bundle accessors: `shaderSpirv`, `shaderStage`, `shaderBindings`, `shaderVertexAttributes`,
  `shaderOverrides`, `shaderSamplerMode`, `shaderWorkgroupSize`
- Full reflection: import `Spirdo.Wesl.Reflection` and keep using the old accessors.
- New accessor: `shaderSource` returns the original source (name + text) when available.

## Quasiquoter
- `wesl` / `weslWith` are now in `Spirdo.Wesl.Reflection` and return raw source (`String`).
- Compile-time shader output now uses `spirv` with explicit `CompileOptions`.
- Pass `imports` / `importsNil` when no inline imports are needed.

## Options
- `Option` constructors (`Opt*`) are unchanged and work with `Spirdo.Wesl.compile`.
- `CompileOptions` helpers (`withSamplerMode`, `withOverrides`, ...) remain available
  in `Spirdo.Wesl.Reflection`.

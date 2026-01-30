# Spirdo API Migration

This guide maps the old Shader‑centric public API to the new bundle‑first surface.

## Module split
- `Spirdo.Wesl` now returns `ShaderBundle` and exposes only minimal metadata.
- Advanced reflection/quasiquoter/`Shader` types live in `Spirdo.Wesl.Reflection`.
- Uniform packing helpers live in `Spirdo.Wesl.Uniform`.
- Inputs remain in `Spirdo.Wesl.Inputs`.

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

## Quasiquoter
- `wesl` / `weslWith` are now in `Spirdo.Wesl.Reflection`.

## Options
- `Option` constructors (`Opt*`) are unchanged and work with `Spirdo.Wesl.compile`.
- `CompileOptions` helpers (`withSamplerMode`, `withOverrides`, ...) remain available
  in `Spirdo.Wesl.Reflection`.

# Spirdo API Migration

This guide maps the old API to the new `Shader`-centric API.

## Type renames
- `PreparedShader` → `Shader`
- `SomePreparedShader` → `SomeShader`

## Accessors
Old helpers were removed from the public surface. Use the new accessors instead:
- `preparedSpirv shader` → `shaderSpirv shader`
- `preparedInterface shader` → `shaderInterface shader`
- `preparedPlan shader` → `shaderPlan shader`
- `preparedStage shader` → `shaderStageCached shader`
- `preparedVertexAttributes shader` → `shaderVertexAttributes shader`

## Runtime compilation
Inline:
- `prepareWesl src`
  → `compile (SourceInline "<inline>" src)`

With options:
- `prepareWeslWith opts src`
  → `compileWith [Opt…] (SourceInline "<inline>" src)`

Diagnostics:
- `prepareWeslWithDiagnostics opts src`
  → `compileWithDiagnostics [Opt…] (SourceInline "<inline>" src)`

Files:
- `prepareWeslFile path` → `compileFile path`
- `prepareWeslFileWith opts path` → `compileFileWith [Opt…] path`
- `prepareWeslFileWithDiagnostics opts path`
  → `compileFileWithDiagnostics [Opt…] path`

## Options mapping
Replace `CompileOptions` builders with `Option` lists:
- `withSamplerMode SamplerSeparate …` → `OptSamplerMode SamplerSeparate`
- `withOverrideSpecMode SpecParity …` → `OptOverrideSpecMode SpecParity`
- `withOverrides [("name", val)] …` → `OptOverrides [("name", val)]`
- `withFeatures ["FOO"] …` → `OptEnableFeature "FOO"`
- `withSpirvVersion v …` → `OptSpirvVersion v`
- `withEntryPoint "main" …` → `OptEntryPoint "main"`
- `withCache False …` → `OptCache CacheDisabled`
- `withCache True …` → `OptCache (CacheInDir "dist-newstyle/.wesl-cache")`
- `withCacheVerbose True …` → `OptCacheVerbose True`
- `withTimingVerbose True …` → `OptTimingVerbose True`

You can combine multiple options in a list:
```
let opts = [OptSamplerMode SamplerSeparate, OptOverrideSpecMode SpecParity]
```

## Inputs
- `inputsFromPrepared shader builder` → `inputsFor shader builder`

## Quasiquoter
`wesl` / `weslWith` now produce `Shader` instead of `PreparedShader`.

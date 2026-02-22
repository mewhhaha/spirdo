# Revision history for spirdo

## Unreleased

### Breaking changes
* `wesl` returns raw WESL source (`String`) instead of compiling to a `Shader`.
* `Spirdo.Wesl.Reflection` now exposes a single compile-time shader path: use `spirv` with typed `Imports` (`imports`, `importsNil`, `import_`/`module_`, and `(:>)`/`(<:)`).
* Removed compile-time shader quasiquoter aliases: `weslShader`, `weslShaderWith`, `weslShaderBatch`, `weslShaderBatchWith`, `weslBatch`, `weslBatchWith`.

### Added
* Compile‑time inline import linking via `spirv` helpers (in‑memory module map).
* HList‑style import composition helpers: `imports`, `module_`, `moduleText`, `(<:)`, plus `Snoc` at the type level.

* Added precise source positions for parameters, expressions, lvalues, and statements; compile errors and diagnostics now report exact line/column locations (including quasiquoter failures).
* New helpers to render errors: `renderCompileError` and `renderCompileErrorWithSource`.
* Demo gallery refreshed (now includes an Inline Imports variant) and prints the active demo name to the console; SPIR-V dumps remain gated by `SPIRDO_WRITE_SPV=1`.
* Documentation refreshed to reflect the bundle-first API and Reflection split.

## 0.1.0.0 -- 2026-01-25

* First public version.
* WESL-to-SPIR-V compiler with reflection, uniform packing helpers, and diagnostics.
* Host-agnostic shader inputs (`Spirdo.Wesl.Inputs`) and typed binding lookup helpers.
* Inputs are now builder-only (HList input API removed); use the declarative builder (`inputsFrom*`, `uniform`, `sampler`, `texture`, etc.).
* Demo app now uses Slop (SDL3) and is gated behind the `spirdo-demo` flag.

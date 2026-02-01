# Revision history for spirdo

## Unreleased

* Added precise source positions for parameters, expressions, lvalues, and statements; compile errors and diagnostics now report exact line/column locations (including quasiquoter failures).
* New helpers to render errors: `renderCompileError` and `renderCompileErrorWithSource`.
* Demo gallery refreshed (12 fragment + 2 compute + 2 vertex shaders) and now prints the active demo name to the console; SPIR-V dumps remain gated by `SPIRDO_WRITE_SPV=1`.
* Documentation refreshed to reflect the bundle-first API and Reflection split.

## 0.1.0.0 -- 2026-01-25

* First public version.
* WESL-to-SPIR-V compiler with reflection, uniform packing helpers, and diagnostics.
* Host-agnostic shader inputs (`Spirdo.Wesl.Inputs`) and typed binding lookup helpers.
* Inputs are now builder-only (HList input API removed); use the declarative builder (`inputsFrom*`, `uniform`, `sampler`, `texture`, etc.).
* Demo app now uses Slop (SDL3) and is gated behind the `spirdo-demo` flag.

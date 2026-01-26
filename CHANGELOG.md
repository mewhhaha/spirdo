# Revision history for spirdo

## 0.1.0.0 -- 2026-01-25

* First public version.
* WESL-to-SPIR-V compiler with reflection, uniform packing helpers, and diagnostics.
* Host-agnostic shader inputs (`Spirdo.Wesl.Inputs`) and typed binding lookup helpers.
* Inputs are now builder-only (HList input API removed); use the declarative builder (`inputsFrom*`, `uniform`, `sampler`, `texture`, etc.).
* Demo app now uses Slop (SDL3) and is gated behind the `spirdo-demo` flag.

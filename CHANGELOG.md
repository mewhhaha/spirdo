# Revision history for Spirdo

## 0.2.0.0 -- 2026-07-13

### Breaking changes

- `wesl` now returns raw WESL source (`String`); Reflection compiles it through
  `spirv` and typed `Imports`.
- `weslWith`, legacy `weslShader*`, and legacy `weslBatch*` APIs were removed.
- Direct `Shader` construction is removed; compiler paths create typed shaders
  and the constructor is hidden.
- `bindingSlotCount` was replaced by `bindingSlotCounts` and
  `singleGroupBindingSlotCount`, returning group-aware `BindingSlotCount`.
- `emptyInputs` and `emptyInputsUnchecked` are removed. Use `mempty` with
  `inputsFor` for a shader with no bindings.
- Runtime cache constructors and `CachePolicy` were removed. Runtime `[Option]`
  deliberately has no cache controls.
- Imported reflection and override keys now use stable source-qualified names;
  undocumented raw `__wesl__...` keys are no longer accepted.

### Added

- Crystal Run is a small rasterized 3D game example with procedural meshes,
  Spirdo-compiled vertex and fragment shaders, pure game-state tests, and an
  Xvfb/Lavapipe screenshot harness for headless rendering checks.
- Runtime `sourceNamed` gives inline sources a useful diagnostic name; inline
  sources explicitly do not resolve filesystem imports.
- Runtime `ShaderBundle` accessors expose stage, bindings, vertex attributes,
  sampler mode, overrides, and compute workgroup size without requiring full
  Reflection.
- `BindingSlotCount { bscGroup :: Word32, bscSlots :: Word64 }` represents
  sparse binding spans independently per descriptor group and covers the full
  `Word32` binding range without overflow.
- File/package resolution discovers the nearest `wesl.toml`, supports recursive
  relative path dependencies, current-package and dependency-alias imports, and
  rejects canonical paths or symlinks escaping package roots.
- Package semantic identities are relocation-stable and injective across module
  segments, dependency routes, and legal identifiers. Ambiguous override
  shorthand reports the canonical candidates.
- Bounded UTF-8 source reads and explicit import/package graph budgets reject
  oversized files, excessive graph depth or breadth, and aggregate source or
  manifest exhaustion with evidence-bearing `CompileError`s.
- The advanced/TH cache is versioned, atomically written, and defensively
  validates bounded artifact shape, reflection, SPIR-V, and exact input identity
  before reuse.
- Runtime array reflection reports zero size and a nonzero stride; uniform and
  host packing paths reject execution-sized allocation.
- Buffer layout lowering emits Vulkan-facing Block, matrix-stride/column-major,
  and f16 capability requirements where applicable.
- Parser budgets, exact numeric-literal handling, reserved-word checks, nested
  comments, and template-closing token handling now fail deterministically.
- Regression coverage now includes import graphs, package containment, cache
  integrity, parser exhaustion, typed inputs, uniform safety, SPIR-V semantics,
  and Vulkan layout validation.

### Changed

- Documentation now distinguishes the tested WGSL/WESL subset from full
  conformance and identifies the parity manifest as the coverage authority.
- Input submission remains opaque, mode-indexed, and validated by `inputsFor`;
  successful input bundles are normalized by binding location.
- Cache directories are documented as local compiler state, not trusted or
  shareable artifacts. They must not be committed, downloaded, or shared.
- Diagnostics report positions where source evidence is available; some AST and
  emission failures remain locationless.
- Runtime logical operators short-circuit through structured SPIR-V control
  flow; specialization workgroup sizes use `OpExecutionModeId`, and SPIR-V
  strings use UTF-8.
- Unsuffixed scalar floats retain binary64 `AbstractFloat` precision until
  checked f32/f16 materialization, including direct round-to-nearest-even f16
  conversion without an intermediate f32 rounding step.
- Pointer parameters preserve identity in supported address spaces and emit
  the required Variable Pointers declarations. Workgroup/storage parameters
  require their source feature; uniform parameters and pointer returns fail
  explicitly instead of being lowered with changed semantics. Partial-pointer
  arguments are limited to storage/workgroup because Logical addressing cannot
  preserve function/private partial pointers across calls.
- Shader specialization emission rejects operations not legal for
  `OpSpecConstantOp`; workgroup overrides without defaults compile with no
  reflected default size and require positive host specialization before
  pipeline use. Their emitted zero is only SPIR-V's structural placeholder.
  Mixed i32/u32 workgroup dimensions remain rejected. Strict mode omits
  `SpecId` from derived and composite/non-scalar-literal specialization
  instructions; parity mode may be validator-incompatible for those forms.
- The optional demo is a separately built GHC 9.12 package; its gallery includes
  a validated SDF Text variant and no longer carries an unusable texture-backed
  shader module.

### Deprecated

- `validateUniformStorable` remains a deprecated alias for
  `validateUniformStorableUnchecked`.
- `packUniformStorable` remains a deprecated alias for
  `packUniformStorableUnchecked`.

## 0.1.0.0 -- 2026-01-25

- First public version.
- WESL-to-SPIR-V compiler with reflection, uniform packing helpers, and diagnostics.
- Host-agnostic shader inputs (`Spirdo.Wesl.Inputs`) and typed binding lookup helpers.
- Demo app gated behind the `spirdo-demo` flag.

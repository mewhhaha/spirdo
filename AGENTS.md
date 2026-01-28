# WESL Expansion Plan

## Phases (kept up to date)
- Phase 1 (Complete): Control flow + comparisons
  - DONE: `if` / `else`, `while`, `for`, `break`, `continue`
  - DONE: comparison ops (`==`, `!=`, `<`, `<=`, `>`, `>=`) and boolean `&&`, `||`, `!`
  - DONE: structured control flow (`OpSelectionMerge`, `OpLoopMerge`, branches; no SSA/phi yet)
  - DONE: example shader updated to exercise loops/conditionals
- Phase 2 (Complete): Math + utility builtins
  - DONE: `abs`, `min/max`, `clamp`, `mix`, `dot`, `length`, `normalize`
  - DONE: `sin`, `cos`, `pow`, `sqrt` (GLSL.std.450)
- Phase 3 (Complete): Types & indexing
  - DONE: arrays (fixed + runtime), matrices, swizzles (`.xyzw`)
  - DONE: constructors, casts, more robust layout/align
  - DONE: matrix constructors accept full scalar lists (column-major)
- Phase 4 (Complete): Functions + modules
  - DONE: user functions, overloading, module constants
  - DONE: improved errors + tests
- Phase 5 (Complete): Full GPU resource model
  - DONE: storage buffers/textures, atomics, derivatives
  - DONE: full stage IO with varying structs
- Phase 6 (Complete): WESL module system + conditional translation
  - DONE: parse `import` + `@if` translate-time expressions
  - DONE: file-based compile path + module linking
  - DONE: item imports + aliases; unqualified item usage
  - DONE: deterministic module-vs-item import resolution (ambiguous imports are errors)
  - DONE: flow-sensitive scope validation for imports/identifiers
  - DONE: multiple `@if` attributes (combined with AND)
  - NOTE: source maps intentionally omitted (not required)
- Phase 7 (Complete): Core WGSL grammar completeness
  - DONE: `switch`, `loop` + `continuing`, `break if`, `discard`
  - DONE: `fallthrough`, `switch` const selector validation
  - DONE: compound assignments, increment/decrement, bitwise ops, shifts, modulo
  - DONE: directives (`enable`, `diagnostic`), `alias`, `override`, `const_assert`
- Phase 8 (Complete): Full type system + address spaces
  - DONE: `f16` scalar type (layouts, ops, casts, constants, SPIR-V capability)
  - DONE: module-scope `var<private>` and `var<workgroup>` globals
  - DONE: `@align` / `@size` on struct fields with host-shareable validation (power-of-two, >= natural, size multiple of alignment)
  - DONE: `ptr`/`ref` types + `&`/`*` (address-of/deref), access propagation, read-only enforcement
  - DONE: pointer address spaces beyond private/workgroup (`uniform`, `storage`) with default access rules
  - DONE: abstract literal coercion for scalar constants (assign/return/binary ops/ctors/calls)
  - DONE: integer literals default to i32 (u32 for large literals), switch selectors coerce consts to match selector type
  - LIMITATION: pointer access qualifiers only tracked as read-only vs read-write (no write-only model)
- Phase 9 (Planned): Textures/samplers + builtins
  - all texture kinds (1d/2d/3d/array/cube, multisampled, depth, storage)
  - sampler comparison, texture sampling/load/store/gather variants
  - full WGSL builtin function set and overload resolution
- Phase 9 (Complete):
  - DONE: added builtins `floor`, `ceil`, `fract`, `exp`, `log`, `exp2`, `log2`, `step`, `smoothstep`
  - DONE: added builtins `distance`, `reflect`
  - DONE: texture types `texture_1d`, `texture_1d_array`, `texture_2d_array`, `texture_3d`, `texture_cube`, `texture_cube_array`
  - DONE: texture types `texture_multisampled_2d`, `texture_depth_2d`, `texture_depth_2d_array`, `texture_depth_cube`, `texture_depth_cube_array`, `texture_depth_multisampled_2d`
  - DONE: sampler comparison type `sampler_comparison`
  - DONE: storage textures `texture_storage_1d`, `texture_storage_2d_array`, `texture_storage_3d`
  - DONE: `textureSample`, `textureSampleLevel`, `textureSampleBias`, `textureSampleGrad`
  - DONE: `textureSampleCompare`, `textureSampleCompareLevel`
  - DONE: `textureLoad` for sampled + multisampled textures (LOD, array index, sample index)
  - DONE: `textureStore` for storage textures (1d/2d/2d_array/3d)
  - DONE: `textureGather`, `textureGatherCompare`
  - DONE: texture query builtins `textureDimensions`, `textureNumLevels`, `textureNumLayers`, `textureNumSamples`
  - DONE: builtins `select`, `any`, `all`, `sign`
  - DONE: integer `abs`/`min`/`max`/`clamp` lowering (OpSelect + comparisons)
  - DONE: bit builtins `countOneBits`, `reverseBits`, `extractBits`, `insertBits`
  - DONE: `arrayLength` for runtime arrays in storage-buffer structs
  - DONE: float builtins `round`, `roundEven`, `trunc`, `radians`, `degrees`, `tan`, `asin`, `acos`, `atan`, `atan2`
  - DONE: float builtins `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `inverseSqrt`, `fma`
  - DONE: matrix/vector builtins `transpose`, `determinant`, `inverse`, `cross`, `faceForward`, `refract`
  - DONE: packing builtins `pack4x8(unorm/snorm)`, `pack2x16(unorm/snorm/float)` and matching `unpack*`
  - DONE: bit builtins `firstLeadingBit`, `firstTrailingBit`, plus `saturate` and `quantizeToF16`
  - DONE: `modf`, `frexp`, `ldexp` (struct-return builtins)
  - DONE: WGSL bit/packing builtins `countLeadingZeros`, `countTrailingZeros`, `dot4U8Packed`, `dot4I8Packed`
  - DONE: `bitcast<T>(value)` for scalar/vector numeric types (size-matched)
- Phase 10 (Complete): Semantics + validation
  - DONE: override specialization values via `CompileOptions.overrideValues` (scalar/vector/matrix/array/struct)
  - DONE: const function evaluation for `const_assert`/`switch` selectors (single return statement, bool/i32/u32 params)
  - DONE: diagnostics handling for `diagnostic(off, const_assert)` + `enable` feature validation
  - DONE: optional `spirv-val` validation in tests when tool is available
  - DONE: constant integer folding for `const_assert` and `switch` selectors (arithmetic/bitwise/shifts)
  - DONE: constant folding in `emitConstExpr` for integer ops and unary minus
  - LIMITATION: diagnostics are non-fatal only (warnings/info are not surfaced), const functions are return-only and int/bool-only
- Phase 11 (Complete): WESL packaging + tooling
  - DONE: `wesl.toml` discovery and minimal parsing for package metadata
  - DONE: `PackageInfo` API for host reflection
  - LIMITATION: parser is minimal and ignores non-`[package]` fields

## Parity Gap Phases (Planned)
- Phase 12 (Complete): Diagnostics + reporting parity
  - DONE: diagnostics data model + compile-with-diagnostics APIs
  - DONE: `diagnostic(off/warning, const_assert)` now surfaced as diagnostics (no hard error)
  - DONE: tests cover diagnostic warning/off behavior
  - DONE: const_assert diagnostics now include source locations
  - DONE: `diagnostic(warning, unreachable_code)` emits warnings and skips unreachable statements in codegen
  - DONE: tests cover unreachable_code warnings
  - DONE: `diagnostic(warning, unused_expression)` warns on expression statements with no effect
  - DONE: `diagnostic(warning, unused_variable)` warns on unused local let/var bindings
  - DONE: `diagnostic(warning, unused_parameter)` warns on unused function parameters
  - DONE: `diagnostic(warning, shadowing)` warns on locals shadowing outer scopes
  - DONE: `diagnostic(warning, constant_condition)` warns on `if/while/for/break if` constant conditions
  - DONE: `diagnostic(warning, duplicate_case)` warns on duplicate switch selectors
  - NOTE: source locations are best-effort (const_assert includes locations; most others are location-less)
- Phase 13 (Complete): Const-eval expansion
  - DONE: const eval for float literals with casts (`f32`/`f16`) and arithmetic
  - DONE: float comparisons in const_assert
  - DONE: const-eval builtins `abs`, `min`, `max`, `clamp`, `mix`, `select`
  - DONE: const functions can accept/return `f32`/`f16` scalars (single-return only)
  - DONE: const eval for vec/mat/array/struct literals + field/index access
  - DONE: const_assert equality for vectors/matrices/arrays/structs
  - DONE: const functions support `let`, `if`, expression statements, and multi-return bodies
  - DONE: const functions support `switch` with const selectors
  - DONE: const functions accept non-scalar parameter/return types (vec/mat/array/struct) when const-evaluable
  - DONE: const functions support `var`, assignments, `while`/`loop`/`for`, `break`/`continue`, and `switch`
  - DONE: const functions support `fallthrough` in `switch`
  - DONE: const functions support pointer ops (`&` / `*`) with mutability checks
- Phase 14 (Complete): Override specialization constants
  - DONE: emit `OpSpecConstant*` for overrides with default initializers when no `overrideValues` provided
  - DONE: overrides without initializers emit zero-valued specialization constants (no `overrideValues` required)
  - DONE: `@id` support for overrides + host reflection (`ShaderInterface.siOverrides`)
  - DONE: override dependency graph + cycle detection
  - DONE: override initializer expressions lowered to spec-constant expressions when possible
  - DONE: `OpSpecConstantOp` emission support for scalar const expressions
  - NOTE: `overrideSpecMode = SpecStrict` omits `SpecId` on derived overrides to satisfy `spirv-val`; `SpecParity` forces `SpecId` for full WESL parity
- Phase 15 (Complete): Import resolution + scoping parity
  - DONE: ambiguous module-vs-item imports now raise an error
  - DONE: duplicate import aliases are rejected
  - DONE: duplicate import targets are rejected
  - DONE: block-local duplicate declarations are rejected (flow-sensitive validation)
  - DONE: parameter name duplicates are rejected
  - DONE: flow-sensitive scope tracking prevents use of out-of-scope locals
  - DONE: shadowing is an error by default (can be downgraded with `diagnostic(warning/off, shadowing)`)
- Phase 16 (Complete): CTS + validation coverage
  - DONE: spirv-val clean on current test suite after fixes for 1D image capabilities, image query capability, and depth image fetch result types
  - DONE: basic CTS fixture harness (`test/cts/positive` and `test/cts/negative`) when directories exist
  - DONE: added CTS fixtures for const-function control flow (positive + negative)
  - DONE: CTS fixtures for override defaults and scope errors
  - DONE: CTS fixtures for import resolution (basic success + duplicate alias/target)
  - DONE: CTS fixtures for const-eval pointers and switch fallthrough
  - DONE: CTS fixtures for const_assert composite equality, type mismatch, and pointer comparisons
  - DONE: CTS fixtures for override dependencies and cycles
- Phase 17 (Complete): Packaging + tooling polish
  - DONE: `wesl.toml` parser for `[package]`, `[dependencies]`, and `[dependencies.<name>]`
  - DONE: dependency path resolution (relative paths normalized from package root)
  - DONE: package discovery rules respected with version strings preserved

## Demo App Notes
- DONE: Fragment shader variants switchable with left/right arrows in `exe/Main.hs`
- DONE: Variants include Feature Mix, Raymarch, Triangle, Plasma, Grid, SDF Text, Clouds, Bits, Aurora, Starfield, Tunnel, Voronoi, Mandelbrot
- DONE: Override variants (Stripes/Rings) compiled via runtime specialization values
- DONE: Grid shader uses bitwise + shift + modulo ops for a jitter pattern
- DONE: Full-screen quad used for rendering (no single-triangle stretch)
- DONE: SDF Text/Clouds tuned for visibility
- DONE: Feature Mix shader now exercises pointer ops (`&`/`*`)
- DONE: Bits shader now exercises `countLeadingZeros`, `countTrailingZeros`, `dot4*`, and `bitcast`
- DONE: Additional vertex/compute shader examples emitted to `vertex-1.spv`/`vertex-2.spv` and `compute-1.spv`/`compute-2.spv`
- DONE: SPIR-V outputs are gated behind `SPIRDO_WRITE_SPV=1`
- DONE: Added combined-sampler mode (inline `// spirdo:sampler=combined`) and `sampledTexture` inputs; clouds now use combined bindings.

## Remaining WESL Parity Gap (Override Specialization)
`spirv-val` rejects `SpecId` on `OpSpecConstantOp`, which is what derived overrides lower to when they depend on other overrides. We now support parity via an opt-in mode.

Recommended path to full parity (now implemented):
1) Use `overrideSpecMode = SpecParity`.
2) In `SpecParity` mode:
   - Always emit `SpecId` for overrides, including those lowered via `OpSpecConstantOp`.
   - Keep `oiSpecId` populated for *all* overrides.
   - Skip/relax `spirv-val` (or mark those cases as expected-failure in tests), since the current validator rejects this pattern.
3) Keep `SpecStrict` (default) for validated builds:
   - Current behavior: derived overrides are emitted *without* `SpecId`.
   - Interface clearly marks runtime-specializable overrides (`oiSpecId = Just ...`) vs derived ones (`oiSpecId = Nothing`).

This keeps default builds fully validated while providing an opt-in path to strict WESL parity for toolchains that accept `SpecId` on `OpSpecConstantOp`.

## Next Steps (Paused)
- DONE: added `Bindings iface` + `binding @"name"` lookup with compile-time membership checks (no manual list plumbing).
- DONE: README updated with zero-plumbing binding access examples.
- DONE: finish refactor verification — `cabal build` and `cabal test`.
- DONE: add golden SPIR-V tests (created `test/golden/`, update/compare logic in `test/Main.hs`, seeded 2 fixtures).
- DONE: add tests for `packUniformFrom`, `validateUniformStorable`, and `packUniformStorable` in `test/Main.hs`.
- DONE: update README to mention public API is `Spirdo.Wesl` + `Spirdo.Wesl.Inputs`, and document uniform packing helpers.

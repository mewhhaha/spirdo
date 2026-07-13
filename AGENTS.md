# Haskell Engineering Guide (Skill Style)

Use this guide for code in this repo. Optimize for:
- correctness by construction,
- pure core logic,
- minimal API surface,
- measurable performance,
- simple data and predictable runtime behavior.

## 1) Push IO To The Boundary

Keep parsing, shaping, and math pure. Do file/process/env work in thin boundary functions.

Good:
```haskell
readConfig :: IO Config
readConfig = do
  raw <- readFile "config.json"
  case decodeConfig raw of
    Left err -> fail err
    Right cfg -> pure cfg

decodeConfig :: String -> Either String Config
decodeConfig = ...
```

Bad:
```haskell
decodeConfig :: IO Config
decodeConfig = do
  raw <- readFile "config.json"
  ...
```

## 2) Make Illegal States Unrepresentable

Prefer domain types and smart constructors over loose tuples and `String`.

Good:
```haskell
newtype PxRange = PxRange Double

mkPxRange :: Double -> Either String PxRange
mkPxRange x
  | x > 0 = Right (PxRange x)
  | otherwise = Left "pxRange must be > 0"
```

Bad:
```haskell
type PxRange = Double
-- any value accepted, including negatives and NaN
```

## 3) GHC 9.12 Record Style

Use modern record ergonomics:
- `NoFieldSelectors`
- `DuplicateRecordFields`
- `OverloadedRecordDot`
- `OverloadedRecordUpdate` (when it helps readability)
- `ApplicativeDo` (prefer when effects are independent)

Prefer short field names scoped by type (`id`, `adv`, `pts`, `bbox`). Keep them clear but compact.

Good:
```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE ApplicativeDo #-}

data Glyph = Glyph { id :: !Int, adv :: !Double }
data Run   = Run   { id :: !Int, adv :: !Double }

scaleAdvance :: Double -> Glyph -> Glyph
scaleAdvance k g = g { adv = g.adv * k }
```

Bad:
```haskell
data Glyph = Glyph
  { glyphIdentifier :: Int
  , glyphAdvanceInFontUnitsAlongX :: Double
  }
```

## 4) Prefer Total Functions

No partial pattern matches, `head`, `tail`, `fromJust`, or unchecked indexing.

Good:
```haskell
firstGlyph :: [a] -> Maybe a
firstGlyph [] = Nothing
firstGlyph (x:_) = Just x
```

Bad:
```haskell
firstGlyph :: [a] -> a
firstGlyph xs = head xs
```

## 5) Use Explicit Errors

Return `Either` for recoverable failures. Reserve exceptions for truly exceptional cases.

Good:
```haskell
parseAxis :: String -> Either String Axis
parseAxis s = ...
```

Bad:
```haskell
parseAxis :: String -> Axis
parseAxis s = error ("bad axis: " <> s)
```

## 6) Keep Data Flow Simple

Break transformations into small pure functions with descriptive names.

Good:
```haskell
buildGlyphPlan :: Font -> [GlyphId] -> Plan
buildGlyphPlan font = finalize . colorize . flatten . outlines font
```

Bad:
```haskell
buildGlyphPlan font ids = -- 150 lines of mixed concerns
```

## 7) Be Deliberate With Strictness

Start clear, then add strictness where profiling proves need. Use strict fields and `foldl'` in hot paths.

Good:
```haskell
data Stats = Stats
  { glyphs :: !Int
  , segments :: !Int
  }

countSegments :: [Int] -> Int
countSegments = foldl' (+) 0
```

Bad:
```haskell
countSegments :: [Int] -> Int
countSegments = foldl (+) 0
```

## 8) Keep Module Surfaces Small

Export only what callers need. Hide constructors when invariants matter.

Good:
```haskell
module Font.Range (PxRange, mkPxRange, unPxRange) where
```

Bad:
```haskell
module Font.Range where
```

## 9) Use Typeclasses Sparingly

Use typeclasses for real ad-hoc polymorphism. Prefer concrete functions for local logic.

Good:
```haskell
encodeGlyphId :: GlyphId -> Text
```

Bad:
```haskell
class Encodable a where
  encode :: a -> Text
-- with only one instance in the whole repo
```

## 10) Test Pure Core First (Prefer TDD)

Put core algorithms behind pure APIs and test those directly. IO wrappers should stay thin.
When practical, do TDD:
1. write a failing test,
2. implement the smallest fix,
3. refactor while keeping tests green.

Property testing should be the default for deterministic core logic.
Use example/unit tests for edge cases and regression locks.

Good:
```haskell
-- property test + targeted regression examples for contour and distance invariants
```

Bad:
```haskell
-- tests only through end-to-end CLI calls, no core properties
```

## 11) Optimize With Evidence

Profile before changing hot code, then remeasure. Keep diffs small and reversible.

Good:
```haskell
-- capture baseline
-- apply one focused optimization
-- rerun benchmark and compare
```

Bad:
```haskell
-- broad refactor + perf assumptions + no benchmark delta
```

## 12) Naming And Comments

Use names that encode domain meaning. Add comments for invariants and non-obvious performance constraints.

Good:
```haskell
-- Invariant: contour winding is normalized before edge coloring.
normalizeContours :: [Contour] -> [Contour]
```

Bad:
```haskell
go2 :: [A] -> [A]
go2 = ...
```

## 13) Default Pragmas And Warnings

Prefer strong warnings and avoid broad suppression.
- Keep `-Wall` enabled.
- If disabling a warning, document why near the pragma.

Useful non-default pragmas for this repo (with `default-language: GHC2024`):
- `NoFieldSelectors`
- `DuplicateRecordFields`
- `OverloadedRecordDot`
- `OverloadedRecordUpdate`
- `ApplicativeDo`
- `DerivingVia`
- `DeriveAnyClass`
- `TypeFamilies`
- `TypeFamilyDependencies` (only when needed)
- `PatternSynonyms`
- `ViewPatterns`
- `StrictData` (hot data modules only)
- `UnboxedTuples`, `UnboxedSums`, `MagicHash` (hot-path leaf modules only)

Use sparingly and only with an inline rationale:
- `UndecidableInstances`
- `IncoherentInstances`
- `OverlappingInstances`
- `ImpredicativeTypes`

Rule of thumb:
- prefer enabling pragmas at module scope, not project-wide,
- keep specialized pragmas close to the small set of modules that need them.

## 14) Performance: Keep Data Simple, Keep Allocations Low

In hot paths, straightforward code with simple data usually beats clever tricks.
- Prefer compact records with strict fields.
- Prefer single-pass loops over chains of `map`/`filter`/`concat`.
- Avoid unnecessary temporary structures.
- Choose readability plus predictable allocation patterns over "clever" hacks.

Good:
```haskell
sumLen :: [Edge] -> Double
sumLen = foldl' step 0
  where
    step !acc e = acc + edgeLen e
```

Bad:
```haskell
sumLen :: [Edge] -> Double
sumLen es = sum (map edgeLen es)
```

Good:
```haskell
-- clear and direct representation used in hot code
data Edge = Edge { x0 :: !Double, y0 :: !Double, x1 :: !Double, y1 :: !Double }
```

Bad:
```haskell
-- opaque "clever" encoding that hurts maintainability
newtype Edge = Edge Word64
```

## 15) Parallelism: Use It Deliberately And Non-Invasively

Parallelism is a tool, not a default. Start from a clear sequential baseline, then parallelize only proven hot pure work.

### Heuristics
- Parallelize CPU-bound, pure, independent work items.
- Avoid parallelizing tiny tasks; chunk work to amortize scheduling overhead.
- Prefer bounded parallelism (typically `numCapabilities` or small multiples).
- Keep each parallel unit self-contained to avoid shared mutable state.
- If the algorithm is memory-bandwidth bound, parallelism may not help.

### Non-invasive rollout pattern
1. Keep a clear sequential function as the source of truth.
2. Add a parallel wrapper that preserves type and output.
3. Gate parallel execution behind config/flag so fallback is trivial.
4. Keep the diff small: do not redesign domain types just to add parallelism.

Good:
```haskell
buildAllSeq :: [GlyphReq] -> [GlyphOut]
buildAllSeq = map buildOne

buildAllPar :: Int -> [GlyphReq] -> [GlyphOut]
buildAllPar chunkN xs =
  concatMap (withStrategy (parList rdeepseq) . map buildOne) (chunksOf chunkN xs)
```

Bad:
```haskell
-- mixes algorithm rewrite, mutable state, and parallelism in one risky diff
buildAll :: [GlyphReq] -> IO [GlyphOut]
buildAll = ...
```

### Benchmarking parallel usefulness
- Measure sequential baseline first.
- Run with fixed workload and fixed RTS settings.
- Compare wall time, allocation, and GC stats.
- Require repeatable wins, not one fast outlier.
- Validate no output differences from sequential mode.

Suggested loop:
1. `cabal bench --benchmark-options='--match "<target>" +RTS -N1 -s -RTS'`
2. `cabal bench --benchmark-options='--match "<target>" +RTS -N -s -RTS'`
3. Repeat both several times and compare medians.
4. Keep parallel path only if speedup is stable and memory blow-up is acceptable.

Red flags:
- Parallel is faster once but slower on median.
- Allocation or GC time spikes enough to erase CPU gains.
- Throughput improves while latency worsens for real workload shape.

## 16) Collect And Enforce Invariants

Invariants are part of the design, not comments-only documentation.
- Collect them near domain types and constructors.
- Enforce them at boundaries (parse/load/decode), then keep core logic assumption-safe.
- Name invariants explicitly (`invBBoxFinite`, `invEdgesNonEmpty`, etc.).
- Add property tests for invariant-preserving transforms.

Good:
```haskell
newtype PxRange = PxRange Double

mkPxRange :: Double -> Either String PxRange
mkPxRange x
  | x > 0 && isFinite x = Right (PxRange x)
  | otherwise = Left "PxRange invariant failed: must be finite and > 0"
```

Bad:
```haskell
type PxRange = Double
-- invariant exists only in developer memory
```

Good:
```haskell
-- property: normalization does not change contour count
prop_normalizeContours_preservesCount :: [Contour] -> Bool
prop_normalizeContours_preservesCount cs =
  length (normalizeContours cs) == length cs
```

Bad:
```haskell
-- no property/regression tests for stated invariants
```

## 17) Prefer Standard Algebraic Abstractions

Use standard typeclasses before inventing custom ones.

### Core classes to reach for
- `Semigroup`: associative combine of partial results (`<>`).
- `Monoid`: `Semigroup` plus identity (`mempty`).
- `Functor`: map values without changing structure.
- `Applicative`: combine independent effects/validations.
- `Monad`: sequence dependent effects.
- `Foldable`: consume/reduce structures.
- `Traversable`: map with effects while preserving shape.
- `Alternative`: choice/fallback for parser-like flows.

### Heuristics
- If computations are independent, prefer `Applicative` over `Monad`.
- If order/dependency matters, use `Monad`.
- If you only need combination, use `Semigroup`/`Monoid`.
- If you only need traversal with effects, use `traverse` (not manual recursion).
- Avoid custom typeclasses unless there are multiple real instances.

Good:
```haskell
-- independent field checks: Applicative style
mkGlyphCfg :: Double -> Double -> Either String GlyphCfg
mkGlyphCfg px rng = GlyphCfg <$> mkScale px <*> mkPxRange rng
```

Bad:
```haskell
-- monadic style used with fake dependency and extra noise
mkGlyphCfg px rng = do
  s <- mkScale px
  r <- mkPxRange rng
  pure (GlyphCfg s r)
```

Good:
```haskell
-- semigroup/monoid for combining summaries
data Stats = Stats { glyphs :: !Int, edges :: !Int }

instance Semigroup Stats where
  a <> b = Stats { glyphs = a.glyphs + b.glyphs, edges = a.edges + b.edges }

instance Monoid Stats where
  mempty = Stats 0 0
```

Bad:
```haskell
-- ad-hoc combine function repeated in multiple call sites
mergeStats :: Stats -> Stats -> Stats
mergeStats = ...
```

Good:
```haskell
-- prefer traverse for effectful mapping with preserved shape
loadAll :: [FilePath] -> IO [Font]
loadAll = traverse loadFont
```

Bad:
```haskell
loadAll [] = pure []
loadAll (p:ps) = do
  f <- loadFont p
  fs <- loadAll ps
  pure (f:fs)
```

## 18) Use Subagents For Substantial Implementation Tasks

For substantial implementation tasks, use subagents in parallel when work can be partitioned safely.
- Do not parallelize edits that contend on the same file or hunk.
- Do not parallelize `apply_patch` flows that target the same file.
- Keep work sequential when later steps depend on earlier outputs or decisions.

## Quick Checklist

Before shipping:
- IO at edges, pure core in library.
- No partial functions in new code.
- Domain constraints encoded in types or constructors.
- Record style follows GHC 9.12 features (`NoFieldSelectors`, duplicate fields, overloaded record syntax).
- If syntax, semantics, lowering, or public language features change, update `README.md` in the same change.
- Field names are short, clear, and type-scoped.
- Hot paths use simple data and single-pass allocation-aware loops.
- Parallelism is applied only to proven hot pure independent work.
- Sequential path remains simple and available as fallback.
- Parallel speedup is verified with repeated `-N1` vs `-N` benchmark comparisons.
- Invariants are explicit, enforced at boundaries, and covered by property/regression tests.
- Standard typeclasses are used where they fit (`Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`).
- Non-default pragmas are intentional, local, and justified.
- Add tests for new behavior; prefer property tests for pure core logic.
- Prefer TDD when practical (failing test first).
- Benchmarks/tests rerun for touched behavior.
- Public exports are intentionally minimal.

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
- DONE: Fragment shader variants switchable with left/right arrows in `examples/app/Main.hs`
- DONE: Variants include Feature Mix, Raymarch, Triangle, Plasma, Grid, SDF Text, Clouds, Bits, Aurora, Starfield, Tunnel, Voronoi, Mandelbrot
- DONE: Override variants (Stripes/Rings) compiled via runtime specialization values
- DONE: Grid shader uses bitwise + shift + modulo ops for a jitter pattern
- DONE: Full-screen quad used for rendering (no single-triangle stretch)
- DONE: SDF Text/Clouds tuned for visibility
- DONE: Feature Mix shader now exercises pointer ops (`&`/`*`)
- DONE: Bits shader now exercises `countLeadingZeros`, `countTrailingZeros`, `dot4*`, and `bitcast`
- DONE: Additional vertex/compute shader examples emitted to `vertex-1.spv`/`vertex-2.spv` and `compute-1.spv`/`compute-2.spv`
- DONE: SPIR-V outputs are gated behind `SPIRDO_WRITE_SPV=1`
- DONE: Added combined-sampler mode (now default) and `sampledTexture` inputs; clouds use combined bindings.

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

## WESL Syntax Gap Closure (Complete)
- DONE: typed numeric literal suffixes (`1i`, `1u`, `1.0f`, `1.0h`)
- DONE: shorthand vector/matrix type names (`vec2f`, `vec3u`, `vec2h`, `mat2x2f`, etc.)
- DONE: typed constructors with scalar splat (e.g., `vec4<f32>(1.0)` and `vec2h(1.0h)`)
- DONE: constant expressions for array lengths and `@workgroup_size` (resolved during typecheck)

## Pause Handoff (2026-02-08)
### Current Status
- Working tree clean.
- Recent changes:
  - `weslBatchWith` compiles batch entries in parallel (TH splice path).
  - Fragment example shaders use derivative-aware anti-aliasing where needed.
- Last validation:
  - `cabal test` passed (2026-02-08).

### Notes
- Batch parallelism is governed by GHC RTS capabilities (`GHCRTS=-N` for full cores).
- Performance refactor history and medians live in `PERF_REFACTOR_REPORT.md`.

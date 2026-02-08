# Performance Refactor Report

## Scope
Non-breaking internal refactors focused on preserving API and features while improving compile/packing performance and keeping code pure/Haskell-idiomatic.

## Baseline (before refactors)
### Tests
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

### Bench
- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `58520346`
- `time per compile (ns)`: `1170406.92`

## Changes applied
- `lib/Spirdo/Wesl/Emit.hs`
  - Removed quadratic list appends in constant composite emission helpers (`acc <> [x]` -> prepend + reverse once).
  - Switched SPIR-V word serialization to `ByteString.Builder` (`spirvToBytes`) to avoid intermediate `[Word8]` allocation.
  - Added O(1) const lookup index (`gsConstKeyById :: Map Word32 ConstKey`) and switched `lookupConstKeyById` to map lookup.
  - Added spec-constant literal id set (`gsSpecConstLiteralIds :: Set Word32`) maintained in `addConst`; `isSpecConstantLiteral` is now O(1).
  - Switched constant cache (`gsConstCache`) from list to `Map ConstKey Word32` for O(1) expected key lookup.

- `lib/Spirdo/Wesl/Typecheck.hs`
  - Replaced list queue in module graph traversal with `Data.Sequence`.
  - Changed module merge from quadratic append fold to linear prepend-preserving merge (`foldr` + prepend fields).
  - Replaced repeated list scans in `moduleHasItem` with `Set` membership.

- `lib/Spirdo/Wesl/Parser.hs`
  - Simplified `@if` evaluation to direct short-circuit checks (`all`) instead of building a temporary combined AST.
  - Made import collection accumulation linear by collecting reversed and reversing once.

- `lib/Spirdo/Wesl/Types/Uniform.hs`
  - Reworked scalar emission to output `Builder` chunks directly (no `[Word8]` -> `ByteString` conversion per scalar).
  - Updated `emitSegment` to work with chunk length + builder directly.
  - Added indexed builder fold helper to avoid `zip [0..]` temporary lists in vectors/matrices/arrays.
  - Collapsed struct value validation/map building into a single pass.
  - Optimized padding builder to reuse a static zero chunk.

- `lib/Spirdo/Wesl/Compiler.hs`
  - Reworked cache-key hashing to stream line-by-line through FNV-1a steps without building one large combined string/byte buffer.

## Verification (after refactors)
### Tests
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

### Bench
- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `45584501`
- `time per compile (ns)`: `911690.02`

## Delta
- `time per compile`: `1170406.92 ns` -> `911690.02 ns`
- Improvement: `258716.90 ns` (`22.10%` faster)

## Simplicity Pass (follow-up)
Additional non-breaking readability refactors were applied after the first performance pass:
- flattened parser control flow in parameter and statement parsing
- flattened/centralized import resolution branching in `Typecheck`
- reduced nesting in uniform matrix/struct packing paths and used `first` for error mapping

### Verification after simplicity pass
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `46654757`
- `time per compile (ns)`: `933095.14`

### Baseline vs final (after simplicity pass)
- `time per compile`: `1170406.92 ns` -> `933095.14 ns`
- Improvement: `237311.78 ns` (`20.28%` faster)

## Modularization + Helpers Pass (1/2/3)
Implemented follow-up changes for:
1. splitting `Emit` internals
2. reducing repeated count checks in hot paths
3. replacing ad-hoc path/list manipulation with helpers

### Code changes
- Added `lib/Spirdo/Wesl/Emit/Encoding.hs` and moved `spirvToBytes` + `encodeString` there.
- Updated `lib/Spirdo/Wesl/Emit.hs` to import the new module.
- Added reusable path helpers in `lib/Spirdo/Wesl/Typecheck.hs`:
  - `appendPathSegments`
  - `splitImportTarget`
  - `renderPath`
- Reduced repeated `length` usage in selected hot paths:
  - function call candidate filtering and arity checks in `Emit`
  - matrix/array count checks in uniform packing

### Verification after modularization/helpers pass
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `45632742`
- `time per compile (ns)`: `912654.84`

### Baseline vs latest
- `time per compile`: `1170406.92 ns` -> `912654.84 ns`
- Improvement: `257752.08 ns` (`22.02%` faster)

## Additional Simplification Pass
Further non-breaking simplifications were applied:
- flatter parser translate-time attribute handling
- less nested typecheck entry/import helper logic
- uniform struct error selection using `Maybe` combinators
- `foldMBuilder` switched to `foldM` builtin
- flatter overload-resolution matching path in `Emit`

### Verification
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `46987116`
- `time per compile (ns)`: `939742.32`

### Baseline vs this pass
- `time per compile`: `1170406.92 ns` -> `939742.32 ns`
- Improvement: `230664.60 ns` (`19.71%` faster)

## Profiling-Driven Lexer Fast Path Pass
Profile-guided optimization targeted lexer prefix checks in `lexWesl`:
- replaced repeated `Text.isPrefixOf` multi-token checks with single-pass char dispatch using `(c, c2, c3)` lookahead
- removed repeated prefix allocations/branching overhead on the hot lexing path

### Verification
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- `compile iterations`: `50`
- `total bytes`: `435200`
- `total time (ns)`: `35910684`
- `time per compile (ns)`: `718213.68`

### Baseline vs this pass
- `time per compile`: `1170406.92 ns` -> `718213.68 ns`
- Improvement: `452193.24 ns` (`38.64%` faster)

## Function IR Accumulation Pass (latest)
Targeted the largest remaining hotspot in `Emit`: repeated list appends while building function-local SPIR-V instruction streams.

### Code changes
- `lib/Spirdo/Wesl/Emit.hs`
  - `FuncState` instruction/local accumulation changed from append (`<> [x]`) to prepend (`x : xs`) in:
    - `addFuncInstr`
    - `addFuncLocal`
    - `addTerminator`
    - `addLabel`
  - Function assembly now reverses `fsLocals`/`fsInstrs` exactly once at emission boundaries:
    - `emitMainFunction`
    - `emitFunctionBody`
  - Return insertion paths updated to prepend in:
    - `emitStmtFn` (void `return`)
    - `finalizeFunctionReturn`
  - Function-parameter seeded local/store lists are reversed once when initializing `FuncState` to preserve emitted order.

### Verification
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- 5-run sample (`time per compile (ns)`):
  - `617069.58`
  - `595934.44`
  - `600080.46`
  - `592468.08`
  - `630702.32`
  - median: `600080.46`

### Baseline vs latest (5-run median)
- `time per compile`: `1170406.92 ns` -> `600080.46 ns`
- Improvement: `570326.46 ns` (`48.73%` faster)

### Notes on rejected follow-ups
- A map/set lookup conversion in `Emit` (constants/struct ids/ext-inst ids/capabilities) regressed benchmark time and was reverted.
- Reverting global section prepend/reverse strategy also did not improve end-to-end benchmark in this environment and was reverted.

## Code-Reduction Follow-up (no API change)
Kept the fast prepend/reverse model and reduced duplicated emit code.

### Code changes
- `lib/Spirdo/Wesl/Emit.hs`
  - Added `finalizeFunctionInstrs` helper and reused it from:
    - `emitMainFunction`
    - `emitFunctionBody`
  - Added `terminateWithReturn` helper and reused it from:
    - `emitStmtFn` (void return path)
    - `finalizeFunctionReturn`
  - Net effect: less duplicated list assembly/termination logic with same behavior.

### Verification
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- 5-run sample (`time per compile (ns)`):
  - `594902.08`
  - `557675.66`
  - `562927.78`
  - `565014.96`
  - `544638.26`
  - median: `562927.78`

## Statement Logic De-dup Pass (less code, same behavior)
Reduced duplicated assignment/update statement code shared by entry and function emit paths.

### Code changes
- `lib/Spirdo/Wesl/Emit.hs`
  - Extracted shared helpers:
    - `withNonAtomicPtr`
    - `emitAssignStmt`
    - `emitAssignOpStmt`
    - `emitIncDecStmt`
  - Reused these from both:
    - `emitStmt`
    - `emitStmtFn`
  - Removed duplicated load helper body:
    - `emitLoadVar = emitLoadFromPtr`
  - Added `INLINE` pragmas on tiny shared helpers to keep overhead low.
  - Net diff in `Emit.hs` for this branch state: fewer lines overall (`-102/+94` with other current edits).

### Verification
- Command: `cabal test`
- Result: pass (`1 of 1 test suites`)

- Command: `cabal bench`
- 10-run sample (`time per compile (ns)`):
  - `628290.22`
  - `542343.6`
  - `545570.16`
  - `559510.42`
  - `554940.28`
  - `542803.82`
  - `540741.22`
  - `575096.64`
  - `534601.9`
  - `548340.16`
  - median (10-run): `546955.16`

## Next Optimizations (1/2/3/4 request)
Executed requested sequence with test+bench after each step.

### Step 1: stream SPIR-V output directly to `Builder`
- Change:
  - `emitSpirv` now calls `buildSpirvBytes` directly (no intermediate `[Word32]` result at top level).
  - added `encodeInstrBuilder` and `buildSpirvBytes` in `lib/Spirdo/Wesl/Emit.hs`.
- Verification:
  - `cabal test`: pass
  - bench sample: `611506.0`, `616433.96`, `620497.74`, `621575.72`, `610370.02`
  - median: `616433.96` (regression)

### Step 2: `encodeString` single-pass packing
- Change:
  - replaced chunking (`take/drop/repeat/chunk4`) with a single-pass accumulator in `lib/Spirdo/Wesl/Emit/Encoding.hs`.
- Verification:
  - `cabal test`: pass
  - bench sample: `580157.78`, `570869.46`, `569351.66`, `567695.08`, `571037.86`
  - median: `570869.46` (improved vs step 1)

### Step 3: function lookup index
- Change:
  - added `gsFunctionsByName :: Map Text [FunctionInfo]` in `GenState`.
  - switched hot function lookup/call sites from full-list scans to name-index lookups:
    - duplicate overload check in `registerFunctions`
    - `findFunctionInfo`
    - function-call path in `emitExprStmt` and `emitFunctionCallByName`
- Verification:
  - `cabal test`: pass
  - bench sample: `589723.06`, `564636.84`, `542542.22`, `560023.68`, `553667.34`
  - median: `560023.68`

### Step 4: lexer follow-up
- Attempted a fast column-advance shortcut in lexer numeric/ident scanning.
- Result: regressed benchmark in this workload and was reverted.
- Final code keeps parser behavior unchanged from pre-step-4 state.

### Final state after 1/2/3 (4 reverted)
- `cabal test`: pass
- Final bench sample: `559895.44`, `558192.04`, `550674.9`, `559047.62`, `584055.66`
- Final median: `559047.62`

## Profiling-Driven Pass (biggest remaining hotspot)
Profiled current state and applied targeted refactors with test+bench after each kept change.

### Initial profile snapshot
- Command: `cabal bench --enable-profiling --benchmark-options='+RTS -p -RTS'`
- Profile file: `spirdo-compile-bench.prof` (`Fri Feb 6 13:48 2026`)
- Top hotspots:
  - `lookup` (`GHC.Internal.List`): `10.4% time`
  - `lexWesl`: `7.5% time`, `13.6% alloc`
  - `emitFieldExpr`: `4.5% time`
  - `emitTypeCached` path showed repeated list-cache lookups

### Attempt A (rejected): `encodeInstrBuilder` single-pass variants
- Change:
  - replaced `length + foldMap` with one-pass operand counting/building variants.
- Result:
  - both variants regressed in this workload and were reverted.
  - samples observed:
    - variant 1 median: `627348.32`
    - variant 2 median: `599796.94`
    - same-session baseline median (original implementation): `590142.52`

### Step B (kept): fast local/const lookup maps
- Files:
  - `lib/Spirdo/Wesl/Emit.hs`
- Change:
  - added internal maps for hot lookups while retaining list fields for behavior parity:
    - `fsVarsByName`, `fsValuesByName`
    - `gsConstValuesByName`
  - switched hot-path variable/const lookup sites to `Map.lookup`.
- Verification:
  - `cabal test`: pass
  - 5-run bench sample: `588962.72`, `580993.4`, `594332.1`, `585442.16`, `585524.96`
  - median: `585524.96` (small improvement vs `590142.52` same-session baseline)

### Step C (kept): `gsTypeCache` from assoc-list to `Map`
- Files:
  - `lib/Spirdo/Wesl/Emit.hs`
  - `lib/Spirdo/Wesl/Types/Layout.hs`
- Change:
  - `gsTypeCache :: Map TypeKey Word32` (was `[(TypeKey, Word32)]`)
  - all type-cache sites switched from list `lookup` to `Map.lookup`/`Map.insert`
  - `TypeKey` derives `Ord`
  - `Scalar` and `StorageFormat` derive `Ord` to support `TypeKey` ordering
- Verification:
  - `cabal test`: pass
  - 10-run bench sample:
    - `530499.94`, `536957.92`, `541279.88`, `569746.3`, `543712.08`,
    - `529306.76`, `569650.52`, `522898.4`, `528628.16`, `542145.48`
  - median (10-run): `539118.90`
  - delta vs same-session baseline (`590142.52`): `-51023.62 ns` (~`8.65%` faster)
  - delta vs prior project baseline (`559047.62`): `-19928.72 ns` (~`3.56%` faster)

### Post-change profile snapshot
- Command: `cabal bench --enable-profiling --benchmark-options='+RTS -p -RTS'`
- Profile file: `spirdo-compile-bench.prof` (`Fri Feb 6 13:50 2026`)
- Compile timing:
  - before this pass profile run: `1478534.44 ns`
  - after this pass profile run: `1298528.5 ns`
- New top hotspots:
  - `lexWesl`: `6.8% time`
  - `ReadP (<|>)`: `6.8% time`
  - `toLazyByteStringWith`: `6.8% time`
  - `lookup` dropped out of top hotspot list

## Parser + SPIR-V Emission Pass (profiling-driven)
Focused on lexing and SPIR-V serialization hot spots.

### Step D (kept): lexer numeric parsing without `read` / `readHex`
- Files:
  - `lib/Spirdo/Wesl/Parser.hs`
  - `lib/Spirdo/Wesl/Util.hs`
- Change:
  - replaced numeric parsing with direct folds + `Data.Text.Read.double`
  - reduced position updates for identifier/number scanning
  - removed `read`/`readHex` from the hot lexing path
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `665181.92`, `590151.38`, `618713.4`, `446265.46`, `527735.56`, `462773.36`, `478082.68`, `462086.96`, `457953.58`, `457448.18`
  - median (10-run): `470428`

### Step E (kept): SPIR-V output assembled as `[Word32]` + `spirvToBytes`
- Files:
  - `lib/Spirdo/Wesl/Emit.hs`
- Change:
  - removed `Builder` assembly in `buildSpirvBytes`
  - emit `[Word32]` stream with `encodeInstr`, then `spirvToBytes`
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `454379.62`, `455758.62`, `450110.64`, `450192.44`, `453696.02`, `462321.78`, `491629.38`, `496939.36`, `467825.34`, `467240.54`
  - median (10-run): `459040`

### Step F (kept): `spirvToBytes` direct strict encoding
- Files:
  - `lib/Spirdo/Wesl/Emit/Encoding.hs`
- Change:
  - replaced `Builder`-based encoding with `unsafeCreate` + little-endian writes
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `449698.04`, `442022.74`, `446689.1`, `438299.22`, `466971.68`, `443124.18`, `444150.76`, `442578.6`, `442620.02`, `434428.34`
  - median (10-run): `442872`
  - note: this is the current best median after Step E + Step F combined

### Attempt G (rejected): faster `parseTypedScalarSuffix` + `consumeLine`
- Files:
  - `lib/Spirdo/Wesl/Util.hs`
  - `lib/Spirdo/Wesl/Parser.hs`
- Result:
  - regressed median; reverted to prior behavior.
  - sample (10-run) with change: `439998.62`, `470882.98`, `452326.94`, `454542.12`, `455954.3`, `450170.96`, `446388.38`, `439068.24`, `458920.3`, `446844.18`
  - median: `451249`

### Attempt H (rejected): `freshId` INLINE
- File:
  - `lib/Spirdo/Wesl/Emit.hs`
- Result:
  - 10-run sample: `443196.76`, `450005.48`, `447035.12`, `456653.42`, `441844.76`, `450916.08`, `454856.26`, `482680.38`, `448424.1`, `450850.1`
  - median: `450428`

### Attempt I (rejected): guard `parseTypedScalarSuffix` at parseType call site
- File:
  - `lib/Spirdo/Wesl/Parser.hs`
- Result:
  - 10-run sample: `496909.26`, `464149.74`, `452002.24`, `451233.86`, `455188.62`, `449249.88`, `448233.3`, `469945.7`, `450790.86`, `451211.26`
  - median: `451618`

### Attempt J (rejected): prefilter `parseVectorCtorName`/`parseMatrixCtorName` by prefix
- File:
  - `lib/Spirdo/Wesl/Util.hs`
- Result:
  - 10-run sample: `440036.14`, `441909.34`, `466979.3`, `456608.2`, `442550.32`, `452093.44`, `440993.72`, `465643.32`, `446418.68`, `478245.22`
  - median: `449256`

### Attempt K (rejected): whitespace chunk skipping in lexer
- File:
  - `lib/Spirdo/Wesl/Parser.hs`
- Result:
  - 10-run sample: `446599.04`, `470129.64`, `448442.22`, `451615.8`, `457102.16`, `457186.76`, `447093.84`, `439598.7`, `450443.62`, `449977.4`
  - median: `450211`

### Attempt L (rejected): direct SPIR-V emission into ByteString buffer
- File:
  - `lib/Spirdo/Wesl/Emit.hs`
- Result:
  - 10-run sample: `446727.1`, `453967.04`, `471108.3`, `456816.6`, `474287.08`, `452225.24`, `442329.54`, `441713.34`, `458310.62`, `452821.24`
  - median: `453394`

### Attempt M (rejected): drop `<...>` handling in `parseTypedScalarSuffix`
- File:
  - `lib/Spirdo/Wesl/Util.hs`
- Result:
  - tests failed (`vec4<f32>` typed constructor not recognized); reverted.

### Attempt N (rejected): manual hex prefix check in lexer
- File:
  - `lib/Spirdo/Wesl/Parser.hs`
- Change:
  - replaced `T.stripPrefix "0x"/"0X"` with manual `T.uncons` checks and a shared decimal parser path
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `441417.12`, `457613.2`, `481776.04`, `455496.82`, `448846.66`, `451566.46`, `446207.1`, `454908.42`, `475732.66`, `445698.68`
  - median (10-run): `453237.44`
- Result:
  - regressed median; reverted.

### Attempt O (rejected): precomputed ctor tables for vec/mat names
- File:
  - `lib/Spirdo/Wesl/Util.hs`
- Change:
  - added `Map` lookups for common `vec*`/`mat*` ctor names (suffix and `<...>` forms), fallback to existing parsing for uncommon sizes
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `466042.12`, `489663.34`, `487930.76`, `480282.02`, `485627.38`, `471912.28`, `478463.62`, `464638.14`, `473454.88`, `496996.08`
  - median (10-run): `479372.82`
- Result:
  - regressed median; reverted.

### Attempt P (rejected): batch `freshId` allocation in hot control-flow paths
- File:
  - `lib/Spirdo/Wesl/Emit.hs`
- Change:
  - added `freshIds2/3/4` and replaced sequential `freshId` calls in `emitIfFn`, `emitWhileFn`, `emitLoopFn`, `emitForFn`, `emitSwitchChainFn`, `emitMainFunction`
- Verification:
  - `cabal test`: pass
  - 10-run bench sample: `458821.56`, `474577.64`, `466971.08`, `479909.4`, `454541.78`, `471567.88`, `472568.24`, `446104.26`, `448401.64`, `460213.74`
  - median (10-run): `463592.41`
- Result:
  - regressed median; reverted.

### Attempt Q (kept): raw `wesl` + inline import linking API
- Files:
  - `lib/Spirdo/Wesl/Compiler.hs`
  - `lib/Spirdo/Wesl/Typecheck.hs`
  - `lib/Spirdo/Wesl/Types.hs`
  - `lib/Spirdo/Wesl/Types/Interface.hs`
  - `lib/Spirdo/Wesl/Reflection.hs`
  - `README.md`, `MIGRATION.md`
  - examples + tests
- Change:
  - `wesl` now returns raw source; `weslShader` keeps compile-time shader output.
  - Added `Imports` and `spirv` for compile-time import linking.
- Verification:
  - `cabal test`: pass
  - `cabal bench`: time per compile `485205.54`
- Result:
  - Slight regression vs best median (~459k), but within recent single-run noise.

### Attempt R (kept): HList-style inline imports
- Files:
  - `lib/Spirdo/Wesl/Types.hs`
  - `lib/Spirdo/Wesl/Reflection.hs`
  - `README.md`
- Change:
  - Switched `Imports` to a GADT with `:>` constructor and `Import` specs for HList-style composition.
- Verification:
  - `cabal test`: pass
  - `cabal bench`: time per compile `465183.42`
- Result:
  - Improved vs prior run; still slightly above best median (~459k).

### Attempt S (kept): avoid `imports` shadowing
- Files:
  - `lib/Spirdo/Wesl/Typecheck.hs`
- Change:
  - Renamed local `imports` bindings to `importItems` to avoid shadowing the new `imports` helper.
- Verification:
  - `cabal test`: pass
  - `cabal bench`: time per compile `454429.36`
- Result:
  - Slight improvement vs prior run.

### Attempt T (reverted): low-risk allocation trims
- Files:
  - `lib/Spirdo/Wesl/Emit.hs`
  - `lib/Spirdo/Wesl/Compiler.hs`
  - `lib/Spirdo/Wesl/Typecheck.hs`
- Change:
  - replaced `zip [0..]` member loops with indexed folds
  - removed tuple allocation in `weslCacheKey` hashing
  - de-duped import targets via `Set` fold
- Verification:
  - pre bench: `484895.6`
  - post bench: `506064.42`
  - pre profile: `1047202.02` (profiling build)
  - post profile: `1085161.76` (profiling build)
- Result:
  - regression on this run; reverted.

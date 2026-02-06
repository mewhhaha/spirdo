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

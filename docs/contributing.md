# Contributing to Spirdo

Start with [the architecture overview](architecture.md). Compiler changes are
easiest to review when one diff owns one phase or invariant.

## Local checks

Run the complete library build and test suite:

```sh
cabal build all --enable-tests --enable-benchmarks
cabal test spirdo-tests --test-show-details=direct
```

Run a focused test by its `section/name` substring:

```sh
cabal test spirdo-tests \
  --test-show-details=direct \
  --test-options='--match short-circuit'
```

`SPIRDO_TEST_FILTER=short-circuit` provides the same filter when invoking the
test executable directly.

The normal suite skips unavailable external validators. Reproduce the CI gate
with both `spirv-val` and `naga` on `PATH`:

```sh
SPIRDO_REQUIRE_VALIDATORS=1 \
  cabal test spirdo-tests --test-show-details=direct
```

Check packaging before a release-facing change:

```sh
cabal check
cabal sdist --list-only
```

## Benchmarks

The benchmark runs several sequential compiler shapes rather than treating one
shader as representative of the whole pipeline:

```sh
cabal bench spirdo-compile-bench
SPIRDO_BENCH_ITERS=100 cabal bench spirdo-compile-bench
```

Record a baseline, change one thing, repeat enough times to compare medians,
and include allocation or profiling evidence for representation changes. Keep
the sequential path as the reference implementation.

## Parity corpus

The manifest and external-oracle workflow is documented in
[parity.md](parity.md). Run its lint/generation checks whenever a manifest,
rule, snapshot pin, or generated index changes.

## Examples

The demo is a separate package with SDL3/Slop system dependencies:

```sh
just demo
# or: (cd examples && cabal run spirdo-demo)

just game
# or: (cd examples && cabal run spirdo-game)
```

Library changes must not introduce demo dependencies into the root package.
The examples project selects GHC 9.12.2 to match the pinned Slop revision. The
game logic can be tested without a GPU via `just game-test`.

## Change discipline

- Preserve source positions and attach the failing value or path to errors.
- Add a regression through a public compiler path for every fixed bug.
- Keep parser, semantic, reflection, emission, and host-input responsibilities
  separate.
- Update `README.md` when public syntax, semantics, options, or APIs change.
- Update `CHANGELOG.md` for user-visible behavior, especially breaking type
  changes or newly rejected invalid programs.
- Never update golden SPIR-V as a side effect of an ordinary test run. Inspect
  and explain the lowering difference first.

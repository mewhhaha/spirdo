# List the available local workflows.
default:
    @just --list

# Build the library, tests, and benchmarks without the optional demo.
build:
    cabal build all --enable-tests --enable-benchmarks

# Run the normal test suite; external validators are optional here.
test:
    cabal test spirdo-tests --test-show-details=direct

# Require spirv-val and Naga on PATH while running the test suite.
validate:
    SPIRDO_REQUIRE_VALIDATORS=1 cabal test spirdo-tests --test-show-details=direct

# Check the parity manifest, oracle normalization, and generated parity files.
parity:
    python3 scripts/parity/lint_manifest.py --manifest test/parity/manifest.tsv --rules test/parity/rules.tsv --allowlist test/parity/cts_allowlist.tsv --blocklist test/parity/cts_blocklist.tsv
    python3 scripts/parity/normalize_oracles.py --check
    bash scripts/parity/fetch_cts.sh
    python3 scripts/parity/index_cts.py
    git diff --exit-code -- test/parity/generated/cts_index.tsv
    python3 scripts/parity/generate_manifest.py --manifest test/parity/manifest.tsv --rules test/parity/rules.tsv --cts-index test/parity/generated/cts_index.tsv --output-candidates test/parity/generated/cts_manifest_candidates.tsv --output-wgsl-rules test/parity/generated/wgsl_rule_index.tsv --output-wesl-rules test/parity/generated/wesl_rule_index.tsv --check
    python3 scripts/parity/verify_idempotent.py --path test/parity/manifest.tsv --path test/parity/rules.tsv -- python3 scripts/parity/promote_cts_backlog.py
    python3 scripts/parity/verify_idempotent.py --path test/parity/manifest.tsv --path test/parity/fixtures/cts_backlog -- python3 scripts/parity/materialize_cts_backlog_fixtures.py

# Run the sequential compiler benchmark; set SPIRDO_BENCH_ITERS to change repetitions.
bench:
    cabal bench spirdo-compile-bench

# Run the optional SDL3/Slop gallery demo.
demo:
    cd examples && cabal run spirdo-demo

# Run the small shader-driven 3D crystal collection game.
game:
    cd examples && cabal run spirdo-game

# Test the pure game state transitions without opening a window.
game-test:
    cd examples && cabal test spirdo-game-tests --test-show-details=direct

# Capture one game frame under Xvfb; use Lavapipe when no physical GPU is available.
game-capture output="/tmp/spirdo-game.png":
    bash scripts/capture_game.sh "{{output}}"

# Run the demo and write its SPIR-V outputs in examples/.
demo-spv:
    cd examples && SPIRDO_WRITE_SPV=1 cabal run spirdo-demo

# Run local build, tests, parity checks, and Cabal package checks.
check: build test parity
    cabal check
    cabal sdist --list-only > /dev/null

# WESL and WGSL Parity Checks

Spirdo tracks its substantial, explicitly tested WGSL/WESL subset in a
manifest-backed corpus and uses Naga plus `spirv-val` as independent oracles.
The manifest is the coverage authority, not a claim of full language
conformance. Its results are compiler acceptance and structural validation; they
do not mean every shader has executed on a GPU.

## Files

- `test/parity/manifest.tsv` lists every case, expected result, source,
  options, origin, and optional oracle.
- `test/parity/rules.tsv` maps spec rules to cases.
- `test/parity/pins.json` records upstream revisions and tool versions.
- `test/parity/cts_allowlist.tsv` and `cts_blocklist.tsv` control CTS import.
- `test/parity/generated/*.tsv` contains reproducible indexes.
- `docs/spec-snapshots.md` documents snapshot policy and current pins.

Manifest expectations are `pass`, `fail`, or `xfail`. An `xfail` row must have
an owner and a concrete exit criterion. `backlog` rows are materialized and run;
`backlog-unmapped` rows remain traced to upstream source but are not executable
fixtures yet.

Feature-gated cases record their required source feature, such as
`unrestricted_pointer_parameters`. A corpus case is not general support for a
feature unless its manifest row and source directives say so. In particular,
write-only storage buffers remain unsupported. Pointer parameters are
validator-backed for `function`, `private`, and gated `workgroup`/`storage`
uses. `ptr<uniform, ...>` parameters remain an explicit `xfail`: SPIR-V
Logical addressing cannot preserve their forwarding and address-space
semantics with the current backend, so Spirdo rejects them rather than silently
changing the program. Partial-pointer arguments are supported for `storage` and
`workgroup`; `function` and `private` calls require whole-variable pointer
roots. Pointer returns are likewise rejected.

## Local validation

Run manifest and generated-file checks:

```sh
python3 scripts/parity/lint_manifest.py \
  --manifest test/parity/manifest.tsv \
  --rules test/parity/rules.tsv \
  --allowlist test/parity/cts_allowlist.tsv \
  --blocklist test/parity/cts_blocklist.tsv

python3 scripts/parity/normalize_oracles.py --check

scripts/parity/fetch_cts.sh
python3 scripts/parity/index_cts.py

python3 scripts/parity/generate_manifest.py \
  --manifest test/parity/manifest.tsv \
  --rules test/parity/rules.tsv \
  --cts-index test/parity/generated/cts_index.tsv \
  --output-candidates test/parity/generated/cts_manifest_candidates.tsv \
  --output-wgsl-rules test/parity/generated/wgsl_rule_index.tsv \
  --output-wesl-rules test/parity/generated/wesl_rule_index.tsv \
  --check
```

Require both external validators for the executable corpus:

```sh
SPIRDO_REQUIRE_VALIDATORS=1 \
  cabal test spirdo-tests --test-show-details=direct
```

## Updating from CTS

The upstream checkout remains outside the package source distribution. `just
parity` and CI fetch the pinned snapshot before indexing and checking generated
artifacts; neither installs validators. To refresh the snapshot intentionally:

```sh
scripts/parity/fetch_cts.sh
python3 scripts/parity/index_cts.py
python3 scripts/parity/promote_cts_backlog.py
python3 scripts/parity/materialize_cts_backlog_fixtures.py
```

Review provenance and expectations before committing generated changes. Never
turn a failing case into `pass` merely because one oracle is unavailable.

## What the oracles prove

- A Spirdo `pass` proves the source reached SPIR-V emission.
- `spirv-val` proves structural validity under the selected SPIR-V environment.
- A Naga oracle compares WGSL acceptance or rejection.
- Golden files detect exact binary changes for a small representative set.

None of these observes shader output. Arithmetic, memory-model, texture, and
control-flow execution claims need a separate execution test that compares a
buffer or pixel result.

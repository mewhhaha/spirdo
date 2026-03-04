# Parity Spec Snapshots

This file pins the spec references used by the parity fixture matrix in
`test/parity/manifest.tsv`.
The required parity scope is enumerated in `test/parity/rules.tsv`.
Pinned source metadata is tracked in `test/parity/pins.json`.

## Snapshot Policy

- Update these pins only in dedicated parity-update changes.
- Keep fixture `spec_ref` tags stable and machine-searchable.
- Any snapshot update must include test corpus updates in the same change.

## Current Pins

- WESL profile:
  - Source: `README.md` WGSL compatibility profile and parser/typecheck constraints in this repo.
  - Snapshot date: `2026-03-02`.
  - Baseline paths:
    - `README.md` (feature + compatibility claims)
    - `lib/Spirdo/Wesl/Parser.hs`
    - `lib/Spirdo/Wesl/Typecheck.hs`

- WGSL:
  - Source: GPUWeb WGSL specification (W3C TR).
  - URL: `https://www.w3.org/TR/WGSL/`
  - Snapshot date: `2026-03-02`.
  - CTS repo: `https://github.com/gpuweb/cts.git`
  - CTS commit: `d213d4b8dba58ca7a0685e30cfaf1d29f4fc5d5b`
  - Parity sections are tracked via `spec_ref` labels in the manifest.

- Naga:
  - Source: `naga-cli`
  - Version pin: `28.0.0`
  - Origin: pinned in `.github/workflows/parity-tests.yml`
  - Policy: `python3 scripts/parity/normalize_oracles.py --check` runs in `parity-lint` and fails on drift.

## Manifest Contract

`test/parity/manifest.tsv` columns:

1. `id`
2. `domain`
3. `spec_ref`
4. `kind`
5. `expected` (`pass`, `fail`, `xfail`)
6. `source`
7. `origin` (`manual`, `cts`)
8. `origin_ref` (stable provenance key, required)
9. `error_contains`
10. `oracles` (`spirv-val`, `naga-pass`, `naga-fail`)
11. `options` (`base`, `no-base`, `feature:<name>`, `sampler:combined|separate`, `spec:strict|parity`)
12. `owner` (required for `xfail`)
13. `exit_criteria` (required for `xfail`)

WGSL oracle rule:
- For `domain=wgsl` rows with `expected=pass`, `oracles` must include `naga-pass`.
- For `domain=wgsl` rows with `expected=fail`, `oracles` must include `naga-fail`.
- Backlog rows (`kind=backlog|backlog-unmapped`) are excluded from this oracle requirement.

`kind` values in this repository:
- `cts` / `manual` for standard parity fixtures.
- `backlog` for mapped, active backlog xfail cases.
- `backlog-unmapped` for tracked backlog cases without concrete executable materialization yet.

## Rule Coverage Contract

- `test/parity/rules.tsv` lists every `spec_ref` that must be covered.
- The test harness fails if:
  - a rule in `rules.tsv` has no case in `manifest.tsv`, or
  - a manifest `spec_ref` is not listed in `rules.tsv`.

## Generated Artifacts

- `test/parity/generated/wgsl_rule_index.tsv` and `test/parity/generated/wesl_rule_index.tsv` are generated from `rules.tsv`.
- `test/parity/generated/cts_index.tsv` is generated from shader-related CTS sources in a pinned checkout (`*.spec.ts`, `*.wgsl`, `*.wesl`).
- `test/parity/generated/cts_manifest_candidates.tsv` is generated from CTS index rows not already present in the main manifest.
- `scripts/parity/promote_cts_backlog.py` promotes all CTS candidates into manifest rows as either `backlog` (mapped) or `backlog-unmapped` (placeholder/unmapped source).
- `scripts/parity/materialize_cts_backlog_fixtures.py` materializes CTS-derived `backlog` / `backlog-unmapped` rows to file-backed fixtures under `test/parity/fixtures/cts_backlog/`.
- `scripts/parity/reclassify_backlog_expectations.sh` recompiles mapped backlog fixtures and promotes successful cases from `kind=backlog expected=xfail` to `kind=cts expected=pass`.
- Regenerate via:
  - `scripts/parity/index_cts.py`
  - `scripts/parity/generate_manifest.py`
- `scripts/parity/promote_cts_backlog.py`
- `scripts/parity/materialize_cts_backlog_fixtures.py`
- `scripts/parity/reclassify_backlog_expectations.sh`
  - `scripts/parity/lint_manifest.py`

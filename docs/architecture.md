# Spirdo Architecture

Spirdo is a renderer-independent compiler for a substantial tested WESL/WGSL
subset. Its product boundary is a shader bundle: emitted SPIR-V plus enough
reflected metadata for a host to build a pipeline and bind resources safely.

The optional SDL demo is an integration example. It is not part of the library
architecture.

## Compiler pipeline

```text
Source text or file
  │
  ├─ Parser       tokens and syntax tree
  ├─ Module graph import resolution and linking
  ├─ Semantics    names, types, constants, overrides, control-flow rules
  ├─ Reflection   stage IO, resource layouts, override metadata
  └─ SPIR-V       structured control flow, values, resources, binary encoding
       │
       ├─ Runtime ShaderBundle
       └─ Typed compile-time Shader
```

The compiler aims to keep parsing, validation, layout, and lowering pure, with
file and cache access at IO boundaries. Expected source, import, validation, or
lowering failures are represented as `Either CompileError`, not host
exceptions. This is a design goal rather than a claim of a fully separate,
fully checked IR: some semantic validation and layout checks still live in
`Emit` beside lowering.

## Public surfaces

- `Spirdo.Wesl` is the small runtime API. It accepts inline or file sources and
  returns a renderer-facing `ShaderBundle`.
- `Spirdo.Wesl.Reflection` is the advanced/static API. It provides the `spirv`
  Template Haskell entry point, typed shaders, and full reflection.
- `Spirdo.Wesl.Inputs` validates named resources against a typed shader
  interface and normalizes them into binding order.
- `Spirdo.Wesl.Uniform` converts Haskell values to the reflected WESL memory
  layout.

Constructors whose values carry type-level interface claims are intentionally
opaque. Callers observe shaders through accessors and obtain them only from a
successful compiler path.

## Internal modules

- `Parser` owns lexical and grammatical recognition. It must not perform file
  IO or emit SPIR-V.
- `Syntax` owns the parsed language representation and source positions.
- `Typecheck` owns import resolution, linking, semantic validation, constant
  evaluation, diagnostics, and name qualification.
- `Emit` owns reflected layouts and SPIR-V lowering. It also retains some
  source-level layout and semantic checks; new work should move checks earlier
  only when that preserves clear errors and phase boundaries.
- `Compiler` orchestrates phases and contains the file, Template Haskell, and
  cache boundaries.
- `SourceFile` is the shared bounded UTF-8 reader for source and manifest IO.
- `Types.*` owns public runtime and type-level representations.

The large semantic and emission modules contain several distinct subsystems.
New work should follow the existing phase boundaries rather than adding a new
cross-phase shortcut. When a subsystem can be extracted with a behavior-neutral
diff and a focused test surface, prefer a narrow internal module with an
explicit export list.

## Required invariants

The following are compiler invariants, not conventions:

- A module is loaded and merged at most once, including diamond import graphs.
- Canonical filesystem paths identify loaded files only. Semantic names use an
  injective package-route/module identity, stay stable when a checkout moves,
  and are rendered as source-qualified names at the reflection boundary. A
  physical file reached through two package identities is parsed once but
  linked once per identity.
- Filesystem source and package graphs have fixed per-file, aggregate, count,
  and depth budgets. Canonical cache hits do not consume the same budget twice.
- An item import resolves to a declaration that exists in the target module.
- Module and struct namespaces contain no ambiguous duplicate declarations.
- Constant evaluation is total, bounded, width-aware, and follows runtime
  short-circuit and coercion semantics. Scalar abstract floats and literals in
  typed composites remain binary64 until checked f32/f16 materialization; half
  conversion rounds directly from that value. Targetless composite inference
  remains concrete-f32 because composite layouts store a concrete `Scalar`.
- Structured control flow is validated before emission; `break`, `continue`,
  and `break if` are legal only in their WGSL contexts.
- Lexical bindings do not escape their block, even when shadowing diagnostics
  are disabled.
- Reflected offsets, sizes, alignments, and binding slot counts use checked
  arithmetic and never wrap.
- Sampler mode is consistent at the type level, in runtime reflection, and in
  emitted resources.
- Cache entries are versioned, bounded, and atomically written. Reuse checks
  validate their exact input identity, reflection shape, and SPIR-V shape, but
  cache files are local compiler optimization state, not authenticated input.
- Compiler errors retain the most precise source evidence available.

Enforce invariants at the earliest boundary that has enough information. Do
not make the emitter repair an invalid semantic program or make host input code
repair an invalid reflected interface. When a check currently belongs to `Emit`,
keep it explicit rather than pretending it was already discharged upstream.

## Testing layers

The test suite has four complementary jobs:

1. Focused regressions exercise public behavior for parser, semantic, API, and
   cache bugs.
2. The parity manifest checks the accepted/rejected WESL and WGSL corpus.
3. `spirv-val` and Naga act as external structural and compatibility oracles.
4. Golden binaries detect intentional changes to representative complete
   modules.

Parity corpus acceptance is not GPU execution conformance. A test named after
an execution CTS case proves compilation unless it also executes and observes
the shader.

Tests must run without external validators by default. CI sets
`SPIRDO_REQUIRE_VALIDATORS=1` on the validator job so missing tools cannot turn
that gate into a skip.

## Performance policy

Keep the sequential compiler as the source of truth. Measure changes against
the benchmark scenarios before changing representations or adding
parallelism. Compile independent shaders in parallel at the outer boundary;
do not introduce shared mutable state into one shader compilation.

Retain rejected benchmark experiments in `PERF_REFACTOR_REPORT.md` so known
regressions are not repeatedly rediscovered.

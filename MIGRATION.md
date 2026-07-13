# Spirdo API Migration

This guide maps the former Shader-centric surface to the current runtime bundle
and advanced Reflection APIs.

## Choose an API boundary

- Use `Spirdo.Wesl` for runtime SPIR-V compilation and compact
  renderer-facing metadata.
- Use `Spirdo.Wesl.Inputs` with a typed Reflection `Shader` to validate
  resource submission.
- Use `Spirdo.Wesl.Reflection` for Template Haskell, complete layouts,
  binding plans, typed interfaces, and `CompileOptions`.
- Use `Spirdo.Wesl.Uniform` for layout-aware packing primitives and `ToUniform`.

## Construction is compiler-owned

The runtime module keeps `Source` and `ShaderBundle` abstract. Construct a
source with `sourceText`, `sourceNamed`, or `sourceFile`; only a successful
`compile` produces a bundle. `ShaderBundle` has no `Read` instance.

Reflection exposes more data for inspection, but `Shader` is still abstract:
its constructor is hidden, and applications obtain it through `spirv`,
`compileWith`, or `compileFileWith`. `InputsBuilder` is likewise opaque; use
the named builders and let `inputsFor` validate them.

```hs
-- Old: direct source/shader construction
compile (SourceInline "<inline>" src) :: Either CompileError SomeShader

-- New: runtime bundle API
compile [] (sourceNamed "effect.wesl" src) :: IO (Either CompileError ShaderBundle)
compile [] (sourceFile "shaders/effect.wesl") :: IO (Either CompileError ShaderBundle)
```

`sourceNamed` is for generated or embedded text and supplies an error label.
It cannot resolve filesystem imports. `sourceFile` is for a top-level file and
uses filesystem/package import resolution.

Use `compileWithDiagnostics` to receive non-fatal warnings and informational
diagnostics. Positions are best effort: some AST and emission errors have no
source location yet.

## Binding slots are per descriptor group

The removed singular `bindingSlotCount` has been replaced by group-aware APIs:

```hs
bindingSlotCounts :: [BindingInfo] -> [BindingSlotCount]
singleGroupBindingSlotCount :: [BindingInfo] -> Either String (Maybe BindingSlotCount)

data BindingSlotCount = BindingSlotCount
  { bscGroup :: Word32
  , bscSlots :: Word64
  }
```

`bindingSlotCounts` returns one count per descriptor group. Each `bscSlots` is
the highest binding number in that group plus one, not a declaration count, so
bindings `0` and `7` need eight addressable slots. `Word64` represents the
entire `Word32` binding range without wrapping. Use
`singleGroupBindingSlotCount` only when the receiving API requires exactly one
group; empty bindings yield `Right Nothing` and multiple groups yield `Left`.

```hs
slotCounts = bindingSlotCounts (shaderPlan shader).bpBindings
-- [BindingSlotCount { bscGroup = 0, bscSlots = 8 }]
```

## Template Haskell and inline imports

`wesl` now returns raw source. Compile it through the single `spirv` path:

```hs
shader = $(spirv defaultCompileOptions imports [wesl|
  @compute @workgroup_size(1)
  fn main() {}
|])
```

For in-memory imports, use `imports <: module_ @"math" mathSource`. The typed
map must match source imports exactly. It does not resolve filesystem packages.
The old `weslWith`, `weslShader*`, and `weslBatch*` APIs were intentionally
removed.

## Resource builders and empty inputs

Inputs are mode-indexed and declarative:

```hs
inputsFor shader $
  uniform @"params" params
    <> sampledTexture @"albedo" textureHandle samplerHandle
```

`InputsCombined iface` accepts `sampledTexture`; `InputsSeparate iface` accepts
`texture` and `sampler`. `inputsFor` infers the mode from `Shader mode iface`,
then rejects missing, duplicate, and wrong-kind bindings and normalizes a
successful result by `(group, binding, name)`.

For a shader with no bindings, use the ordinary monoidal identity:

```hs
inputsFor shader mempty
```

There is no public `emptyInputs` or `emptyInputsUnchecked` constructor. The
normal builder API remains opaque and validated.

## Uniform packing and runtime arrays

Prefer `uniform`, `inputsFor`, or `packUniformFrom`; they use `ToUniform` and
the reflected WESL layout. Runtime arrays have reflected size zero and a
nonzero stride. Their actual footprint depends on the bound storage buffer, so
host allocation and packing helpers reject them. They are permitted only as a
direct storage type or final member of a storage-buffer struct, never in a
uniform buffer.

The old `validateUniformStorable` and `packUniformStorable` names are the only
retained deprecations. They alias the intentionally explicit APIs below, which
check size and alignment only—not offsets, padding, representation, byte
order, or cross-platform `Storable` ABI compatibility:

```hs
validateUniformStorableUnchecked layout (Proxy @HostRecord)
packUniformStorableUnchecked layout hostRecord
```

## Options and cache behavior

Runtime compilation takes `[Option]`:

```hs
compile [OptEntryPoint "fast", OptOverrides [("workgroupSize", OVU32 128)]] source
```

The former runtime cache constructors and `CachePolicy` were intentionally
removed because they were misleading/no-op. Runtime `[Option]` exposes no cache
control.

Template Haskell uses `CompileOptions` instead:

```hs
let options = withCacheDir "dist-newstyle/.wesl-cache"
            . withCache True
            . withOverrides [("workgroupSize", OVU32 128)]
            . withEntryPoint "fast"
            $ defaultCompileOptions
```

`withCache`, `withCacheDir`, and `withCacheVerbose` configure a versioned,
atomically written local compiler cache. It validates corruption and artifact
shape defensively, but provides no authentication. Do not commit, download, or
share cache directories: entries may include exact source text and are not a
portable artifact format.

`SpecStrict` is the default override mode. It omits `SpecId` from derived and
composite/non-scalar-literal specialization instructions for validator
compatibility. `SpecParity` assigns IDs for WESL parity, which some
`spirv-val` versions reject for derived or composite forms.

Unsuffixed floating-point literals now remain binary64 `AbstractFloat` values
through scalar constant evaluation and are checked only when materialized as
f32 or f16. Code that previously depended on premature f32 rounding may now
produce a different, specification-aligned constant or a target-range error.

Pointer parameters keep pointer identity for `function` and `private` address
spaces. `workgroup` and `storage` additionally require
`enable unrestricted_pointer_parameters;` in the declaring module and compiler
authorization through `OptEnableFeature` or `withFeatures`; `uniform` pointer
parameters and all pointer returns are rejected. Partial-pointer arguments are
supported only in `storage` and `workgroup`; `function` and `private` pointer
arguments must identify a whole variable root.

A workgroup-size override without an initializer now compiles without an
`overrideValues` entry. Its reflected workgroup size is `Nothing` because no
default is known. SPIR-V contains a required zero-valued structural placeholder,
but zero is not a usable pipeline default: specialize the dimension to a
positive value before pipeline creation or dispatch.

## File packages

File compilation tries the exact path, then `.wesl`, then `.wgsl`, and finds
the nearest `wesl.toml`. The supported manifest subset requires
`[package].edition`; `root` must be relative and defaults to `./shaders`. Path
dependencies are relative to their manifest and recurse. Use
`dependency::module` imports for dependency aliases; `package::module` refers
to the current package's own root. Version-only and package-manager dependencies
are unsupported, and canonical containment rejects paths or symlinks escaping
a package root.

Imported reflected names are now stable source-qualified names rather than
private delimiter-mangled names containing canonical filesystem paths. Current
package imports use `package::module::name`; direct dependencies use
`dependency::module::name`; transitive dependencies retain an explicit
boundary such as `outer::dependency::package::module::name`. Use those names
for binding lookup and override values. An unmarked override shorthand is
accepted when unique and otherwise reports its canonical candidates; an exact
reflected key always takes precedence. Old raw `__wesl__...` override keys are
intentionally unsupported because they were undocumented, non-injective, and
checkout-dependent. WESL keywords, including `package`, are no longer accepted
as dependency aliases.

File compilation also has explicit resource budgets: 1 MiB decoded characters
and 4 MiB UTF-8 per source, 256 KiB per manifest, at most 256 modules or
packages and depth 64, plus 16 MiB aggregate decoded filesystem source (entry
file and distinct imports) and 1 MiB aggregate manifest bytes. Programs beyond
those boundaries now fail with a `CompileError` instead of consuming unbounded
memory or recursion depth.

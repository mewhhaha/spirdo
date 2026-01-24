# Spirdo

Haskell WESL compiler + SDL3 demo app that renders shader variants (optional SPIR-V output).

## Build and Run
```sh
cabal build
cabal run
```

Use left/right arrow keys to cycle fragment shader variants in the demo window.

## Example Usage (Quasiquoter)
```hs
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString as BS
import Spirdo.Wesl (compileWeslToSpirvWith, defaultCompileOptions, wesl)

main :: IO ()
main = do
  let shader = [wesl|
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]
  case compileWeslToSpirvWith defaultCompileOptions shader of
    Left err -> error (show err)
    Right compiled -> BS.writeFile "example.spv" (shaderSpirv compiled)
```

Note: the public API is exposed from `Spirdo.Wesl` and `Spirdo.Wesl.Inputs`.
Internal modules are not part of the supported surface area.

## Type-Safe Binding Reflection
Using the quasiquoter gives you a `CompiledShader iface` where the binding
interface is captured in the type. You can then derive the exact bindings
needed by the shader in a type-safe way without manual plumbing.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( BindingDesc(..)
  , CompiledShader
  , binding
  , bindingEither
  , bindingMaybe
  , wesl
  )

shader :: CompiledShader iface
shader = [wesl|
struct Params {
  time_res: vec4<f32>;
};

@group(0) @binding(0)
var<uniform> params: Params;

@group(0) @binding(1)
var sampler0: sampler;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]

params :: BindingDesc
params = binding @"params" shader

sampler0 :: BindingDesc
sampler0 = binding @"sampler0" shader

-- Total lookup if you prefer explicit error handling:
paramsSafe :: Either String BindingDesc
paramsSafe = bindingEither @"params" shader

paramsMaybe :: Maybe BindingDesc
paramsMaybe = bindingMaybe @"params" shader
```

Each `BindingDesc` includes the name, group (set), and binding index you need
to create descriptor layouts or bind resources on the CPU side.

Convenience filters are available: `uniformBindingsFor`, `samplerBindingsFor`,
`storageBufferBindingsFor`, and `storageTextureBindingsFor`.

### Uniform Packing Helpers
If you want to pack CPU-side data into a uniform buffer layout (std140-like),
use the `TypeLayout` from the compiled interface and the helpers in
`Spirdo.Wesl`.

```hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.List (find)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Spirdo.Wesl

shader :: CompiledShader iface
shader = [wesl|
struct Params { time: f32; };
@group(0) @binding(0) var<uniform> params: Params;
@fragment fn main(@builtin(position) p: vec4<f32>) -> @location(0) vec4<f32> {
  return vec4(0.0, 0.0, 0.0, 1.0);
}
|]

data Params = Params { time :: Float } deriving (Generic)
instance ToUniform Params

packed :: Either String BS.ByteString
packed =
  case find (\b -> biName b == "params") (siBindings (shaderInterface shader)) of
    Nothing -> Left "missing params binding"
    Just bi -> packUniformFrom (biType bi) (Params 1.5)
```

### Typed Input Lists (Host-Agnostic)
You can build a typed input list for the shader interface using `HList` and
opaque handles. This stays host‑agnostic and still enforces ordering/types.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( CompiledShader
  , uniform
  , wesl
  )
import Spirdo.Wesl.Inputs
  ( HList(..)
  , SamplerHandle(..)
  , TextureHandle(..)
  , ShaderInputs
  , inputsFor
  )

shader :: CompiledShader iface
shader = [wesl|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var samp0: sampler;
@group(0) @binding(2) var tex0: texture_2d<f32>;
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  return vec4(0.0, 0.0, 0.0, 1.0);
}
|]

inputs :: Either String (ShaderInputs iface)
inputs =
  inputsFor shader
    ( uniform (V4 0.0 0.0 0.0 0.0 :: V4 Float)
        :& SamplerHandle 1
        :& TextureHandle 2
        :& HNil
    )
```

Note: `SamplerHandle`/`TextureHandle` values are **your runtime resource IDs**, not shader binding indices. They must correspond to real resources in your renderer.

`InputsOf iface` is a type-level list derived from the shader’s bindings, so the
order and kinds are enforced at compile time. The resulting `ShaderInputs` lists
(`siUniforms`, `siSamplers`, `siTextures`, ...) are ready to hand off to your
SDL upload/bind code.

`inputsFor` is pure; handle `Either` as you prefer.

Notes:
- Record field names must match the WESL struct field names (extra or missing
  fields are errors).

### SDL3 Demo Helpers (exe only)
The demo executable ships its own SDL3 helpers under `exe/Spirdo/SDL3/Safe.hs`
and `exe/Spirdo/SDL3.hsc`. These are **not** part of the library API; they’re
only used by `exe/Main.hs`.

```hs
import Spirdo.SDL3.Safe (withSDL, withWindow, withGPURenderer)
import Spirdo.SDL3 (sdl_INIT_VIDEO, sdlGetGPURendererDevice)

main :: IO ()
main =
  withSDL sdl_INIT_VIDEO $
    withWindow "Spirdo SDL3" 800 600 0 $ \window ->
      withGPURenderer window $ \renderer -> do
        device <- sdlGetGPURendererDevice renderer
        -- ...
```


### Mapping To A Haskell Record
If you still want an explicit record in your app, you can map reflected
bindings into a concrete type:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (find)
import Spirdo.Wesl
  ( BindingDesc(..)
  , CompiledShader
  , reflectBindings
  , wesl
  )
import Data.Proxy (Proxy(..))

data MyBindings = MyBindings
  { bParams   :: BindingDesc
  , bSampler0 :: BindingDesc
  }

mkBindings :: [BindingDesc] -> Either String MyBindings
mkBindings bs = do
  params <- byName "params"
  samp0  <- byName "sampler0"
  pure MyBindings { bParams = params, bSampler0 = samp0 }
  where
    byName n =
      case find (\b -> descName b == n) bs of
        Just b -> Right b
        Nothing -> Left ("missing binding: " <> n)

shader :: CompiledShader iface
shader = [wesl|
struct Params {
  time_res: vec4<f32>;
};

@group(0) @binding(0)
var<uniform> params: Params;

@group(0) @binding(1)
var sampler0: sampler;

@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]

bindings :: Either String MyBindings
bindings = mkBindings (reflectBindings (Proxy @iface))
```

This gives you a strongly-typed `MyBindings` record to pass around your renderer.

## Example Shaders in the Demo
Fragment variants in `exe/Main.hs` (left/right to switch):
- Feature Mix
- Raymarch
- Triangle
- Plasma
- Grid
- SDF Text
- Clouds
- Bits
- Aurora
- Starfield
- Tunnel
- Voronoi
- Mandelbrot
- Override Stripes / Override Rings (specialization values)

Compute examples emitted when SPIR-V output is enabled:
- `compute.spv`: storage buffer + storage texture sample
- `compute-1.spv`: particle update on runtime array
- `compute-2.spv`: tiled storage texture writer

Vertex examples emitted when SPIR-V output is enabled:
- `vertex.spv`: passthrough quad
- `vertex-1.spv`: wave-displaced positions
- `vertex-2.spv`: fullscreen triangle (vertex_index)

## SPIR-V Outputs
SPIR-V output is opt-in. Set `SPIRDO_WRITE_SPV=1` before running the demo:
```
SPIRDO_WRITE_SPV=1 cabal run
```

When enabled, files are written to the repo root:
- `fragment-*.spv` for each fragment variant
- `compute*.spv` for compute examples
- `vertex*.spv` for vertex examples

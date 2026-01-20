# Spirdo

Haskell WESL compiler + SDL3 demo app that emits SPIR-V and renders shader variants.

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
  , samplerBindingsFor
  , storageBufferBindingsFor
  , uniformBindingsFor
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

uniforms :: [BindingDesc]
uniforms = uniformBindingsFor shader

samplers :: [BindingDesc]
samplers = samplerBindingsFor shader

storageBuffers :: [BindingDesc]
storageBuffers = storageBufferBindingsFor shader

params :: BindingDesc
params = binding @"params" shader

sampler0 :: BindingDesc
sampler0 = binding @"sampler0" shader
```

Each `BindingDesc` includes the name, group (set), and binding index you need
to create descriptor layouts or bind resources on the CPU side.

### SDL-Style Input Lists (Type-Safe)
If you want simple lists of uniforms/samplers/textures to pass into an SDL3.4
style API without naming each binding, you can use the derived input list type
for the shader interface.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import Spirdo.Wesl
  ( CompiledShader
  , HList(..)
  , M4(..)
  , SDLSamplerHandle(..)
  , SDLTextureHandle(..)
  , ShaderInputs
  , inputsFor
  , uniform
  , ToUniform(..)
  , V2(..)
  , V4(..)
  , wesl
  )
import GHC.Generics (Generic)

shader :: CompiledShader iface
shader = [wesl|
struct Params { time_res: vec4<f32>; };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var sampler0: sampler;
@group(0) @binding(2) var tex0: texture_2d<f32>;
@fragment
fn main(@builtin(position) frag_coord: vec4<f32>) -> @location(0) vec4<f32> {
  let uv = vec2(frag_coord.x / 800.0, frag_coord.y / 600.0);
  return vec4(uv.x, uv.y, 0.4, 1.0);
}
|]

data Globals = Globals
  { time :: Float
  , resolution :: V2 Float
  , viewProj :: M4 Float
  } deriving (Generic)

instance ToUniform Globals

data Material = Material
  { baseColor :: V4 Float
  , roughness :: Float
  , metallic :: Float
  , pad0 :: V2 Float
  } deriving (Generic)

instance ToUniform Material

globals :: Globals
globals = ...

material :: Material
material = ...

inputs :: ShaderInputs iface
inputs =
  inputsFor shader
    ( uniform globals
        :& uniform material
        :& SDLSamplerHandle 1
        :& SDLTextureHandle 2
        :& HNil
    )
```

`InputsOf iface` is a type-level list derived from the shader’s bindings, so the
order and kinds are enforced at compile time. The resulting `ShaderInputs` lists
(`siUniforms`, `siSamplers`, `siTextures`, ...) are ready to hand off to your
SDL upload/bind code.

Notes:
- Record field names must match the WESL struct field names.
- Use `inputsForEither` if you want to handle packing/layout mismatches instead
  of throwing an error.


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

Compute examples emitted on build:
- `compute.spv`: storage buffer + storage texture sample
- `compute-1.spv`: particle update on runtime array
- `compute-2.spv`: tiled storage texture writer

Vertex examples emitted on build:
- `vertex.spv`: passthrough quad
- `vertex-1.spv`: wave-displaced positions
- `vertex-2.spv`: fullscreen triangle (vertex_index)

## SPIR-V Outputs
On build/run, SPIR-V files are written to the repo root:
- `fragment-*.spv` for each fragment variant
- `compute*.spv` for compute examples
- `vertex*.spv` for vertex examples

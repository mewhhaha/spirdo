{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Low-level SDL3 FFI bindings for the demo.
module Spirdo.SDL3
  ( SDL_Window
  , SDL_Renderer
  , SDL_Texture
  , SDL_GPUDevice
  , SDL_GPUShader
  , SDL_GPUSampler
  , SDL_GPUGraphicsPipeline
  , SDL_GPUCommandBuffer
  , SDL_GPURenderPass
  , SDL_GPUTexture
  , SDL_GPUTextureFormat
  , SDL_GPUTextureCreateInfo(..)
  , SDL_GPUSamplerCreateInfo(..)
  , SDL_GPUTextureSamplerBinding(..)
  , SDL_GPURenderState
  , SDL_GPUShaderStage
  , SDL_FColor(..)
  , SDL_GPUViewport(..)
  , SDL_GPUShaderCreateInfo(..)
  , SDL_GPURenderStateCreateInfo(..)
  , SDL_GPURasterizerState(..)
  , SDL_GPUMultisampleState(..)
  , SDL_GPUStencilOpState(..)
  , SDL_GPUDepthStencilState(..)
  , SDL_GPUColorTargetBlendState(..)
  , SDL_GPUColorTargetDescription(..)
  , SDL_GPUGraphicsPipelineTargetInfo(..)
  , SDL_GPUVertexInputState(..)
  , SDL_GPUGraphicsPipelineCreateInfo(..)
  , SDL_GPUColorTargetInfo(..)
  , sdlInit
  , sdlQuit
  , sdlCreateWindow
  , sdlDestroyWindow
  , sdlGetError
  , sdlPollEvent
  , sdlDelay
  , sdlCreateGPURenderer
  , sdlDestroyRenderer
  , sdlGetGPURendererDevice
  , sdlCreateGPURenderState
  , sdlSetGPURenderStateFragmentUniforms
  , sdlSetGPURenderState
  , sdlDestroyGPURenderState
  , sdlSetRenderDrawColor
  , sdlRenderClear
  , sdlRenderPresent
  , sdlRenderGeometryRaw
  , sdlCreateGPUDevice
  , sdlDestroyGPUDevice
  , sdlGetGPUShaderFormats
  , sdlClaimWindowForGPUDevice
  , sdlReleaseWindowFromGPUDevice
  , sdlGetGPUSwapchainTextureFormat
  , sdlCreateGPUShader
  , sdlReleaseGPUShader
  , sdlCreateGPUTexture
  , sdlReleaseGPUTexture
  , sdlCreateGPUSampler
  , sdlReleaseGPUSampler
  , sdlCreateGPUGraphicsPipeline
  , sdlReleaseGPUGraphicsPipeline
  , sdlAcquireGPUCommandBuffer
  , sdlWaitAndAcquireGPUSwapchainTexture
  , sdlBeginGPURenderPass
  , sdlBindGPUGraphicsPipeline
  , sdlBindGPUFragmentSamplers
  , sdlSetGPUViewport
  , sdlPushGPUFragmentUniformData
  , sdlDrawGPUPrimitives
  , sdlEndGPURenderPass
  , sdlSubmitGPUCommandBuffer
  , sdl_INIT_VIDEO
  , sdl_EVENT_QUIT
  , sdl_EVENT_KEY_DOWN
  , sdl_EVENT_KEY_UP
  , sdl_SCANCODE_LEFT
  , sdl_SCANCODE_RIGHT
  , sdlEventSize
  , sdlKeyboardEventScancodeOffset
  , sdlKeyboardEventRepeatOffset
  , sdl_GPU_SHADERFORMAT_SPIRV
  , sdl_GPU_SHADERSTAGE_VERTEX
  , sdl_GPU_SHADERSTAGE_FRAGMENT
  , sdl_GPU_PRIMITIVETYPE_TRIANGLELIST
  , sdl_GPU_SAMPLECOUNT_1
  , sdl_GPU_FILLMODE_FILL
  , sdl_GPU_CULLMODE_NONE
  , sdl_GPU_FRONTFACE_COUNTER_CLOCKWISE
  , sdl_GPU_COMPAREOP_ALWAYS
  , sdl_GPU_LOADOP_CLEAR
  , sdl_GPU_STOREOP_STORE
  , sdl_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT
  , sdl_GPU_TEXTUREUSAGE_SAMPLER
  , sdl_GPU_TEXTUREUSAGE_COLOR_TARGET
  , sdl_GPU_TEXTURETYPE_2D
  , sdl_GPU_FILTER_NEAREST
  , sdl_GPU_FILTER_LINEAR
  , sdl_GPU_SAMPLERMIPMAPMODE_NEAREST
  , sdl_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
  ) where

#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3/SDL_render.h>
#include <SDL3/SDL_pixels.h>
#include <SDL3/SDL_events.h>
#include <SDL3/SDL_scancode.h>

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- Opaque SDL types

data SDL_Window

data SDL_Renderer

data SDL_Texture

data SDL_GPUDevice

data SDL_GPUShader

data SDL_GPUSampler

data SDL_GPUGraphicsPipeline

data SDL_GPUCommandBuffer

data SDL_GPURenderPass

data SDL_GPUTexture

data SDL_GPURenderState

data SDL_GPUVertexBufferDescription

data SDL_GPUVertexAttribute

-- Basic constants

sdl_INIT_VIDEO :: Word32
sdl_INIT_VIDEO = #{const SDL_INIT_VIDEO}

sdl_EVENT_QUIT :: Word32
sdl_EVENT_QUIT = #{const SDL_EVENT_QUIT}

sdl_EVENT_KEY_DOWN :: Word32
sdl_EVENT_KEY_DOWN = #{const SDL_EVENT_KEY_DOWN}

sdl_EVENT_KEY_UP :: Word32
sdl_EVENT_KEY_UP = #{const SDL_EVENT_KEY_UP}

sdlEventSize :: Int
sdlEventSize = #{size SDL_Event}

sdlKeyboardEventScancodeOffset :: Int
sdlKeyboardEventScancodeOffset = #{offset SDL_KeyboardEvent, scancode}

sdlKeyboardEventRepeatOffset :: Int
sdlKeyboardEventRepeatOffset = #{offset SDL_KeyboardEvent, repeat}

sdl_SCANCODE_LEFT :: Word32
sdl_SCANCODE_LEFT = #{const SDL_SCANCODE_LEFT}

sdl_SCANCODE_RIGHT :: Word32
sdl_SCANCODE_RIGHT = #{const SDL_SCANCODE_RIGHT}

sdl_GPU_SHADERFORMAT_SPIRV :: Word32
sdl_GPU_SHADERFORMAT_SPIRV = #{const SDL_GPU_SHADERFORMAT_SPIRV}

sdl_GPU_SHADERSTAGE_VERTEX :: SDL_GPUShaderStage
sdl_GPU_SHADERSTAGE_VERTEX = fromIntegral (#{const SDL_GPU_SHADERSTAGE_VERTEX} :: CInt)

sdl_GPU_SHADERSTAGE_FRAGMENT :: SDL_GPUShaderStage
sdl_GPU_SHADERSTAGE_FRAGMENT = fromIntegral (#{const SDL_GPU_SHADERSTAGE_FRAGMENT} :: CInt)

sdl_GPU_PRIMITIVETYPE_TRIANGLELIST :: SDL_GPUPrimitiveType
sdl_GPU_PRIMITIVETYPE_TRIANGLELIST = fromIntegral (#{const SDL_GPU_PRIMITIVETYPE_TRIANGLELIST} :: CInt)

sdl_GPU_SAMPLECOUNT_1 :: SDL_GPUSampleCount
sdl_GPU_SAMPLECOUNT_1 = fromIntegral (#{const SDL_GPU_SAMPLECOUNT_1} :: CInt)

sdl_GPU_FILLMODE_FILL :: SDL_GPUFillMode
sdl_GPU_FILLMODE_FILL = fromIntegral (#{const SDL_GPU_FILLMODE_FILL} :: CInt)

sdl_GPU_CULLMODE_NONE :: SDL_GPUCullMode
sdl_GPU_CULLMODE_NONE = fromIntegral (#{const SDL_GPU_CULLMODE_NONE} :: CInt)

sdl_GPU_FRONTFACE_COUNTER_CLOCKWISE :: SDL_GPUFrontFace
sdl_GPU_FRONTFACE_COUNTER_CLOCKWISE = fromIntegral (#{const SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE} :: CInt)

sdl_GPU_COMPAREOP_ALWAYS :: SDL_GPUCompareOp
sdl_GPU_COMPAREOP_ALWAYS = fromIntegral (#{const SDL_GPU_COMPAREOP_ALWAYS} :: CInt)

sdl_GPU_LOADOP_CLEAR :: SDL_GPULoadOp
sdl_GPU_LOADOP_CLEAR = fromIntegral (#{const SDL_GPU_LOADOP_CLEAR} :: CInt)

sdl_GPU_STOREOP_STORE :: SDL_GPUStoreOp
sdl_GPU_STOREOP_STORE = fromIntegral (#{const SDL_GPU_STOREOP_STORE} :: CInt)

sdl_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT :: SDL_GPUTextureFormat
sdl_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT = #{const SDL_GPU_TEXTUREFORMAT_R16G16B16A16_FLOAT}

sdl_GPU_TEXTUREUSAGE_SAMPLER :: SDL_GPUTextureUsageFlags
sdl_GPU_TEXTUREUSAGE_SAMPLER = #{const SDL_GPU_TEXTUREUSAGE_SAMPLER}

sdl_GPU_TEXTUREUSAGE_COLOR_TARGET :: SDL_GPUTextureUsageFlags
sdl_GPU_TEXTUREUSAGE_COLOR_TARGET = #{const SDL_GPU_TEXTUREUSAGE_COLOR_TARGET}

sdl_GPU_TEXTURETYPE_2D :: SDL_GPUTextureType
sdl_GPU_TEXTURETYPE_2D = fromIntegral (#{const SDL_GPU_TEXTURETYPE_2D} :: CInt)

sdl_GPU_FILTER_NEAREST :: SDL_GPUFilter
sdl_GPU_FILTER_NEAREST = fromIntegral (#{const SDL_GPU_FILTER_NEAREST} :: CInt)

sdl_GPU_FILTER_LINEAR :: SDL_GPUFilter
sdl_GPU_FILTER_LINEAR = fromIntegral (#{const SDL_GPU_FILTER_LINEAR} :: CInt)

sdl_GPU_SAMPLERMIPMAPMODE_NEAREST :: SDL_GPUSamplerMipmapMode
sdl_GPU_SAMPLERMIPMAPMODE_NEAREST = fromIntegral (#{const SDL_GPU_SAMPLERMIPMAPMODE_NEAREST} :: CInt)

sdl_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE :: SDL_GPUSamplerAddressMode
sdl_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE = fromIntegral (#{const SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE} :: CInt)

-- Structs

data SDL_FColor = SDL_FColor
  { fcR :: CFloat
  , fcG :: CFloat
  , fcB :: CFloat
  , fcA :: CFloat
  } deriving (Eq, Show)

instance Storable SDL_FColor where
  sizeOf _ = #{size SDL_FColor}
  alignment _ = #{alignment SDL_FColor}
  poke ptr SDL_FColor{..} = do
    pokeByteOff ptr #{offset SDL_FColor, r} fcR
    pokeByteOff ptr #{offset SDL_FColor, g} fcG
    pokeByteOff ptr #{offset SDL_FColor, b} fcB
    pokeByteOff ptr #{offset SDL_FColor, a} fcA
  peek ptr = SDL_FColor
    <$> peekByteOff ptr #{offset SDL_FColor, r}
    <*> peekByteOff ptr #{offset SDL_FColor, g}
    <*> peekByteOff ptr #{offset SDL_FColor, b}
    <*> peekByteOff ptr #{offset SDL_FColor, a}


data SDL_GPUViewport = SDL_GPUViewport
  { vpX :: CFloat
  , vpY :: CFloat
  , vpW :: CFloat
  , vpH :: CFloat
  , vpMinDepth :: CFloat
  , vpMaxDepth :: CFloat
  } deriving (Eq, Show)

instance Storable SDL_GPUViewport where
  sizeOf _ = #{size SDL_GPUViewport}
  alignment _ = #{alignment SDL_GPUViewport}
  poke ptr SDL_GPUViewport{..} = do
    pokeByteOff ptr #{offset SDL_GPUViewport, x} vpX
    pokeByteOff ptr #{offset SDL_GPUViewport, y} vpY
    pokeByteOff ptr #{offset SDL_GPUViewport, w} vpW
    pokeByteOff ptr #{offset SDL_GPUViewport, h} vpH
    pokeByteOff ptr #{offset SDL_GPUViewport, min_depth} vpMinDepth
    pokeByteOff ptr #{offset SDL_GPUViewport, max_depth} vpMaxDepth
  peek ptr = SDL_GPUViewport
    <$> peekByteOff ptr #{offset SDL_GPUViewport, x}
    <*> peekByteOff ptr #{offset SDL_GPUViewport, y}
    <*> peekByteOff ptr #{offset SDL_GPUViewport, w}
    <*> peekByteOff ptr #{offset SDL_GPUViewport, h}
    <*> peekByteOff ptr #{offset SDL_GPUViewport, min_depth}
    <*> peekByteOff ptr #{offset SDL_GPUViewport, max_depth}


type SDL_GPUBlendFactor = #{type SDL_GPUBlendFactor}
type SDL_GPUBlendOp = #{type SDL_GPUBlendOp}
type SDL_GPUColorComponentFlags = #{type SDL_GPUColorComponentFlags}

data SDL_GPUColorTargetBlendState = SDL_GPUColorTargetBlendState
  { blendSrcColor :: SDL_GPUBlendFactor
  , blendDstColor :: SDL_GPUBlendFactor
  , blendColorOp :: SDL_GPUBlendOp
  , blendSrcAlpha :: SDL_GPUBlendFactor
  , blendDstAlpha :: SDL_GPUBlendFactor
  , blendAlphaOp :: SDL_GPUBlendOp
  , blendColorWriteMask :: SDL_GPUColorComponentFlags
  , blendEnable :: CBool
  , blendEnableColorWriteMask :: CBool
  , blendPadding1 :: Word8
  , blendPadding2 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPUColorTargetBlendState where
  sizeOf _ = #{size SDL_GPUColorTargetBlendState}
  alignment _ = #{alignment SDL_GPUColorTargetBlendState}
  poke ptr SDL_GPUColorTargetBlendState{..} = do
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, src_color_blendfactor} blendSrcColor
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, dst_color_blendfactor} blendDstColor
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, color_blend_op} blendColorOp
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, src_alpha_blendfactor} blendSrcAlpha
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, dst_alpha_blendfactor} blendDstAlpha
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, alpha_blend_op} blendAlphaOp
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, color_write_mask} blendColorWriteMask
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, enable_blend} blendEnable
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, enable_color_write_mask} blendEnableColorWriteMask
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, padding1} blendPadding1
    pokeByteOff ptr #{offset SDL_GPUColorTargetBlendState, padding2} blendPadding2
  peek ptr = SDL_GPUColorTargetBlendState
    <$> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, src_color_blendfactor}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, dst_color_blendfactor}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, color_blend_op}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, src_alpha_blendfactor}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, dst_alpha_blendfactor}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, alpha_blend_op}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, color_write_mask}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, enable_blend}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, enable_color_write_mask}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, padding1}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetBlendState, padding2}


type SDL_GPUTextureFormat = #{type SDL_GPUTextureFormat}

type SDL_GPUTextureType = #{type SDL_GPUTextureType}
type SDL_GPUTextureUsageFlags = #{type SDL_GPUTextureUsageFlags}
type SDL_GPUFilter = #{type SDL_GPUFilter}
type SDL_GPUSamplerMipmapMode = #{type SDL_GPUSamplerMipmapMode}
type SDL_GPUSamplerAddressMode = #{type SDL_GPUSamplerAddressMode}
type SDL_GPULoadOp = #{type SDL_GPULoadOp}
type SDL_GPUStoreOp = #{type SDL_GPUStoreOp}

data SDL_GPUColorTargetDescription = SDL_GPUColorTargetDescription
  { colorTargetFormat :: SDL_GPUTextureFormat
  , colorTargetBlendState :: SDL_GPUColorTargetBlendState
  } deriving (Eq, Show)

instance Storable SDL_GPUColorTargetDescription where
  sizeOf _ = #{size SDL_GPUColorTargetDescription}
  alignment _ = #{alignment SDL_GPUColorTargetDescription}
  poke ptr SDL_GPUColorTargetDescription{..} = do
    pokeByteOff ptr #{offset SDL_GPUColorTargetDescription, format} colorTargetFormat
    pokeByteOff ptr #{offset SDL_GPUColorTargetDescription, blend_state} colorTargetBlendState
  peek ptr = SDL_GPUColorTargetDescription
    <$> peekByteOff ptr #{offset SDL_GPUColorTargetDescription, format}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetDescription, blend_state}


data SDL_GPUTextureCreateInfo = SDL_GPUTextureCreateInfo
  { texType :: SDL_GPUTextureType
  , texFormat :: SDL_GPUTextureFormat
  , texUsage :: SDL_GPUTextureUsageFlags
  , texWidth :: Word32
  , texHeight :: Word32
  , texLayerCountOrDepth :: Word32
  , texNumLevels :: Word32
  , texSampleCount :: SDL_GPUSampleCount
  , texProps :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPUTextureCreateInfo where
  sizeOf _ = #{size SDL_GPUTextureCreateInfo}
  alignment _ = #{alignment SDL_GPUTextureCreateInfo}
  poke ptr SDL_GPUTextureCreateInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, type} texType
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, format} texFormat
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, usage} texUsage
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, width} texWidth
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, height} texHeight
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, layer_count_or_depth} texLayerCountOrDepth
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, num_levels} texNumLevels
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, sample_count} texSampleCount
    pokeByteOff ptr #{offset SDL_GPUTextureCreateInfo, props} texProps
  peek ptr = SDL_GPUTextureCreateInfo
    <$> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, type}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, format}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, usage}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, width}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, height}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, layer_count_or_depth}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, num_levels}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, sample_count}
    <*> peekByteOff ptr #{offset SDL_GPUTextureCreateInfo, props}


data SDL_GPUSamplerCreateInfo = SDL_GPUSamplerCreateInfo
  { sampMinFilter :: SDL_GPUFilter
  , sampMagFilter :: SDL_GPUFilter
  , sampMipmapMode :: SDL_GPUSamplerMipmapMode
  , sampAddressModeU :: SDL_GPUSamplerAddressMode
  , sampAddressModeV :: SDL_GPUSamplerAddressMode
  , sampAddressModeW :: SDL_GPUSamplerAddressMode
  , sampMipLodBias :: CFloat
  , sampMaxAnisotropy :: CFloat
  , sampCompareOp :: SDL_GPUCompareOp
  , sampMinLod :: CFloat
  , sampMaxLod :: CFloat
  , sampEnableAnisotropy :: CBool
  , sampEnableCompare :: CBool
  , sampPadding1 :: Word8
  , sampPadding2 :: Word8
  , sampProps :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPUSamplerCreateInfo where
  sizeOf _ = #{size SDL_GPUSamplerCreateInfo}
  alignment _ = #{alignment SDL_GPUSamplerCreateInfo}
  poke ptr SDL_GPUSamplerCreateInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_filter} sampMinFilter
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mag_filter} sampMagFilter
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mipmap_mode} sampMipmapMode
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_u} sampAddressModeU
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_v} sampAddressModeV
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_w} sampAddressModeW
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mip_lod_bias} sampMipLodBias
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, max_anisotropy} sampMaxAnisotropy
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, compare_op} sampCompareOp
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_lod} sampMinLod
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, max_lod} sampMaxLod
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_anisotropy} sampEnableAnisotropy
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_compare} sampEnableCompare
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, padding1} sampPadding1
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, padding2} sampPadding2
    pokeByteOff ptr #{offset SDL_GPUSamplerCreateInfo, props} sampProps
  peek ptr = SDL_GPUSamplerCreateInfo
    <$> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_filter}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mag_filter}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mipmap_mode}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_u}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_v}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, address_mode_w}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, mip_lod_bias}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, max_anisotropy}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, compare_op}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, min_lod}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, max_lod}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_anisotropy}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, enable_compare}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, padding1}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, padding2}
    <*> peekByteOff ptr #{offset SDL_GPUSamplerCreateInfo, props}


data SDL_GPUTextureSamplerBinding = SDL_GPUTextureSamplerBinding
  { tsTexture :: Ptr SDL_GPUTexture
  , tsSampler :: Ptr SDL_GPUSampler
  } deriving (Eq, Show)

instance Storable SDL_GPUTextureSamplerBinding where
  sizeOf _ = #{size SDL_GPUTextureSamplerBinding}
  alignment _ = #{alignment SDL_GPUTextureSamplerBinding}
  poke ptr SDL_GPUTextureSamplerBinding{..} = do
    pokeByteOff ptr #{offset SDL_GPUTextureSamplerBinding, texture} tsTexture
    pokeByteOff ptr #{offset SDL_GPUTextureSamplerBinding, sampler} tsSampler
  peek ptr = SDL_GPUTextureSamplerBinding
    <$> peekByteOff ptr #{offset SDL_GPUTextureSamplerBinding, texture}
    <*> peekByteOff ptr #{offset SDL_GPUTextureSamplerBinding, sampler}


type SDL_GPUFillMode = #{type SDL_GPUFillMode}
type SDL_GPUCullMode = #{type SDL_GPUCullMode}
type SDL_GPUFrontFace = #{type SDL_GPUFrontFace}

data SDL_GPURasterizerState = SDL_GPURasterizerState
  { rastFillMode :: SDL_GPUFillMode
  , rastCullMode :: SDL_GPUCullMode
  , rastFrontFace :: SDL_GPUFrontFace
  , rastDepthBiasConstant :: CFloat
  , rastDepthBiasClamp :: CFloat
  , rastDepthBiasSlope :: CFloat
  , rastEnableDepthBias :: CBool
  , rastEnableDepthClip :: CBool
  , rastPadding1 :: Word8
  , rastPadding2 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPURasterizerState where
  sizeOf _ = #{size SDL_GPURasterizerState}
  alignment _ = #{alignment SDL_GPURasterizerState}
  poke ptr SDL_GPURasterizerState{..} = do
    pokeByteOff ptr #{offset SDL_GPURasterizerState, fill_mode} rastFillMode
    pokeByteOff ptr #{offset SDL_GPURasterizerState, cull_mode} rastCullMode
    pokeByteOff ptr #{offset SDL_GPURasterizerState, front_face} rastFrontFace
    pokeByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_constant_factor} rastDepthBiasConstant
    pokeByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_clamp} rastDepthBiasClamp
    pokeByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_slope_factor} rastDepthBiasSlope
    pokeByteOff ptr #{offset SDL_GPURasterizerState, enable_depth_bias} rastEnableDepthBias
    pokeByteOff ptr #{offset SDL_GPURasterizerState, enable_depth_clip} rastEnableDepthClip
    pokeByteOff ptr #{offset SDL_GPURasterizerState, padding1} rastPadding1
    pokeByteOff ptr #{offset SDL_GPURasterizerState, padding2} rastPadding2
  peek ptr = SDL_GPURasterizerState
    <$> peekByteOff ptr #{offset SDL_GPURasterizerState, fill_mode}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, cull_mode}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, front_face}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_constant_factor}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_clamp}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, depth_bias_slope_factor}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, enable_depth_bias}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, enable_depth_clip}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, padding1}
    <*> peekByteOff ptr #{offset SDL_GPURasterizerState, padding2}


type SDL_GPUSampleCount = #{type SDL_GPUSampleCount}

data SDL_GPUMultisampleState = SDL_GPUMultisampleState
  { msSampleCount :: SDL_GPUSampleCount
  , msSampleMask :: Word32
  , msEnableMask :: CBool
  , msEnableAlphaToCoverage :: CBool
  , msPadding2 :: Word8
  , msPadding3 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPUMultisampleState where
  sizeOf _ = #{size SDL_GPUMultisampleState}
  alignment _ = #{alignment SDL_GPUMultisampleState}
  poke ptr SDL_GPUMultisampleState{..} = do
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, sample_count} msSampleCount
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, sample_mask} msSampleMask
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, enable_mask} msEnableMask
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, enable_alpha_to_coverage} msEnableAlphaToCoverage
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, padding2} msPadding2
    pokeByteOff ptr #{offset SDL_GPUMultisampleState, padding3} msPadding3
  peek ptr = SDL_GPUMultisampleState
    <$> peekByteOff ptr #{offset SDL_GPUMultisampleState, sample_count}
    <*> peekByteOff ptr #{offset SDL_GPUMultisampleState, sample_mask}
    <*> peekByteOff ptr #{offset SDL_GPUMultisampleState, enable_mask}
    <*> peekByteOff ptr #{offset SDL_GPUMultisampleState, enable_alpha_to_coverage}
    <*> peekByteOff ptr #{offset SDL_GPUMultisampleState, padding2}
    <*> peekByteOff ptr #{offset SDL_GPUMultisampleState, padding3}


type SDL_GPUCompareOp = #{type SDL_GPUCompareOp}
type SDL_GPUStencilOp = #{type SDL_GPUStencilOp}

data SDL_GPUStencilOpState = SDL_GPUStencilOpState
  { stencilFailOp :: SDL_GPUStencilOp
  , stencilPassOp :: SDL_GPUStencilOp
  , stencilDepthFailOp :: SDL_GPUStencilOp
  , stencilCompareOp :: SDL_GPUCompareOp
  } deriving (Eq, Show)

instance Storable SDL_GPUStencilOpState where
  sizeOf _ = #{size SDL_GPUStencilOpState}
  alignment _ = #{alignment SDL_GPUStencilOpState}
  poke ptr SDL_GPUStencilOpState{..} = do
    pokeByteOff ptr #{offset SDL_GPUStencilOpState, fail_op} stencilFailOp
    pokeByteOff ptr #{offset SDL_GPUStencilOpState, pass_op} stencilPassOp
    pokeByteOff ptr #{offset SDL_GPUStencilOpState, depth_fail_op} stencilDepthFailOp
    pokeByteOff ptr #{offset SDL_GPUStencilOpState, compare_op} stencilCompareOp
  peek ptr = SDL_GPUStencilOpState
    <$> peekByteOff ptr #{offset SDL_GPUStencilOpState, fail_op}
    <*> peekByteOff ptr #{offset SDL_GPUStencilOpState, pass_op}
    <*> peekByteOff ptr #{offset SDL_GPUStencilOpState, depth_fail_op}
    <*> peekByteOff ptr #{offset SDL_GPUStencilOpState, compare_op}


data SDL_GPUDepthStencilState = SDL_GPUDepthStencilState
  { dsCompareOp :: SDL_GPUCompareOp
  , dsBackStencil :: SDL_GPUStencilOpState
  , dsFrontStencil :: SDL_GPUStencilOpState
  , dsCompareMask :: Word8
  , dsWriteMask :: Word8
  , dsEnableDepthTest :: CBool
  , dsEnableDepthWrite :: CBool
  , dsEnableStencilTest :: CBool
  , dsPadding1 :: Word8
  , dsPadding2 :: Word8
  , dsPadding3 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPUDepthStencilState where
  sizeOf _ = #{size SDL_GPUDepthStencilState}
  alignment _ = #{alignment SDL_GPUDepthStencilState}
  poke ptr SDL_GPUDepthStencilState{..} = do
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, compare_op} dsCompareOp
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, back_stencil_state} dsBackStencil
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, front_stencil_state} dsFrontStencil
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, compare_mask} dsCompareMask
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, write_mask} dsWriteMask
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, enable_depth_test} dsEnableDepthTest
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, enable_depth_write} dsEnableDepthWrite
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, enable_stencil_test} dsEnableStencilTest
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, padding1} dsPadding1
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, padding2} dsPadding2
    pokeByteOff ptr #{offset SDL_GPUDepthStencilState, padding3} dsPadding3
  peek ptr = SDL_GPUDepthStencilState
    <$> peekByteOff ptr #{offset SDL_GPUDepthStencilState, compare_op}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, back_stencil_state}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, front_stencil_state}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, compare_mask}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, write_mask}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, enable_depth_test}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, enable_depth_write}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, enable_stencil_test}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, padding1}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, padding2}
    <*> peekByteOff ptr #{offset SDL_GPUDepthStencilState, padding3}


data SDL_GPUGraphicsPipelineTargetInfo = SDL_GPUGraphicsPipelineTargetInfo
  { targetColorDescriptions :: Ptr SDL_GPUColorTargetDescription
  , targetNumColorTargets :: Word32
  , targetDepthStencilFormat :: SDL_GPUTextureFormat
  , targetHasDepthStencil :: CBool
  , targetPadding1 :: Word8
  , targetPadding2 :: Word8
  , targetPadding3 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPUGraphicsPipelineTargetInfo where
  sizeOf _ = #{size SDL_GPUGraphicsPipelineTargetInfo}
  alignment _ = #{alignment SDL_GPUGraphicsPipelineTargetInfo}
  poke ptr SDL_GPUGraphicsPipelineTargetInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, color_target_descriptions} targetColorDescriptions
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, num_color_targets} targetNumColorTargets
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, depth_stencil_format} targetDepthStencilFormat
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, has_depth_stencil_target} targetHasDepthStencil
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding1} targetPadding1
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding2} targetPadding2
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding3} targetPadding3
  peek ptr = SDL_GPUGraphicsPipelineTargetInfo
    <$> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, color_target_descriptions}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, num_color_targets}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, depth_stencil_format}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, has_depth_stencil_target}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding1}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding2}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineTargetInfo, padding3}


data SDL_GPUVertexInputState = SDL_GPUVertexInputState
  { viVertexBufferDescriptions :: Ptr SDL_GPUVertexBufferDescription
  , viNumVertexBuffers :: Word32
  , viVertexAttributes :: Ptr SDL_GPUVertexAttribute
  , viNumVertexAttributes :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPUVertexInputState where
  sizeOf _ = #{size SDL_GPUVertexInputState}
  alignment _ = #{alignment SDL_GPUVertexInputState}
  poke ptr SDL_GPUVertexInputState{..} = do
    pokeByteOff ptr #{offset SDL_GPUVertexInputState, vertex_buffer_descriptions} viVertexBufferDescriptions
    pokeByteOff ptr #{offset SDL_GPUVertexInputState, num_vertex_buffers} viNumVertexBuffers
    pokeByteOff ptr #{offset SDL_GPUVertexInputState, vertex_attributes} viVertexAttributes
    pokeByteOff ptr #{offset SDL_GPUVertexInputState, num_vertex_attributes} viNumVertexAttributes
  peek ptr = SDL_GPUVertexInputState
    <$> peekByteOff ptr #{offset SDL_GPUVertexInputState, vertex_buffer_descriptions}
    <*> peekByteOff ptr #{offset SDL_GPUVertexInputState, num_vertex_buffers}
    <*> peekByteOff ptr #{offset SDL_GPUVertexInputState, vertex_attributes}
    <*> peekByteOff ptr #{offset SDL_GPUVertexInputState, num_vertex_attributes}


type SDL_GPUPrimitiveType = #{type SDL_GPUPrimitiveType}

data SDL_GPUGraphicsPipelineCreateInfo = SDL_GPUGraphicsPipelineCreateInfo
  { gpVertexShader :: Ptr SDL_GPUShader
  , gpFragmentShader :: Ptr SDL_GPUShader
  , gpVertexInputState :: SDL_GPUVertexInputState
  , gpPrimitiveType :: SDL_GPUPrimitiveType
  , gpRasterizerState :: SDL_GPURasterizerState
  , gpMultisampleState :: SDL_GPUMultisampleState
  , gpDepthStencilState :: SDL_GPUDepthStencilState
  , gpTargetInfo :: SDL_GPUGraphicsPipelineTargetInfo
  , gpProps :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPUGraphicsPipelineCreateInfo where
  sizeOf _ = #{size SDL_GPUGraphicsPipelineCreateInfo}
  alignment _ = #{alignment SDL_GPUGraphicsPipelineCreateInfo}
  poke ptr SDL_GPUGraphicsPipelineCreateInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, vertex_shader} gpVertexShader
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, fragment_shader} gpFragmentShader
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, vertex_input_state} gpVertexInputState
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, primitive_type} gpPrimitiveType
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state} gpRasterizerState
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, multisample_state} gpMultisampleState
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, depth_stencil_state} gpDepthStencilState
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, target_info} gpTargetInfo
    pokeByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, props} gpProps
  peek ptr = SDL_GPUGraphicsPipelineCreateInfo
    <$> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, vertex_shader}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, fragment_shader}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, vertex_input_state}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, primitive_type}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, rasterizer_state}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, multisample_state}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, depth_stencil_state}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, target_info}
    <*> peekByteOff ptr #{offset SDL_GPUGraphicsPipelineCreateInfo, props}


type SDL_GPUShaderFormat = #{type SDL_GPUShaderFormat}
type SDL_GPUShaderStage = #{type SDL_GPUShaderStage}

data SDL_GPUShaderCreateInfo = SDL_GPUShaderCreateInfo
  { shaderCodeSize :: CSize
  , shaderCode :: Ptr Word8
  , shaderEntrypoint :: CString
  , shaderFormat :: SDL_GPUShaderFormat
  , shaderStage :: SDL_GPUShaderStage
  , shaderNumSamplers :: Word32
  , shaderNumStorageTextures :: Word32
  , shaderNumStorageBuffers :: Word32
  , shaderNumUniformBuffers :: Word32
  , shaderProps :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPUShaderCreateInfo where
  sizeOf _ = #{size SDL_GPUShaderCreateInfo}
  alignment _ = #{alignment SDL_GPUShaderCreateInfo}
  poke ptr SDL_GPUShaderCreateInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, code_size} shaderCodeSize
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, code} shaderCode
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, entrypoint} shaderEntrypoint
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, format} shaderFormat
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, stage} shaderStage
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_samplers} shaderNumSamplers
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_textures} shaderNumStorageTextures
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_buffers} shaderNumStorageBuffers
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_uniform_buffers} shaderNumUniformBuffers
    pokeByteOff ptr #{offset SDL_GPUShaderCreateInfo, props} shaderProps
  peek ptr = SDL_GPUShaderCreateInfo
    <$> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, code_size}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, code}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, entrypoint}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, format}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, stage}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_samplers}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_textures}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_storage_buffers}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, num_uniform_buffers}
    <*> peekByteOff ptr #{offset SDL_GPUShaderCreateInfo, props}

data SDL_GPURenderStateCreateInfo = SDL_GPURenderStateCreateInfo
  { rsFragmentShader :: Ptr SDL_GPUShader
  , rsNumSamplerBindings :: CInt
  , rsSamplerBindings :: Ptr ()
  , rsNumStorageTextures :: CInt
  , rsStorageTextures :: Ptr ()
  , rsNumStorageBuffers :: CInt
  , rsStorageBuffers :: Ptr ()
  , rsProps :: Word32
  } deriving (Eq, Show)

instance Storable SDL_GPURenderStateCreateInfo where
  sizeOf _ = #{size SDL_GPURenderStateCreateInfo}
  alignment _ = #{alignment SDL_GPURenderStateCreateInfo}
  poke ptr SDL_GPURenderStateCreateInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, fragment_shader} rsFragmentShader
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_sampler_bindings} rsNumSamplerBindings
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, sampler_bindings} rsSamplerBindings
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_storage_textures} rsNumStorageTextures
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, storage_textures} rsStorageTextures
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_storage_buffers} rsNumStorageBuffers
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, storage_buffers} rsStorageBuffers
    pokeByteOff ptr #{offset SDL_GPURenderStateCreateInfo, props} rsProps
  peek ptr = SDL_GPURenderStateCreateInfo
    <$> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, fragment_shader}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_sampler_bindings}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, sampler_bindings}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_storage_textures}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, storage_textures}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, num_storage_buffers}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, storage_buffers}
    <*> peekByteOff ptr #{offset SDL_GPURenderStateCreateInfo, props}


data SDL_GPUColorTargetInfo = SDL_GPUColorTargetInfo
  { ctTexture :: Ptr SDL_GPUTexture
  , ctMipLevel :: Word32
  , ctLayerOrDepth :: Word32
  , ctClearColor :: SDL_FColor
  , ctLoadOp :: SDL_GPULoadOp
  , ctStoreOp :: SDL_GPUStoreOp
  , ctResolveTexture :: Ptr SDL_GPUTexture
  , ctResolveMip :: Word32
  , ctResolveLayer :: Word32
  , ctCycle :: CBool
  , ctCycleResolve :: CBool
  , ctPadding1 :: Word8
  , ctPadding2 :: Word8
  } deriving (Eq, Show)

instance Storable SDL_GPUColorTargetInfo where
  sizeOf _ = #{size SDL_GPUColorTargetInfo}
  alignment _ = #{alignment SDL_GPUColorTargetInfo}
  poke ptr SDL_GPUColorTargetInfo{..} = do
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, texture} ctTexture
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, mip_level} ctMipLevel
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, layer_or_depth_plane} ctLayerOrDepth
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, clear_color} ctClearColor
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, load_op} ctLoadOp
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, store_op} ctStoreOp
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_texture} ctResolveTexture
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_mip_level} ctResolveMip
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_layer} ctResolveLayer
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, cycle} ctCycle
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, cycle_resolve_texture} ctCycleResolve
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, padding1} ctPadding1
    pokeByteOff ptr #{offset SDL_GPUColorTargetInfo, padding2} ctPadding2
  peek ptr = SDL_GPUColorTargetInfo
    <$> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, texture}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, mip_level}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, layer_or_depth_plane}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, clear_color}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, load_op}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, store_op}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_texture}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_mip_level}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, resolve_layer}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, cycle}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, cycle_resolve_texture}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, padding1}
    <*> peekByteOff ptr #{offset SDL_GPUColorTargetInfo, padding2}

-- FFI functions

foreign import ccall "SDL_Init" sdlInit :: Word32 -> IO CBool
foreign import ccall "SDL_Quit" sdlQuit :: IO ()
foreign import ccall "SDL_CreateWindow" sdlCreateWindow :: CString -> CInt -> CInt -> Word32 -> IO (Ptr SDL_Window)
foreign import ccall "SDL_DestroyWindow" sdlDestroyWindow :: Ptr SDL_Window -> IO ()
foreign import ccall "SDL_GetError" sdlGetError :: IO CString
foreign import ccall "SDL_PollEvent" sdlPollEvent :: Ptr () -> IO CBool
foreign import ccall "SDL_Delay" sdlDelay :: Word32 -> IO ()

foreign import ccall "SDL_CreateGPURenderer" sdlCreateGPURenderer :: Ptr SDL_GPUDevice -> Ptr SDL_Window -> IO (Ptr SDL_Renderer)
foreign import ccall "SDL_DestroyRenderer" sdlDestroyRenderer :: Ptr SDL_Renderer -> IO ()
foreign import ccall "SDL_GetGPURendererDevice" sdlGetGPURendererDevice :: Ptr SDL_Renderer -> IO (Ptr SDL_GPUDevice)
foreign import ccall "SDL_CreateGPURenderState" sdlCreateGPURenderState :: Ptr SDL_Renderer -> Ptr SDL_GPURenderStateCreateInfo -> IO (Ptr SDL_GPURenderState)
foreign import ccall "SDL_SetGPURenderStateFragmentUniforms" sdlSetGPURenderStateFragmentUniforms :: Ptr SDL_GPURenderState -> Word32 -> Ptr () -> Word32 -> IO CBool
foreign import ccall "SDL_SetGPURenderState" sdlSetGPURenderState :: Ptr SDL_Renderer -> Ptr SDL_GPURenderState -> IO CBool
foreign import ccall "SDL_DestroyGPURenderState" sdlDestroyGPURenderState :: Ptr SDL_GPURenderState -> IO ()
foreign import ccall "SDL_SetRenderDrawColor" sdlSetRenderDrawColor :: Ptr SDL_Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
foreign import ccall "SDL_RenderClear" sdlRenderClear :: Ptr SDL_Renderer -> IO CBool
foreign import ccall "SDL_RenderPresent" sdlRenderPresent :: Ptr SDL_Renderer -> IO CBool
foreign import ccall "SDL_RenderGeometryRaw" sdlRenderGeometryRaw :: Ptr SDL_Renderer -> Ptr SDL_Texture -> Ptr CFloat -> CInt -> Ptr SDL_FColor -> CInt -> Ptr CFloat -> CInt -> CInt -> Ptr () -> CInt -> CInt -> IO CBool

foreign import ccall "SDL_CreateGPUDevice" sdlCreateGPUDevice :: SDL_GPUShaderFormat -> CBool -> CString -> IO (Ptr SDL_GPUDevice)
foreign import ccall "SDL_DestroyGPUDevice" sdlDestroyGPUDevice :: Ptr SDL_GPUDevice -> IO ()
foreign import ccall "SDL_GetGPUShaderFormats" sdlGetGPUShaderFormats :: Ptr SDL_GPUDevice -> IO SDL_GPUShaderFormat
foreign import ccall "SDL_ClaimWindowForGPUDevice" sdlClaimWindowForGPUDevice :: Ptr SDL_GPUDevice -> Ptr SDL_Window -> IO CBool
foreign import ccall "SDL_ReleaseWindowFromGPUDevice" sdlReleaseWindowFromGPUDevice :: Ptr SDL_GPUDevice -> Ptr SDL_Window -> IO ()
foreign import ccall "SDL_GetGPUSwapchainTextureFormat" sdlGetGPUSwapchainTextureFormat :: Ptr SDL_GPUDevice -> Ptr SDL_Window -> IO SDL_GPUTextureFormat

foreign import ccall "SDL_CreateGPUShader" sdlCreateGPUShader :: Ptr SDL_GPUDevice -> Ptr SDL_GPUShaderCreateInfo -> IO (Ptr SDL_GPUShader)
foreign import ccall "SDL_ReleaseGPUShader" sdlReleaseGPUShader :: Ptr SDL_GPUDevice -> Ptr SDL_GPUShader -> IO ()
foreign import ccall "SDL_CreateGPUTexture" sdlCreateGPUTexture :: Ptr SDL_GPUDevice -> Ptr SDL_GPUTextureCreateInfo -> IO (Ptr SDL_GPUTexture)
foreign import ccall "SDL_ReleaseGPUTexture" sdlReleaseGPUTexture :: Ptr SDL_GPUDevice -> Ptr SDL_GPUTexture -> IO ()
foreign import ccall "SDL_CreateGPUSampler" sdlCreateGPUSampler :: Ptr SDL_GPUDevice -> Ptr SDL_GPUSamplerCreateInfo -> IO (Ptr SDL_GPUSampler)
foreign import ccall "SDL_ReleaseGPUSampler" sdlReleaseGPUSampler :: Ptr SDL_GPUDevice -> Ptr SDL_GPUSampler -> IO ()
foreign import ccall "SDL_CreateGPUGraphicsPipeline" sdlCreateGPUGraphicsPipeline :: Ptr SDL_GPUDevice -> Ptr SDL_GPUGraphicsPipelineCreateInfo -> IO (Ptr SDL_GPUGraphicsPipeline)
foreign import ccall "SDL_ReleaseGPUGraphicsPipeline" sdlReleaseGPUGraphicsPipeline :: Ptr SDL_GPUDevice -> Ptr SDL_GPUGraphicsPipeline -> IO ()

foreign import ccall "SDL_AcquireGPUCommandBuffer" sdlAcquireGPUCommandBuffer :: Ptr SDL_GPUDevice -> IO (Ptr SDL_GPUCommandBuffer)
foreign import ccall "SDL_WaitAndAcquireGPUSwapchainTexture" sdlWaitAndAcquireGPUSwapchainTexture :: Ptr SDL_GPUCommandBuffer -> Ptr SDL_Window -> Ptr (Ptr SDL_GPUTexture) -> Ptr Word32 -> Ptr Word32 -> IO CBool
foreign import ccall "SDL_BeginGPURenderPass" sdlBeginGPURenderPass :: Ptr SDL_GPUCommandBuffer -> Ptr SDL_GPUColorTargetInfo -> Word32 -> Ptr () -> IO (Ptr SDL_GPURenderPass)
foreign import ccall "SDL_BindGPUGraphicsPipeline" sdlBindGPUGraphicsPipeline :: Ptr SDL_GPURenderPass -> Ptr SDL_GPUGraphicsPipeline -> IO ()
foreign import ccall "SDL_BindGPUFragmentSamplers" sdlBindGPUFragmentSamplers :: Ptr SDL_GPURenderPass -> Word32 -> Ptr SDL_GPUTextureSamplerBinding -> Word32 -> IO ()
foreign import ccall "SDL_SetGPUViewport" sdlSetGPUViewport :: Ptr SDL_GPURenderPass -> Ptr SDL_GPUViewport -> IO ()
foreign import ccall "SDL_PushGPUFragmentUniformData" sdlPushGPUFragmentUniformData :: Ptr SDL_GPUCommandBuffer -> Word32 -> Ptr () -> Word32 -> IO ()
foreign import ccall "SDL_DrawGPUPrimitives" sdlDrawGPUPrimitives :: Ptr SDL_GPURenderPass -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
foreign import ccall "SDL_EndGPURenderPass" sdlEndGPURenderPass :: Ptr SDL_GPURenderPass -> IO ()
foreign import ccall "SDL_SubmitGPUCommandBuffer" sdlSubmitGPUCommandBuffer :: Ptr SDL_GPUCommandBuffer -> IO CBool

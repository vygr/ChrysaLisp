/* sdl3_load.h - SDL3 loaded when the host starts, not linked, include after SDL3/SDL.h */

// A host on SDL3 runs on a machine with no SDL3, with no GUI and no audio.
// Every call the drivers make is a pointer, of the type SDL's header gives
// the call, found by host_sdl3_load() in sdl3_load.cpp, and the name of the
// call is made to mean the pointer. Nothing links SDL3, so a call missing
// from this list is a link error.

#ifndef SDL3_LOAD_H
#define SDL3_LOAD_H

#define SDL3_CALLS(X) \
	X(SDL_AcquireGPUCommandBuffer) \
	X(SDL_AddAtomicInt) \
	X(SDL_BeginGPUComputePass) \
	X(SDL_BeginGPUCopyPass) \
	X(SDL_BeginGPURenderPass) \
	X(SDL_BindGPUComputePipeline) \
	X(SDL_BindGPUComputeStorageBuffers) \
	X(SDL_BindGPUFragmentSamplers) \
	X(SDL_BindGPUGraphicsPipeline) \
	X(SDL_BindGPUVertexBuffers) \
	X(SDL_CompareAndSwapAtomicInt) \
	X(SDL_ComposeCustomBlendMode) \
	X(SDL_ConvertAudioSamples) \
	X(SDL_ConvertPixels) \
	X(SDL_CreateGPUBuffer) \
	X(SDL_CreateGPUComputePipeline) \
	X(SDL_CreateGPUDeviceWithProperties) \
	X(SDL_CreateGPUGraphicsPipeline) \
	X(SDL_CreateGPURenderer) \
	X(SDL_CreateGPUSampler) \
	X(SDL_CreateGPUShader) \
	X(SDL_CreateGPUTexture) \
	X(SDL_CreateGPUTransferBuffer) \
	X(SDL_CreateProperties) \
	X(SDL_CreateRenderer) \
	X(SDL_CreateSurfaceFrom) \
	X(SDL_CreateTexture) \
	X(SDL_CreateTextureFromSurface) \
	X(SDL_CreateThreadWithPropertiesRuntime) \
	X(SDL_CreateWindow) \
	X(SDL_Delay) \
	X(SDL_DestroyAudioStream) \
	X(SDL_DestroyGPUDevice) \
	X(SDL_DestroyProperties) \
	X(SDL_DestroyRenderer) \
	X(SDL_DestroySurface) \
	X(SDL_DestroyTexture) \
	X(SDL_DestroyWindow) \
	X(SDL_DetachThread) \
	X(SDL_DispatchGPUCompute) \
	X(SDL_DownloadFromGPUBuffer) \
	X(SDL_DrawGPUPrimitives) \
	X(SDL_EndGPUComputePass) \
	X(SDL_EndGPUCopyPass) \
	X(SDL_EndGPURenderPass) \
	X(SDL_FlushRenderer) \
	X(SDL_GPUTextureSupportsFormat) \
	X(SDL_GPUTextureSupportsSampleCount) \
	X(SDL_GetAtomicInt) \
	X(SDL_GetAudioStreamData) \
	X(SDL_GetClipboardText) \
	X(SDL_GetCurrentVideoDriver) \
	X(SDL_GetDesktopDisplayMode) \
	X(SDL_GetError) \
	X(SDL_GetGPUShaderFormats) \
	X(SDL_GetKeyboardFocus) \
	X(SDL_GetPointerProperty) \
	X(SDL_GetPrimaryDisplay) \
	X(SDL_GetRenderTarget) \
	X(SDL_GetTextureBlendMode) \
	X(SDL_GetTextureColorMod) \
	X(SDL_GetTextureProperties) \
	X(SDL_GetTextureSize) \
	X(SDL_GetTicks) \
	X(SDL_GetTicksNS) \
	X(SDL_GetTouchDeviceType) \
	X(SDL_GetWindowFromID) \
	X(SDL_GetWindowProperties) \
	X(SDL_GetWindowSize) \
	X(SDL_HideCursor) \
	X(SDL_Init) \
	X(SDL_InitSubSystem) \
	X(SDL_LoadWAV) \
	X(SDL_LockAudioStream) \
	X(SDL_Log) \
	X(SDL_MapGPUTransferBuffer) \
	X(SDL_OpenAudioDeviceStream) \
	X(SDL_PollEvent) \
	X(SDL_PumpEvents) \
	X(SDL_PushGPUComputeUniformData) \
	X(SDL_PushGPUFragmentUniformData) \
	X(SDL_PushGPUVertexUniformData) \
	X(SDL_PutAudioStreamData) \
	X(SDL_QueryGPUFence) \
	X(SDL_QuitSubSystem) \
	X(SDL_RaiseWindow) \
	X(SDL_ReleaseGPUBuffer) \
	X(SDL_ReleaseGPUComputePipeline) \
	X(SDL_ReleaseGPUFence) \
	X(SDL_ReleaseGPUGraphicsPipeline) \
	X(SDL_ReleaseGPUSampler) \
	X(SDL_ReleaseGPUShader) \
	X(SDL_ReleaseGPUTexture) \
	X(SDL_ReleaseGPUTransferBuffer) \
	X(SDL_RenderFillRect) \
	X(SDL_RenderPresent) \
	X(SDL_RenderReadPixels) \
	X(SDL_RenderRect) \
	X(SDL_RenderTexture) \
	X(SDL_ResumeAudioStreamDevice) \
	X(SDL_SetAssertionHandler) \
	X(SDL_SetAtomicInt) \
	X(SDL_SetAudioStreamGain) \
	X(SDL_SetBooleanProperty) \
	X(SDL_SetClipboardText) \
	X(SDL_SetGPUScissor) \
	X(SDL_SetHint) \
	X(SDL_SetNumberProperty) \
	X(SDL_SetPointerProperty) \
	X(SDL_SetRenderClipRect) \
	X(SDL_SetRenderDrawBlendMode) \
	X(SDL_SetRenderDrawColor) \
	X(SDL_SetRenderTarget) \
	X(SDL_SetRenderVSync) \
	X(SDL_SetStringProperty) \
	X(SDL_SetTextureBlendMode) \
	X(SDL_SetTextureColorMod) \
	X(SDL_SetTextureScaleMode) \
	X(SDL_SetWindowFullscreen) \
	X(SDL_ShowCursor) \
	X(SDL_SubmitGPUCommandBuffer) \
	X(SDL_SubmitGPUCommandBufferAndAcquireFence) \
	X(SDL_UnlockAudioStream) \
	X(SDL_UnmapGPUTransferBuffer) \
	X(SDL_UploadToGPUBuffer) \
	X(SDL_WaitForGPUFences) \
	X(SDL_WasInit) \
	X(SDL_free) \
	X(SDL_getenv) \
	X(SDL_malloc) \
	X(SDL_strcmp)

#define SDL3_DECLARE(name) extern decltype(&name) cl_##name;
SDL3_CALLS(SDL3_DECLARE)
#undef SDL3_DECLARE

#define SDL_AcquireGPUCommandBuffer cl_SDL_AcquireGPUCommandBuffer
#define SDL_AddAtomicInt cl_SDL_AddAtomicInt
#define SDL_BeginGPUComputePass cl_SDL_BeginGPUComputePass
#define SDL_BeginGPUCopyPass cl_SDL_BeginGPUCopyPass
#define SDL_BeginGPURenderPass cl_SDL_BeginGPURenderPass
#define SDL_BindGPUComputePipeline cl_SDL_BindGPUComputePipeline
#define SDL_BindGPUComputeStorageBuffers cl_SDL_BindGPUComputeStorageBuffers
#define SDL_BindGPUFragmentSamplers cl_SDL_BindGPUFragmentSamplers
#define SDL_BindGPUGraphicsPipeline cl_SDL_BindGPUGraphicsPipeline
#define SDL_BindGPUVertexBuffers cl_SDL_BindGPUVertexBuffers
#define SDL_CompareAndSwapAtomicInt cl_SDL_CompareAndSwapAtomicInt
#define SDL_ComposeCustomBlendMode cl_SDL_ComposeCustomBlendMode
#define SDL_ConvertAudioSamples cl_SDL_ConvertAudioSamples
#define SDL_ConvertPixels cl_SDL_ConvertPixels
#define SDL_CreateGPUBuffer cl_SDL_CreateGPUBuffer
#define SDL_CreateGPUComputePipeline cl_SDL_CreateGPUComputePipeline
#define SDL_CreateGPUDeviceWithProperties cl_SDL_CreateGPUDeviceWithProperties
#define SDL_CreateGPUGraphicsPipeline cl_SDL_CreateGPUGraphicsPipeline
#define SDL_CreateGPURenderer cl_SDL_CreateGPURenderer
#define SDL_CreateGPUSampler cl_SDL_CreateGPUSampler
#define SDL_CreateGPUShader cl_SDL_CreateGPUShader
#define SDL_CreateGPUTexture cl_SDL_CreateGPUTexture
#define SDL_CreateGPUTransferBuffer cl_SDL_CreateGPUTransferBuffer
#define SDL_CreateProperties cl_SDL_CreateProperties
#define SDL_CreateRenderer cl_SDL_CreateRenderer
#define SDL_CreateSurfaceFrom cl_SDL_CreateSurfaceFrom
#define SDL_CreateTexture cl_SDL_CreateTexture
#define SDL_CreateTextureFromSurface cl_SDL_CreateTextureFromSurface
#define SDL_CreateThreadWithPropertiesRuntime cl_SDL_CreateThreadWithPropertiesRuntime
#define SDL_CreateWindow cl_SDL_CreateWindow
#define SDL_Delay cl_SDL_Delay
#define SDL_DestroyAudioStream cl_SDL_DestroyAudioStream
#define SDL_DestroyGPUDevice cl_SDL_DestroyGPUDevice
#define SDL_DestroyProperties cl_SDL_DestroyProperties
#define SDL_DestroyRenderer cl_SDL_DestroyRenderer
#define SDL_DestroySurface cl_SDL_DestroySurface
#define SDL_DestroyTexture cl_SDL_DestroyTexture
#define SDL_DestroyWindow cl_SDL_DestroyWindow
#define SDL_DetachThread cl_SDL_DetachThread
#define SDL_DispatchGPUCompute cl_SDL_DispatchGPUCompute
#define SDL_DownloadFromGPUBuffer cl_SDL_DownloadFromGPUBuffer
#define SDL_DrawGPUPrimitives cl_SDL_DrawGPUPrimitives
#define SDL_EndGPUComputePass cl_SDL_EndGPUComputePass
#define SDL_EndGPUCopyPass cl_SDL_EndGPUCopyPass
#define SDL_EndGPURenderPass cl_SDL_EndGPURenderPass
#define SDL_FlushRenderer cl_SDL_FlushRenderer
#define SDL_GPUTextureSupportsFormat cl_SDL_GPUTextureSupportsFormat
#define SDL_GPUTextureSupportsSampleCount cl_SDL_GPUTextureSupportsSampleCount
#define SDL_GetAtomicInt cl_SDL_GetAtomicInt
#define SDL_GetAudioStreamData cl_SDL_GetAudioStreamData
#define SDL_GetClipboardText cl_SDL_GetClipboardText
#define SDL_GetCurrentVideoDriver cl_SDL_GetCurrentVideoDriver
#define SDL_GetDesktopDisplayMode cl_SDL_GetDesktopDisplayMode
#define SDL_GetError cl_SDL_GetError
#define SDL_GetGPUShaderFormats cl_SDL_GetGPUShaderFormats
#define SDL_GetKeyboardFocus cl_SDL_GetKeyboardFocus
#define SDL_GetPointerProperty cl_SDL_GetPointerProperty
#define SDL_GetPrimaryDisplay cl_SDL_GetPrimaryDisplay
#define SDL_GetRenderTarget cl_SDL_GetRenderTarget
#define SDL_GetTextureBlendMode cl_SDL_GetTextureBlendMode
#define SDL_GetTextureColorMod cl_SDL_GetTextureColorMod
#define SDL_GetTextureProperties cl_SDL_GetTextureProperties
#define SDL_GetTextureSize cl_SDL_GetTextureSize
#define SDL_GetTicks cl_SDL_GetTicks
#define SDL_GetTicksNS cl_SDL_GetTicksNS
#define SDL_GetTouchDeviceType cl_SDL_GetTouchDeviceType
#define SDL_GetWindowFromID cl_SDL_GetWindowFromID
#define SDL_GetWindowProperties cl_SDL_GetWindowProperties
#define SDL_GetWindowSize cl_SDL_GetWindowSize
#define SDL_HideCursor cl_SDL_HideCursor
#define SDL_Init cl_SDL_Init
#define SDL_InitSubSystem cl_SDL_InitSubSystem
#define SDL_LoadWAV cl_SDL_LoadWAV
#define SDL_LockAudioStream cl_SDL_LockAudioStream
#define SDL_Log cl_SDL_Log
#define SDL_MapGPUTransferBuffer cl_SDL_MapGPUTransferBuffer
#define SDL_OpenAudioDeviceStream cl_SDL_OpenAudioDeviceStream
#define SDL_PollEvent cl_SDL_PollEvent
#define SDL_PumpEvents cl_SDL_PumpEvents
#define SDL_PushGPUComputeUniformData cl_SDL_PushGPUComputeUniformData
#define SDL_PushGPUFragmentUniformData cl_SDL_PushGPUFragmentUniformData
#define SDL_PushGPUVertexUniformData cl_SDL_PushGPUVertexUniformData
#define SDL_PutAudioStreamData cl_SDL_PutAudioStreamData
#define SDL_QueryGPUFence cl_SDL_QueryGPUFence
#define SDL_QuitSubSystem cl_SDL_QuitSubSystem
#define SDL_RaiseWindow cl_SDL_RaiseWindow
#define SDL_ReleaseGPUBuffer cl_SDL_ReleaseGPUBuffer
#define SDL_ReleaseGPUComputePipeline cl_SDL_ReleaseGPUComputePipeline
#define SDL_ReleaseGPUFence cl_SDL_ReleaseGPUFence
#define SDL_ReleaseGPUGraphicsPipeline cl_SDL_ReleaseGPUGraphicsPipeline
#define SDL_ReleaseGPUSampler cl_SDL_ReleaseGPUSampler
#define SDL_ReleaseGPUShader cl_SDL_ReleaseGPUShader
#define SDL_ReleaseGPUTexture cl_SDL_ReleaseGPUTexture
#define SDL_ReleaseGPUTransferBuffer cl_SDL_ReleaseGPUTransferBuffer
#define SDL_RenderFillRect cl_SDL_RenderFillRect
#define SDL_RenderPresent cl_SDL_RenderPresent
#define SDL_RenderReadPixels cl_SDL_RenderReadPixels
#define SDL_RenderRect cl_SDL_RenderRect
#define SDL_RenderTexture cl_SDL_RenderTexture
#define SDL_ResumeAudioStreamDevice cl_SDL_ResumeAudioStreamDevice
#define SDL_SetAssertionHandler cl_SDL_SetAssertionHandler
#define SDL_SetAtomicInt cl_SDL_SetAtomicInt
#define SDL_SetAudioStreamGain cl_SDL_SetAudioStreamGain
#define SDL_SetBooleanProperty cl_SDL_SetBooleanProperty
#define SDL_SetClipboardText cl_SDL_SetClipboardText
#define SDL_SetGPUScissor cl_SDL_SetGPUScissor
#define SDL_SetHint cl_SDL_SetHint
#define SDL_SetNumberProperty cl_SDL_SetNumberProperty
#define SDL_SetPointerProperty cl_SDL_SetPointerProperty
#define SDL_SetRenderClipRect cl_SDL_SetRenderClipRect
#define SDL_SetRenderDrawBlendMode cl_SDL_SetRenderDrawBlendMode
#define SDL_SetRenderDrawColor cl_SDL_SetRenderDrawColor
#define SDL_SetRenderTarget cl_SDL_SetRenderTarget
#define SDL_SetRenderVSync cl_SDL_SetRenderVSync
#define SDL_SetStringProperty cl_SDL_SetStringProperty
#define SDL_SetTextureBlendMode cl_SDL_SetTextureBlendMode
#define SDL_SetTextureColorMod cl_SDL_SetTextureColorMod
#define SDL_SetTextureScaleMode cl_SDL_SetTextureScaleMode
#define SDL_SetWindowFullscreen cl_SDL_SetWindowFullscreen
#define SDL_ShowCursor cl_SDL_ShowCursor
#define SDL_SubmitGPUCommandBuffer cl_SDL_SubmitGPUCommandBuffer
#define SDL_SubmitGPUCommandBufferAndAcquireFence cl_SDL_SubmitGPUCommandBufferAndAcquireFence
#define SDL_UnlockAudioStream cl_SDL_UnlockAudioStream
#define SDL_UnmapGPUTransferBuffer cl_SDL_UnmapGPUTransferBuffer
#define SDL_UploadToGPUBuffer cl_SDL_UploadToGPUBuffer
#define SDL_WaitForGPUFences cl_SDL_WaitForGPUFences
#define SDL_WasInit cl_SDL_WasInit
#define SDL_free cl_SDL_free
#define SDL_getenv cl_SDL_getenv
#define SDL_malloc cl_SDL_malloc
#define SDL_strcmp cl_SDL_strcmp

//load SDL3 and find its calls, true if every one is there
bool host_sdl3_load(void);

#endif

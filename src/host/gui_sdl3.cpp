#if defined(_HOST_GUI)
#if _HOST_GUI == 3

// SDL3 GUI driver. The 2D drawing is SDL's GPU renderer, and a shader is
// drawn into a texture with SDL's GPU interface on the same device.

#include <SDL3/SDL.h>
#include <stdint.h>
#include <string.h>
#include "gui_sdl3_event.h"

#if defined(__APPLE__)
#include <objc/message.h>
#include <objc/runtime.h>

static void set_macos_activation_policy(intptr_t policy)
{
	Class nsapp_class = (Class)objc_getClass("NSApplication");
	if (nsapp_class)
	{
		SEL shared_app_sel = sel_registerName("sharedApplication");
		id app = ((id (*)(Class, SEL))objc_msgSend)(nsapp_class, shared_app_sel);
		if (app)
		{
			SEL set_policy_sel = sel_registerName("setActivationPolicy:");
			((BOOL (*)(id, SEL, intptr_t))objc_msgSend)(app, set_policy_sel, policy);
		}
	}
}
#endif

// a shader, the pipeline that draws it into a target texture

struct Shader
{
	SDL_GPUGraphicsPipeline *pipeline;
};

static const SDL_PixelFormat target_pixel_format = SDL_PIXELFORMAT_ARGB8888;
static const SDL_GPUTextureFormat target_gpu_format = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM;

// a texture, as the GUI knows it. Only a glyph or a greyscale texture is
// drawn in a color, mode 1 or 2, a normal one is drawn as it is, the
// same as the drivers that do their own drawing.

struct Texture
{
	SDL_Texture *texture;
	uint64_t mode;
};

static void *new_texture(SDL_Texture *t, uint64_t mode)
{
	if (!t) return nullptr;
	auto texture = (Texture*)SDL_malloc(sizeof(Texture));
	texture->texture = t;
	texture->mode = mode;
	return texture;
}

static SDL_Window *window = nullptr;
static SDL_Renderer *renderer = nullptr;
static SDL_Texture *backbuffer = nullptr;
static SDL_GPUDevice *device = nullptr;
static SDL_GPUFence *shader_fence = nullptr;

static SDL_BlendMode premul_blend_mode()
{
	return SDL_ComposeCustomBlendMode(SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD,
		SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD);
}

static SDL_Texture *create_backbuffer(uint64_t w, uint64_t h)
{
	auto t = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_XRGB8888, SDL_TEXTUREACCESS_TARGET, (int)w, (int)h);
	SDL_SetTextureBlendMode(t, SDL_BLENDMODE_NONE);
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	return t;
}

// the GPU renderer, that lets a shader share the device the GUI is drawn
// with, came with SDL 3.4. On an older SDL3, the 3.2 of Debian 13 say, the
// driver is built without it, the GUI runs, and it can not draw a shader.
#define HOST_GUI_GPU SDL_VERSION_ATLEAST(3, 4, 0)

// the GPU device is made here, not by the renderer, to ask for no more
// than is used. A device SDL makes for itself wants depth clamping and the
// like, which a small GPU may not have, the Raspberry Pi 4 has no depth
// clamp, and SDL then picks a software device in its place.

static SDL_GPUDevice *create_device()
{
#if !HOST_GUI_GPU
	return nullptr;
#else
	auto props = SDL_CreateProperties();
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_SHADERS_SPIRV_BOOLEAN, true);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_SHADERS_MSL_BOOLEAN, true);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_CLIP_DISTANCE_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_DEPTH_CLAMPING_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_INDIRECT_DRAW_FIRST_INSTANCE_BOOLEAN, false);
	SDL_SetBooleanProperty(props, SDL_PROP_GPU_DEVICE_CREATE_FEATURE_ANISOTROPY_BOOLEAN, false);
	auto dev = SDL_CreateGPUDeviceWithProperties(props);
	SDL_DestroyProperties(props);
	return dev;
#endif
}

void host_gui_init(host_gui_rect *rect, uint64_t flags)
{
#if defined(__APPLE__)
	set_macos_activation_policy(0); // NSApplicationActivationPolicyRegular
#endif
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	// with no desktop, SDL on the bare display, a Raspberry Pi with no
	// window system say, the window is the whole screen. Vulkan can only
	// have it if it is made for Vulkan and is the size of the display mode.
	auto driver = SDL_GetCurrentVideoDriver();
	auto bare = driver && !SDL_strcmp(driver, "kmsdrm");
	if (bare)
	{
		auto mode = SDL_GetDesktopDisplayMode(SDL_GetPrimaryDisplay());
		if (mode)
		{
			rect->w = mode->w;
			rect->h = mode->h;
		}
	}
	window = SDL_CreateWindow("ChrysaLisp GUI Window", rect->w, rect->h,
		SDL_WINDOW_RESIZABLE | (bare ? SDL_WINDOW_VULKAN : 0));
	// the GPU renderer if there is one, so that a shader can share its device
	device = create_device();
#if HOST_GUI_GPU
	if (device) renderer = SDL_CreateGPURenderer(device, window);
#endif
	if (!renderer)
	{
		if (device) SDL_DestroyGPUDevice(device);
		device = nullptr;
		if (bare)
		{
			// a window made for Vulkan is no use to another renderer
			SDL_DestroyWindow(window);
			window = SDL_CreateWindow("ChrysaLisp GUI Window", rect->w, rect->h, SDL_WINDOW_RESIZABLE);
		}
		renderer = SDL_CreateRenderer(window, nullptr);
	}
	SDL_SetRenderVSync(renderer, 1);
	backbuffer = create_backbuffer(rect->w, rect->h);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	if (flags) SDL_HideCursor();
}

void host_gui_deinit()
{
	SDL_ShowCursor();
	if (backbuffer)
	{
		SDL_DestroyTexture(backbuffer);
		backbuffer = nullptr;
	}
	if (renderer)
	{
		SDL_DestroyRenderer(renderer);
		renderer = nullptr;
	}
	if (device)
	{
		if (shader_fence) SDL_ReleaseGPUFence(device, shader_fence);
		shader_fence = nullptr;
		SDL_DestroyGPUDevice(device);
		device = nullptr;
	}
	if (window)
	{
		SDL_DestroyWindow(window);
		window = nullptr;
	}

	// Drain pending events so Cocoa finishes processing the window destruction
	SDL_Event ev;
	while (SDL_PollEvent(&ev)) {}
	SDL_PumpEvents();

	SDL_QuitSubSystem(SDL_INIT_VIDEO | SDL_INIT_EVENTS);

#if defined(__APPLE__)
	set_macos_activation_policy(2); // NSApplicationActivationPolicyProhibited
#endif
}

uint64_t host_gui_poll_event(void *handle)
{
	return host_gui_sdl3_poll(handle);
}

void *host_gui_create_texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto surface = SDL_CreateSurfaceFrom((int)w, (int)h, SDL_PIXELFORMAT_ARGB8888, data, (int)s);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_SetTextureBlendMode(t, premul_blend_mode());
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	SDL_DestroySurface(surface);
	return new_texture(t, m);
}

void host_gui_destroy_texture(void *handle)
{
	auto texture = (Texture*)handle;
	if (!texture) return;
	SDL_DestroyTexture(texture->texture);
	SDL_free(texture);
}

void host_gui_begin_composite()
{
	SDL_SetRenderTarget(renderer, backbuffer);
}

void host_gui_end_composite()
{
	SDL_SetRenderTarget(renderer, 0);
}

void host_gui_flush(const host_gui_rect *rect)
{
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
	SDL_RenderTexture(renderer, backbuffer, 0, 0);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	SDL_RenderPresent(renderer);
}

static SDL_FRect frect(const host_gui_rect *rect)
{
	SDL_FRect r = {(float)rect->x, (float)rect->y, (float)rect->w, (float)rect->h};
	return r;
}

void host_gui_box(const host_gui_rect *rect)
{
	auto r = frect(rect);
	SDL_RenderRect(renderer, &r);
}

void host_gui_filled_box(const host_gui_rect *rect)
{
	auto r = frect(rect);
	SDL_RenderFillRect(renderer, &r);
}

void host_gui_set_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
	SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

void host_gui_set_texture_color(void *handle, uint8_t r, uint8_t g, uint8_t b)
{
	auto texture = (Texture*)handle;
	if (texture->mode) SDL_SetTextureColorMod(texture->texture, r, g, b);
}

void host_gui_blit(void *handle, const host_gui_rect *srect, const host_gui_rect *drect)
{
	auto t = ((Texture*)handle)->texture;
	auto s = frect(srect);
	auto d = frect(drect);
	SDL_RenderTexture(renderer, t, &s, &d);
}

void host_gui_set_clip(const host_gui_rect *rect)
{
	SDL_SetRenderClipRect(renderer, (const SDL_Rect*)rect);
}

void host_gui_resize(uint64_t w, uint64_t h)
{
	SDL_DestroyTexture(backbuffer);
	backbuffer = create_backbuffer(w, h);
}

uint64_t host_gui_clip_put(const char *text)
{
	return SDL_SetClipboardText(text) ? 0 : -1;
}

char *host_gui_clip_get()
{
	return SDL_GetClipboardText();
}

void host_gui_clip_free(char *text)
{
	SDL_free(text);
}

// the shading language the driver takes, 0 if it can not draw a shader

uint64_t host_gui_shader_format()
{
	if (!device) return 0;
	auto formats = SDL_GetGPUShaderFormats(device);
	if (formats & SDL_GPU_SHADERFORMAT_MSL) return 1;
	if (formats & SDL_GPU_SHADERFORMAT_SPIRV) return 2;
	return 0;
}

static SDL_GPUShader *create_shader(const char *code, uint64_t size, SDL_GPUShaderStage stage, const char *entry)
{
	SDL_GPUShaderCreateInfo info = {};
	info.code = (const Uint8*)code;
	info.code_size = size;
	info.entrypoint = entry;
	info.format = host_gui_shader_format() == 1 ? SDL_GPU_SHADERFORMAT_MSL : SDL_GPU_SHADERFORMAT_SPIRV;
	info.stage = stage;
	info.num_uniform_buffers = 1;
	return SDL_CreateGPUShader(device, &info);
}

void *host_gui_shader_create(const char *vertex, uint64_t vertex_size, const char *fragment, uint64_t fragment_size)
{
	if (!host_gui_shader_format()) return nullptr;
	auto vs = create_shader(vertex, vertex_size, SDL_GPU_SHADERSTAGE_VERTEX, "vertex_main");
	auto fs = create_shader(fragment, fragment_size, SDL_GPU_SHADERSTAGE_FRAGMENT, "fragment_main");
	SDL_GPUGraphicsPipeline *pipeline = nullptr;
	if (vs && fs)
	{
		SDL_GPUColorTargetDescription target = {};
		target.format = target_gpu_format;
		SDL_GPUGraphicsPipelineCreateInfo info = {};
		info.vertex_shader = vs;
		info.fragment_shader = fs;
		info.primitive_type = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST;
		info.rasterizer_state.fill_mode = SDL_GPU_FILLMODE_FILL;
		// depth is clipped, not clamped, the device was not asked for clamping
		info.rasterizer_state.enable_depth_clip = true;
		info.target_info.color_target_descriptions = &target;
		info.target_info.num_color_targets = 1;
		pipeline = SDL_CreateGPUGraphicsPipeline(device, &info);
	}
	if (!pipeline) SDL_Log("shader: %s", SDL_GetError());
	if (vs) SDL_ReleaseGPUShader(device, vs);
	if (fs) SDL_ReleaseGPUShader(device, fs);
	if (!pipeline) return nullptr;
	auto shader = (Shader*)SDL_malloc(sizeof(Shader));
	shader->pipeline = pipeline;
	return shader;
}

void host_gui_shader_destroy(void *handle)
{
	auto shader = (Shader*)handle;
	if (!shader) return;
	if (device) SDL_ReleaseGPUGraphicsPipeline(device, shader->pipeline);
	SDL_free(shader);
}

// a texture a shader can be drawn into, and that blits as any other does

void *host_gui_shader_texture(uint64_t w, uint64_t h)
{
	if (!device) return nullptr;
	auto t = SDL_CreateTexture(renderer, target_pixel_format, SDL_TEXTUREACCESS_TARGET, (int)w, (int)h);
	if (!t) return nullptr;
	SDL_SetTextureBlendMode(t, premul_blend_mode());
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	return new_texture(t, 0);
}

// a shader is drawn into a texture, all of it, or the part given. A draw
// that takes the GPU a long time holds up the drawing of the GUI behind it,
// so there is only ever one on the go. While the last has not finished this
// draws nothing and returns 0, and the caller tries again later. A caller
// with a slow GPU draws a frame as strips, each small enough to be done in
// a tick, and the GUI is drawn in between them.

uint64_t host_gui_shader_draw(void *handle, void *texture, const void *block, uint64_t size, const host_gui_rect *rect)
{
#if !HOST_GUI_GPU
	return 0;
#else
	auto shader = (Shader*)handle;
	if (!device || !shader || !texture) return 0;
	if (shader_fence)
	{
		if (!SDL_QueryGPUFence(device, shader_fence)) return 0;
		SDL_ReleaseGPUFence(device, shader_fence);
		shader_fence = nullptr;
	}
	auto t = ((Texture*)texture)->texture;
	auto target = (SDL_GPUTexture*)SDL_GetPointerProperty(SDL_GetTextureProperties(t),
		SDL_PROP_TEXTURE_GPU_TEXTURE_POINTER, nullptr);
	if (!target) return 0;
	float w, h;
	SDL_GetTextureSize(t, &w, &h);
	float target_size[4] = {w, h, 0.0f, 0.0f};
	// what the renderer has drawn so far goes first
	SDL_FlushRenderer(renderer);
	auto cmd = SDL_AcquireGPUCommandBuffer(device);
	if (!cmd) return 0;
	SDL_PushGPUVertexUniformData(cmd, 0, target_size, sizeof(target_size));
	SDL_PushGPUFragmentUniformData(cmd, 0, block, (Uint32)size);
	SDL_GPUColorTargetInfo info = {};
	info.texture = target;
	// a part leaves the rest of the texture as it was
	info.load_op = rect ? SDL_GPU_LOADOP_LOAD : SDL_GPU_LOADOP_DONT_CARE;
	info.store_op = SDL_GPU_STOREOP_STORE;
	auto pass = SDL_BeginGPURenderPass(cmd, &info, 1, nullptr);
	SDL_BindGPUGraphicsPipeline(pass, shader->pipeline);
	if (rect)
	{
		SDL_Rect scissor = {rect->x, rect->y, rect->w, rect->h};
		SDL_SetGPUScissor(pass, &scissor);
	}
	SDL_DrawGPUPrimitives(pass, 3, 1, 0, 0);
	SDL_EndGPURenderPass(pass);
	shader_fence = SDL_SubmitGPUCommandBufferAndAcquireFence(cmd);
	return 1;
#endif
}

// copy a texture into a buffer, 32 bit premultiplied argb, as it was uploaded

uint64_t host_gui_read_texture(void *handle, uint32_t *data, uint64_t w, uint64_t h, uint64_t stride)
{
	if (!handle || !renderer) return 0;
	auto t = ((Texture*)handle)->texture;
	// it is drawn, as it is, to a texture that can be read from
	auto copy = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET, (int)w, (int)h);
	if (!copy) return 0;
	SDL_BlendMode blend;
	Uint8 r, g, b;
	SDL_GetTextureBlendMode(t, &blend);
	SDL_GetTextureColorMod(t, &r, &g, &b);
	SDL_SetTextureBlendMode(t, SDL_BLENDMODE_NONE);
	SDL_SetTextureColorMod(t, 255, 255, 255);
	auto target = SDL_GetRenderTarget(renderer);
	SDL_SetRenderTarget(renderer, copy);
	SDL_RenderTexture(renderer, t, 0, 0);
	auto surface = SDL_RenderReadPixels(renderer, nullptr);
	SDL_SetRenderTarget(renderer, target);
	SDL_SetTextureBlendMode(t, blend);
	SDL_SetTextureColorMod(t, r, g, b);
	bool ok = surface && SDL_ConvertPixels(surface->w, surface->h, surface->format, surface->pixels, surface->pitch,
		SDL_PIXELFORMAT_ARGB8888, data, (int)stride);
	if (surface) SDL_DestroySurface(surface);
	SDL_DestroyTexture(copy);
	return ok;
}

void (*host_gui_funcs[]) = {
	(void*)host_gui_init,
	(void*)host_gui_deinit,
	(void*)host_gui_box,
	(void*)host_gui_filled_box,
	(void*)host_gui_blit,
	(void*)host_gui_set_clip,
	(void*)host_gui_set_color,
	(void*)host_gui_set_texture_color,
	(void*)host_gui_destroy_texture,
	(void*)host_gui_create_texture,
	(void*)host_gui_begin_composite,
	(void*)host_gui_end_composite,
	(void*)host_gui_flush,
	(void*)host_gui_resize,
	(void*)host_gui_poll_event,
	(void*)host_gui_clip_put,
	(void*)host_gui_clip_get,
	(void*)host_gui_clip_free,
	(void*)host_gui_shader_format,
	(void*)host_gui_shader_create,
	(void*)host_gui_shader_destroy,
	(void*)host_gui_shader_texture,
	(void*)host_gui_shader_draw,
	(void*)host_gui_read_texture,
};

#endif
#endif

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

// a shader is built by a thread of its own. A driver can take a long time
// to build one, the Raspberry Pi 4's takes 18 seconds over the raymarch
// shader the first time it sees it, and the GUI must not stop for that.
enum
{
	shader_building,
	shader_ready,
	shader_failed,
	shader_dropped,
};

struct Shader
{
	SDL_GPUGraphicsPipeline *pipeline;
	SDL_AtomicInt state;
	SDL_GPUShaderFormat format;
	Uint8 *vertex;
	Uint8 *fragment;
	size_t vertex_size;
	size_t fragment_size;
	// a pair that draws triangles, how many attrs a vertex has, how many
	// floats each is, and whether a triangle that faces away is left out
	bool mesh;
	Uint8 num_attrs;
	Uint8 attrs[8];
	Uint8 cull;
	Uint32 stride;
};

// the vertices of a mesh, on the GPU, as floats
struct Mesh
{
	SDL_GPUBuffer *buffer;
	Uint32 floats;
};

// the depth buffer triangles are drawn with, kept for the next frame of
// the same size
static SDL_GPUTexture *depth_texture = nullptr;
static Uint32 depth_w = 0, depth_h = 0;
static SDL_GPUTextureFormat depth_format = SDL_GPU_TEXTUREFORMAT_INVALID;

// how many shaders are being built
static SDL_AtomicInt shader_builds;

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
		host_gui_sdl3_wheel = -1;
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
		// a shader being built is using the device
		while (SDL_GetAtomicInt(&shader_builds) > 0) SDL_Delay(10);
		if (shader_fence) SDL_ReleaseGPUFence(device, shader_fence);
		shader_fence = nullptr;
		if (depth_texture) SDL_ReleaseGPUTexture(device, depth_texture);
		depth_texture = nullptr;
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

static SDL_GPUTextureFormat find_depth_format();

static SDL_GPUShader *create_shader(const Uint8 *code, size_t size, SDL_GPUShaderFormat format,
	SDL_GPUShaderStage stage, const char *entry, Uint32 uniforms)
{
	SDL_GPUShaderCreateInfo info = {};
	info.code = code;
	info.code_size = size;
	info.entrypoint = entry;
	info.format = format;
	info.stage = stage;
	info.num_uniform_buffers = uniforms;
	return SDL_CreateGPUShader(device, &info);
}

// the format of a depth buffer, the first of these the GPU has

static SDL_GPUTextureFormat find_depth_format()
{
	if (depth_format != SDL_GPU_TEXTUREFORMAT_INVALID) return depth_format;
	const SDL_GPUTextureFormat formats[] = {SDL_GPU_TEXTUREFORMAT_D32_FLOAT,
		SDL_GPU_TEXTUREFORMAT_D24_UNORM, SDL_GPU_TEXTUREFORMAT_D16_UNORM};
	for (auto f : formats)
	{
		if (SDL_GPUTextureSupportsFormat(device, f, SDL_GPU_TEXTURETYPE_2D, SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET))
		{
			depth_format = f;
			break;
		}
	}
	return depth_format;
}

static int SDLCALL build_shader(void *data)
{
	auto shader = (Shader*)data;
	// a pair that draws triangles has the inputs of the vertex shader as the
	// vertex stage's block, and the fragment stage has the size of the target
	// as a second block, after the inputs of the pixel shader
	auto vs = create_shader(shader->vertex, shader->vertex_size, shader->format,
		SDL_GPU_SHADERSTAGE_VERTEX, "vertex_main", 1);
	auto fs = create_shader(shader->fragment, shader->fragment_size, shader->format,
		SDL_GPU_SHADERSTAGE_FRAGMENT, "fragment_main", shader->mesh ? 2 : 1);
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
		SDL_GPUVertexBufferDescription buffer = {};
		SDL_GPUVertexAttribute attrs[8] = {};
		if (shader->mesh)
		{
			// the attrs of a vertex, floats, one after another in the one buffer
			const SDL_GPUVertexElementFormat formats[] = {SDL_GPU_VERTEXELEMENTFORMAT_FLOAT,
				SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3,
				SDL_GPU_VERTEXELEMENTFORMAT_FLOAT4};
			Uint32 offset = 0;
			for (int i = 0; i < shader->num_attrs; ++i)
			{
				attrs[i].location = i;
				attrs[i].buffer_slot = 0;
				attrs[i].format = formats[shader->attrs[i] - 1];
				attrs[i].offset = offset;
				offset += shader->attrs[i] * sizeof(float);
			}
			buffer.slot = 0;
			buffer.pitch = offset;
			buffer.input_rate = SDL_GPU_VERTEXINPUTRATE_VERTEX;
			info.vertex_input_state.vertex_buffer_descriptions = &buffer;
			info.vertex_input_state.num_vertex_buffers = 1;
			info.vertex_input_state.vertex_attributes = attrs;
			info.vertex_input_state.num_vertex_attributes = shader->num_attrs;
			// the front of a triangle is the side it goes round counter
			// clockwise from, and what is nearest is what is seen
			info.rasterizer_state.front_face = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE;
			info.rasterizer_state.cull_mode = shader->cull == 1 ? SDL_GPU_CULLMODE_BACK
				: shader->cull == 2 ? SDL_GPU_CULLMODE_FRONT : SDL_GPU_CULLMODE_NONE;
			// a pixel comes with its alpha multiplied in, and goes over what is there
			target.blend_state.enable_blend = true;
			target.blend_state.src_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE;
			target.blend_state.dst_color_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA;
			target.blend_state.color_blend_op = SDL_GPU_BLENDOP_ADD;
			target.blend_state.src_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE;
			target.blend_state.dst_alpha_blendfactor = SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA;
			target.blend_state.alpha_blend_op = SDL_GPU_BLENDOP_ADD;
			info.depth_stencil_state.enable_depth_test = true;
			info.depth_stencil_state.enable_depth_write = true;
			info.depth_stencil_state.compare_op = SDL_GPU_COMPAREOP_LESS;
			info.target_info.has_depth_stencil_target = true;
			info.target_info.depth_stencil_format = find_depth_format();
		}
		pipeline = SDL_CreateGPUGraphicsPipeline(device, &info);
	}
	if (!pipeline) SDL_Log("shader: %s", SDL_GetError());
	if (vs) SDL_ReleaseGPUShader(device, vs);
	if (fs) SDL_ReleaseGPUShader(device, fs);
	SDL_free(shader->vertex);
	SDL_free(shader->fragment);
	shader->pipeline = pipeline;
	if (!SDL_CompareAndSwapAtomicInt(&shader->state, shader_building, pipeline ? shader_ready : shader_failed))
	{
		// it was destroyed while it was being built
		if (pipeline) SDL_ReleaseGPUGraphicsPipeline(device, pipeline);
		SDL_free(shader);
	}
	SDL_AddAtomicInt(&shader_builds, -1);
	return 0;
}

static Uint8 *copy_code(const char *code, uint64_t size)
{
	// a byte more, and zero, text is read as far as that by some drivers
	auto copy = (Uint8*)SDL_malloc(size + 1);
	SDL_memcpy(copy, code, size);
	copy[size] = 0;
	return copy;
}

// the handle is given at once, the shader may not be built yet, and may
// turn out not to build at all. host_gui_shader_draw says which.

static void *start_shader(Shader *shader, const char *vertex, uint64_t vertex_size,
	const char *fragment, uint64_t fragment_size)
{
	shader->pipeline = nullptr;
	shader->format = host_gui_shader_format() == 1 ? SDL_GPU_SHADERFORMAT_MSL : SDL_GPU_SHADERFORMAT_SPIRV;
	shader->vertex = copy_code(vertex, vertex_size);
	shader->fragment = copy_code(fragment, fragment_size);
	shader->vertex_size = vertex_size;
	shader->fragment_size = fragment_size;
	SDL_SetAtomicInt(&shader->state, shader_building);
	SDL_AddAtomicInt(&shader_builds, 1);
	// a driver's compiler can want a deep stack
	auto props = SDL_CreateProperties();
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_ENTRY_FUNCTION_POINTER, (void*)build_shader);
	SDL_SetStringProperty(props, SDL_PROP_THREAD_CREATE_NAME_STRING, "shader");
	SDL_SetPointerProperty(props, SDL_PROP_THREAD_CREATE_USERDATA_POINTER, shader);
	SDL_SetNumberProperty(props, SDL_PROP_THREAD_CREATE_STACKSIZE_NUMBER, 8 * 1024 * 1024);
	auto thread = SDL_CreateThreadWithProperties(props);
	SDL_DestroyProperties(props);
	if (thread) SDL_DetachThread(thread);
	else build_shader(shader);
	return shader;
}

void *host_gui_shader_create(const char *vertex, uint64_t vertex_size, const char *fragment, uint64_t fragment_size)
{
	if (!host_gui_shader_format()) return nullptr;
	auto shader = (Shader*)SDL_malloc(sizeof(Shader));
	shader->mesh = false;
	return start_shader(shader, vertex, vertex_size, fragment, fragment_size);
}

// a vertex shader and a pixel shader that draw triangles. The layout is how
// many attrs a vertex has, the cull, 0 none, 1 those that face away, 2 those
// that face us, then how many floats each attr is, a byte each. It is
// destroyed as a shader is.

void *host_gui_pair_create(const char *vertex, uint64_t vertex_size, const char *fragment, uint64_t fragment_size,
	const uint8_t *layout)
{
#if !HOST_GUI_GPU
	return nullptr;
#else
	if (!host_gui_shader_format() || find_depth_format() == SDL_GPU_TEXTUREFORMAT_INVALID) return nullptr;
	auto shader = (Shader*)SDL_malloc(sizeof(Shader));
	shader->mesh = true;
	shader->num_attrs = layout[0] > 8 ? 8 : layout[0];
	shader->cull = layout[1];
	shader->stride = 0;
	for (int i = 0; i < shader->num_attrs; ++i)
	{
		auto n = layout[2 + i];
		shader->attrs[i] = n < 1 ? 1 : n > 4 ? 4 : n;
		shader->stride += shader->attrs[i];
	}
	return start_shader(shader, vertex, vertex_size, fragment, fragment_size);
#endif
}

// the vertices of a mesh, kept on the GPU. They come as a double for each
// number, as the nodes have them, and are kept as floats.

void *host_gui_mesh_create(const void *verts, uint64_t size)
{
#if !HOST_GUI_GPU
	return nullptr;
#else
	Uint32 floats = (Uint32)(size / 8);
	if (!device || !floats) return nullptr;
	SDL_GPUBufferCreateInfo bi = {};
	bi.usage = SDL_GPU_BUFFERUSAGE_VERTEX;
	bi.size = floats * (Uint32)sizeof(float);
	auto buffer = SDL_CreateGPUBuffer(device, &bi);
	SDL_GPUTransferBufferCreateInfo ti = {};
	ti.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
	ti.size = bi.size;
	auto transfer = SDL_CreateGPUTransferBuffer(device, &ti);
	auto cmd = buffer && transfer ? SDL_AcquireGPUCommandBuffer(device) : nullptr;
	if (!cmd)
	{
		if (buffer) SDL_ReleaseGPUBuffer(device, buffer);
		if (transfer) SDL_ReleaseGPUTransferBuffer(device, transfer);
		return nullptr;
	}
	auto out = (float*)SDL_MapGPUTransferBuffer(device, transfer, false);
	for (Uint32 i = 0; i < floats; ++i)
	{
		double v;
		SDL_memcpy(&v, (const Uint8*)verts + (size_t)i * 8, 8);
		out[i] = (float)v;
	}
	SDL_UnmapGPUTransferBuffer(device, transfer);
	auto copy = SDL_BeginGPUCopyPass(cmd);
	SDL_GPUTransferBufferLocation from = {transfer, 0};
	SDL_GPUBufferRegion to = {buffer, 0, bi.size};
	SDL_UploadToGPUBuffer(copy, &from, &to, false);
	SDL_EndGPUCopyPass(copy);
	SDL_SubmitGPUCommandBuffer(cmd);
	SDL_ReleaseGPUTransferBuffer(device, transfer);
	auto mesh = (Mesh*)SDL_malloc(sizeof(Mesh));
	mesh->buffer = buffer;
	mesh->floats = floats;
	return mesh;
#endif
}

void host_gui_mesh_destroy(void *handle)
{
#if HOST_GUI_GPU
	auto mesh = (Mesh*)handle;
	if (!mesh) return;
	if (device) SDL_ReleaseGPUBuffer(device, mesh->buffer);
	SDL_free(mesh);
#endif
}

void host_gui_shader_destroy(void *handle)
{
	auto shader = (Shader*)handle;
	if (!shader) return;
	// still being built, the thread that is building it frees it
	if (SDL_CompareAndSwapAtomicInt(&shader->state, shader_building, shader_dropped)) return;
	if (device && shader->pipeline) SDL_ReleaseGPUGraphicsPipeline(device, shader->pipeline);
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

// triangles, drawn into a texture by pairs of shaders, with a depth buffer,
// a frame of them. What is nearest is what is seen, and the texture is
// cleared first. As a shader is, a frame is one draw on the go at a time,
// 0 is the GPU still busy with the last, or a pair not yet built, nothing
// was drawn, try again. -1 is a pair that did not build.
//
// The frame is a count, 8 bytes, then for each thing drawn, 8 bytes each,
// the pair, the mesh, the length of the vertex shader's block and of the
// pixel shader's block, then the two blocks, each made up to a whole 8
// bytes.

uint64_t host_gui_tris_draw(void *texture, const void *frame, uint64_t size)
{
#if !HOST_GUI_GPU
	return 0;
#else
	if (!device || !texture || size < 8) return 0;
	if (find_depth_format() == SDL_GPU_TEXTUREFORMAT_INVALID) return (uint64_t)-1;
	struct Draw { Shader *pair; Mesh *mesh; uint64_t vlen, plen; const Uint8 *vblock, *pblock; };
	auto data = (const Uint8*)frame;
	uint64_t count;
	SDL_memcpy(&count, data, 8);
	auto draws = (Draw*)SDL_malloc(sizeof(Draw) * (count ? count : 1));
	const Uint8 *at = data + 8, *end = data + size;
	uint64_t good = 0, result = 1;
	for (; good < count && at + 32 <= end; ++good)
	{
		auto d = &draws[good];
		SDL_memcpy(d, at, 32);
		at += 32;
		auto vpad = (d->vlen + 7) & ~(uint64_t)7, ppad = (d->plen + 7) & ~(uint64_t)7;
		if (at + vpad + ppad > end) break;
		d->vblock = at;
		d->pblock = at + vpad;
		at += vpad + ppad;
		// every pair of the frame has to be built before any of it is drawn
		auto state = d->pair ? SDL_GetAtomicInt(&d->pair->state) : shader_failed;
		if (state == shader_building) result = 0;
		else if (state != shader_ready || !d->pair->mesh) { result = (uint64_t)-1; break; }
	}
	if (result == 1 && shader_fence)
	{
		if (!SDL_QueryGPUFence(device, shader_fence)) result = 0;
		else
		{
			SDL_ReleaseGPUFence(device, shader_fence);
			shader_fence = nullptr;
		}
	}
	auto t = ((Texture*)texture)->texture;
	auto target = result == 1 ? (SDL_GPUTexture*)SDL_GetPointerProperty(SDL_GetTextureProperties(t),
		SDL_PROP_TEXTURE_GPU_TEXTURE_POINTER, nullptr) : nullptr;
	if (result == 1 && !target) result = 0;
	float fw = 0, fh = 0;
	if (result == 1) SDL_GetTextureSize(t, &fw, &fh);
	Uint32 w = (Uint32)fw, h = (Uint32)fh;
	if (result == 1 && (!depth_texture || depth_w != w || depth_h != h))
	{
		if (depth_texture) SDL_ReleaseGPUTexture(device, depth_texture);
		SDL_GPUTextureCreateInfo info = {};
		info.type = SDL_GPU_TEXTURETYPE_2D;
		info.format = depth_format;
		info.usage = SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET;
		info.width = w;
		info.height = h;
		info.layer_count_or_depth = 1;
		info.num_levels = 1;
		depth_texture = SDL_CreateGPUTexture(device, &info);
		depth_w = w;
		depth_h = h;
		if (!depth_texture) result = 0;
	}
	SDL_GPUCommandBuffer *cmd = nullptr;
	if (result == 1)
	{
		// what the renderer has drawn so far goes first
		SDL_FlushRenderer(renderer);
		cmd = SDL_AcquireGPUCommandBuffer(device);
		if (!cmd) result = 0;
	}
	if (result == 1)
	{
		SDL_GPUColorTargetInfo color = {};
		color.texture = target;
		color.load_op = SDL_GPU_LOADOP_CLEAR;
		color.store_op = SDL_GPU_STOREOP_STORE;
		SDL_GPUDepthStencilTargetInfo depth = {};
		depth.texture = depth_texture;
		depth.clear_depth = 1.0f;
		depth.load_op = SDL_GPU_LOADOP_CLEAR;
		depth.store_op = SDL_GPU_STOREOP_DONT_CARE;
		depth.stencil_load_op = SDL_GPU_LOADOP_DONT_CARE;
		depth.stencil_store_op = SDL_GPU_STOREOP_DONT_CARE;
		float target_size[4] = {fw, fh, 0.0f, 0.0f};
		auto pass = SDL_BeginGPURenderPass(cmd, &color, 1, &depth);
		for (uint64_t i = 0; i < good; ++i)
		{
			auto d = &draws[i];
			if (!d->mesh || !d->pair->stride) continue;
			SDL_BindGPUGraphicsPipeline(pass, d->pair->pipeline);
			SDL_GPUBufferBinding binding = {d->mesh->buffer, 0};
			SDL_BindGPUVertexBuffers(pass, 0, &binding, 1);
			SDL_PushGPUVertexUniformData(cmd, 0, d->vblock, (Uint32)d->vlen);
			SDL_PushGPUFragmentUniformData(cmd, 0, d->pblock, (Uint32)d->plen);
			SDL_PushGPUFragmentUniformData(cmd, 1, target_size, sizeof(target_size));
			SDL_DrawGPUPrimitives(pass, d->mesh->floats / d->pair->stride, 1, 0, 0);
		}
		SDL_EndGPURenderPass(pass);
		shader_fence = SDL_SubmitGPUCommandBufferAndAcquireFence(cmd);
	}
	SDL_free(draws);
	return result;
#endif
}

// a shader is drawn into a texture, all of it, or the part given. A draw
// that takes the GPU a long time holds up the drawing of the GUI behind it,
// so there is only ever one on the go. While the last has not finished, or
// the shader is still being built, this draws nothing and returns 0, and the
// caller tries again later. A caller with a slow GPU draws a frame as
// strips, each small enough to be done in a tick, and the GUI is drawn in
// between them. A shader that did not build returns -1.

uint64_t host_gui_shader_draw(void *handle, void *texture, const void *block, uint64_t size, const host_gui_rect *rect)
{
#if !HOST_GUI_GPU
	return 0;
#else
	auto shader = (Shader*)handle;
	if (!device || !shader || !texture) return 0;
	auto state = SDL_GetAtomicInt(&shader->state);
	if (state == shader_building) return 0;
	if (state != shader_ready || shader->mesh) return (uint64_t)-1;
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
	(void*)host_gui_mesh_create,
	(void*)host_gui_mesh_destroy,
	(void*)host_gui_pair_create,
	(void*)host_gui_tris_draw,
};

#endif
#endif

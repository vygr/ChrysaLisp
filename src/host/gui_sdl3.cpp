#if defined(_HOST_GUI)
#if _HOST_GUI == 3

// SDL3 GUI driver. The 2D drawing is SDL's GPU renderer, and a shader is
// drawn into a texture with SDL's GPU interface on the same device.

#include <SDL3/SDL.h>
#include <stdint.h>
#include <string.h>

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

// what the GUI service is given, a rect, and an event record with the
// layout of an SDL2 event, as the other drivers give it

struct Rect
{
	int32_t x, y, w, h;
};

enum
{
	EV_QUIT = 0x100,
	EV_WINDOWEVENT = 0x200,
	EV_KEYDOWN = 0x300,
	EV_KEYUP,
	EV_MOUSEMOTION = 0x400,
	EV_MOUSEBUTTONDOWN,
	EV_MOUSEBUTTONUP,
	EV_MOUSEWHEEL,
};

enum
{
	EV_WINDOW_SHOWN = 0x1,
	EV_WINDOW_SIZE_CHANGED = 0x6,
	EV_WINDOW_RESTORED = 0x9,
};

struct WindowEvent
{
	uint32_t type, timestamp, windowID;
	uint8_t event, padding1, padding2, padding3;
	int32_t data1, data2;
};

struct KeyEvent
{
	uint32_t type, timestamp, windowID;
	uint8_t state, repeat, padding2, padding3;
	int32_t scancode, sym;
	uint16_t mod;
	uint32_t unused;
};

struct MotionEvent
{
	uint32_t type, timestamp, windowID, which, state;
	int32_t x, y, xrel, yrel;
};

struct ButtonEvent
{
	uint32_t type, timestamp, windowID, which;
	uint8_t button, state, clicks, padding1;
	int32_t x, y;
};

struct WheelEvent
{
	uint32_t type, timestamp, windowID, which;
	int32_t x, y;
	uint32_t direction;
};

union Event
{
	uint32_t type;
	WindowEvent window;
	KeyEvent key;
	MotionEvent motion;
	ButtonEvent button;
	WheelEvent wheel;
	uint8_t padding[56];
};

// a shader, the pipeline that draws it into a target texture

struct Shader
{
	SDL_GPUGraphicsPipeline *pipeline;
};

static const SDL_PixelFormat target_pixel_format = SDL_PIXELFORMAT_ARGB8888;
static const SDL_GPUTextureFormat target_gpu_format = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM;

static SDL_Window *window = nullptr;
static SDL_Renderer *renderer = nullptr;
static SDL_Texture *backbuffer = nullptr;
static SDL_GPUDevice *device = nullptr;

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

void host_gui_init(Rect *rect, uint64_t flags)
{
#if defined(__APPLE__)
	set_macos_activation_policy(0); // NSApplicationActivationPolicyRegular
#endif
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	window = SDL_CreateWindow("ChrysaLisp GUI Window", rect->w, rect->h, SDL_WINDOW_RESIZABLE);
	// the GPU renderer if there is one, so that a shader can share its device
	renderer = SDL_CreateGPURenderer(nullptr, window);
	if (renderer) device = SDL_GetGPURendererDevice(renderer);
	else renderer = SDL_CreateRenderer(window, nullptr);
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

// the next event for the GUI service, 0 if there is none. Not every
// SDL event is one it takes, so they are read till one is.

static bool next_event(Event *out)
{
	SDL_Event e;
	while (SDL_PollEvent(&e))
	{
		memset(out, 0, sizeof(Event));
		switch (e.type)
		{
		case SDL_EVENT_QUIT:
			out->type = EV_QUIT;
			return true;
		case SDL_EVENT_WINDOW_RESIZED:
			out->window.type = EV_WINDOWEVENT;
			out->window.event = EV_WINDOW_SIZE_CHANGED;
			out->window.data1 = e.window.data1;
			out->window.data2 = e.window.data2;
			return true;
		case SDL_EVENT_WINDOW_SHOWN:
		case SDL_EVENT_WINDOW_RESTORED:
			out->window.type = EV_WINDOWEVENT;
			out->window.event = e.type == SDL_EVENT_WINDOW_SHOWN ? EV_WINDOW_SHOWN : EV_WINDOW_RESTORED;
			return true;
		case SDL_EVENT_KEY_DOWN:
		case SDL_EVENT_KEY_UP:
			out->key.type = e.type == SDL_EVENT_KEY_DOWN ? EV_KEYDOWN : EV_KEYUP;
			out->key.state = e.key.down;
			out->key.repeat = e.key.repeat;
			out->key.scancode = e.key.scancode;
			out->key.sym = e.key.key;
			out->key.mod = e.key.mod;
			return true;
		case SDL_EVENT_MOUSE_MOTION:
			out->motion.type = EV_MOUSEMOTION;
			out->motion.state = e.motion.state;
			out->motion.x = (int32_t)e.motion.x;
			out->motion.y = (int32_t)e.motion.y;
			out->motion.xrel = (int32_t)e.motion.xrel;
			out->motion.yrel = (int32_t)e.motion.yrel;
			return true;
		case SDL_EVENT_MOUSE_BUTTON_DOWN:
		case SDL_EVENT_MOUSE_BUTTON_UP:
			out->button.type = e.type == SDL_EVENT_MOUSE_BUTTON_DOWN ? EV_MOUSEBUTTONDOWN : EV_MOUSEBUTTONUP;
			out->button.button = e.button.button;
			out->button.state = e.button.down;
			out->button.clicks = e.button.clicks;
			out->button.x = (int32_t)e.button.x;
			out->button.y = (int32_t)e.button.y;
			return true;
		case SDL_EVENT_MOUSE_WHEEL:
			out->wheel.type = EV_MOUSEWHEEL;
			out->wheel.x = e.wheel.integer_x;
			out->wheel.y = e.wheel.integer_y;
			out->wheel.direction = e.wheel.direction;
			// a wheel can turn less than a whole step
			if (out->wheel.x || out->wheel.y) return true;
			break;
		default:
			break;
		}
	}
	return false;
}

// with no handle the question is only if there is an event, and it
// is kept for the call that takes it

uint64_t host_gui_poll_event(void *handle)
{
	static Event pending;
	static bool have_pending = false;
	SDL_PumpEvents();
	if (!have_pending) have_pending = next_event(&pending);
	if (!have_pending) return 0;
	if (handle)
	{
		memcpy(handle, &pending, sizeof(Event));
		have_pending = false;
	}
	return 1;
}

void *host_gui_create_texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto surface = SDL_CreateSurfaceFrom((int)w, (int)h, SDL_PIXELFORMAT_ARGB8888, data, (int)s);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_SetTextureBlendMode(t, premul_blend_mode());
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	SDL_DestroySurface(surface);
	return t;
}

void host_gui_destroy_texture(void *handle)
{
	auto t = (SDL_Texture*)handle;
	SDL_DestroyTexture(t);
}

void host_gui_begin_composite()
{
	SDL_SetRenderTarget(renderer, backbuffer);
}

void host_gui_end_composite()
{
	SDL_SetRenderTarget(renderer, 0);
}

void host_gui_flush(const Rect *rect)
{
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
	SDL_RenderTexture(renderer, backbuffer, 0, 0);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	SDL_RenderPresent(renderer);
}

static SDL_FRect frect(const Rect *rect)
{
	SDL_FRect r = {(float)rect->x, (float)rect->y, (float)rect->w, (float)rect->h};
	return r;
}

void host_gui_box(const Rect *rect)
{
	auto r = frect(rect);
	SDL_RenderRect(renderer, &r);
}

void host_gui_filled_box(const Rect *rect)
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
	auto t = (SDL_Texture*)handle;
	SDL_SetTextureColorMod(t, r, g, b);
}

void host_gui_blit(void *handle, const Rect *srect, const Rect *drect)
{
	auto t = (SDL_Texture*)handle;
	auto s = frect(srect);
	auto d = frect(drect);
	SDL_RenderTexture(renderer, t, &s, &d);
}

void host_gui_set_clip(const Rect *rect)
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
	return t;
}

// draw the shader over the whole of the texture, the block is its inputs

void host_gui_shader_draw(void *handle, void *texture, const void *block, uint64_t size)
{
	auto shader = (Shader*)handle;
	auto t = (SDL_Texture*)texture;
	if (!device || !shader || !t) return;
	auto target = (SDL_GPUTexture*)SDL_GetPointerProperty(SDL_GetTextureProperties(t),
		SDL_PROP_TEXTURE_GPU_TEXTURE_POINTER, nullptr);
	if (!target) return;
	float w, h;
	SDL_GetTextureSize(t, &w, &h);
	float target_size[4] = {w, h, 0.0f, 0.0f};
	// what the renderer has drawn so far goes first
	SDL_FlushRenderer(renderer);
	auto cmd = SDL_AcquireGPUCommandBuffer(device);
	SDL_PushGPUVertexUniformData(cmd, 0, target_size, sizeof(target_size));
	SDL_PushGPUFragmentUniformData(cmd, 0, block, (Uint32)size);
	SDL_GPUColorTargetInfo info = {};
	info.texture = target;
	info.load_op = SDL_GPU_LOADOP_DONT_CARE;
	info.store_op = SDL_GPU_STOREOP_STORE;
	auto pass = SDL_BeginGPURenderPass(cmd, &info, 1, nullptr);
	SDL_BindGPUGraphicsPipeline(pass, shader->pipeline);
	SDL_DrawGPUPrimitives(pass, 3, 1, 0, 0);
	SDL_EndGPURenderPass(pass);
	SDL_SubmitGPUCommandBuffer(cmd);
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
};

#endif
#endif

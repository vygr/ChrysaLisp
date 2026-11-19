#if defined(_HOST_GUI)
#if _HOST_GUI == 3

// SDL3 GUI driver. The 2D drawing is SDL's GPU renderer, on the device of
// the GPU driver, gpu_sdl3.cpp, which is lent a texture of this driver's to
// draw triangles straight into, where it is shown.

#include <SDL3/SDL.h>
#include "sdl3_load.h"
#include "gpu_sdl3.h"
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

static const SDL_PixelFormat target_pixel_format = SDL_PIXELFORMAT_ARGB8888;
static const SDL_GPUTextureFormat target_gpu_format = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM;

// a texture, as the GUI knows it. Only a glyph or a greyscale texture is
// drawn in a color, mode 1 or 2, a normal one is drawn as it is, the
// same as the drivers that do their own drawing.

struct Texture
{
	SDL_Texture *texture;
	uint64_t mode;
	// the last shader or triangle draw into this texture, if the GPU has not
	// finished it, and when it was handed over
	SDL_GPUFence *fence;
	Uint64 drawn_at;
	uint64_t made_at;
	// what triangles drawn into this texture need of their own, made the first
	// time they are, a depth buffer, and a target with 4 samples a pixel if the
	// device has that. Each texture has its own, so two of different sizes do
	// not make each other's again every frame
	SDL_GPUTexture *depth;
	SDL_GPUTexture *samples;
	Uint32 depth_w, depth_h;
};

// Who may draw on the GPU, and when. See gpu_may_draw().
static Texture *gpu_flights[32];
static int gpu_num_flights = 0;
static Texture *gpu_waiting = nullptr;
static Uint64 gpu_waiting_since = 0;
// another texture's draw that is younger than this does not hold a draw up
static const Uint64 gpu_fresh_ns = 3000000;
// and one that was refused has the next turn for this long, then loses it
static const Uint64 gpu_turn_ns = 100000000;

static void gpu_forget(Texture *tex)
{
	// a texture that is going, or has no draw on the go
	for (int i = 0; i < gpu_num_flights; ++i)
	{
		if (gpu_flights[i] == tex) gpu_flights[i--] = gpu_flights[--gpu_num_flights];
	}
}

// how many frames of the GUI have been shown. The pixels of a texture the
// renderer makes go to the GPU with the frame that is shown next, not when
// it is made, so till that has gone a shader that reads it would read
// what was in the memory before
static uint64_t gui_presents = 0;

static void *new_texture(SDL_Texture *t, uint64_t mode)
{
	if (!t) return nullptr;
	auto texture = (Texture*)SDL_malloc(sizeof(Texture));
	texture->texture = t;
	texture->mode = mode;
	texture->fence = nullptr;
	texture->drawn_at = 0;
	texture->made_at = gui_presents;
	texture->depth = nullptr;
	texture->samples = nullptr;
	texture->depth_w = texture->depth_h = 0;
	return texture;
}

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

// the GPU renderer, that lets a shader share the device the GUI is drawn
// with, came with SDL 3.4. On an older SDL3, the 3.2 of Debian 13 say, the
// driver is built without it, the GUI runs, and it can not draw a shader.
#define HOST_GUI_GPU SDL_VERSION_ATLEAST(3, 4, 0)

// the GPU device is the GPU driver's, gpu_sdl3.cpp, made with no more asked
// of it than is used, and shared, so what a kernel makes a shader can draw,
// and what the GPU driver draws can go straight into a texture shown here

// the textures a shader can be drawn into, those of host_gui_shader_texture.
// The GPU driver is given a handle by a task, and only one of these is taken
// as a texture to draw into
static void *gui_targets[256];
static int gui_num_targets = 0;

// this driver's textures lent to the GPU driver to draw into, or no longer
static void gui_lend(bool on);

static SDL_GPUDevice *create_device()
{
#if !HOST_GUI_GPU
	return nullptr;
#else
	return host_gpu_device();
#endif
}

// -hidden in the args, a desktop that is never seen: its window is not
// shown, not raised, and on a Mac it is not an app of the Dock that can be
// made the one in front. It draws as any does, into the back buffer, which
// is what is read for a grab. For a desktop of a tool, rack -g, that comes
// and goes while someone works on theirs, and must never take their input

extern char **host_argv;

static bool host_gui_hidden(void)
{
	if (host_argv) for (char **arg = host_argv; *arg; arg++) if (!strcmp(*arg, "-hidden")) return true;
	return false;
}

void host_gui_init(host_gui_rect *rect, uint64_t flags)
{
	auto hidden = host_gui_hidden();
	host_gui_sdl3_hidden = hidden;
#if defined(__APPLE__)
	// NSApplicationActivationPolicyRegular, or Accessory, no Dock icon
	set_macos_activation_policy(hidden ? 1 : 0);
#endif
#if defined(__linux__)
	// on Linux a hidden desktop is drawn offscreen and never asks for the
	// display. With no window system, a Raspberry Pi's, the display is one
	// process's at a time, the desktop someone works at has it, and with the
	// screen turned off there is none at all. SDL_VIDEO_DRIVER still says
	if (hidden) SDL_SetHint(SDL_HINT_VIDEO_DRIVER, "offscreen");
#endif
	// a trackpad gives SDL no fingers unless it is told to, before it
	// starts, and then it gives no mouse. CL_TOUCH_TRACKPAD asks for that
	if (host_gui_sdl3_trackpad()) SDL_SetHint(SDL_HINT_TRACKPAD_IS_TOUCH_ONLY, "1");
	host_gpu_quiet();
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
		SDL_WINDOW_RESIZABLE | (bare ? SDL_WINDOW_VULKAN : 0) | (hidden ? SDL_WINDOW_HIDDEN : 0));
	// the GPU renderer if there is one, so that a shader can share its device
	device = create_device();
#if HOST_GUI_GPU
	if (device) renderer = SDL_CreateGPURenderer(device, window);
#endif
	if (renderer) gui_lend(true);
	if (!renderer)
	{
		//the device is the GPU driver's, this driver just does not draw with it
		device = nullptr;
		if (bare)
		{
			// a window made for Vulkan is no use to another renderer
			SDL_DestroyWindow(window);
			window = SDL_CreateWindow("ChrysaLisp GUI Window", rect->w, rect->h,
				SDL_WINDOW_RESIZABLE | (hidden ? SDL_WINDOW_HIDDEN : 0));
		}
		renderer = SDL_CreateRenderer(window, nullptr);
	}
	SDL_SetRenderVSync(renderer, 1);
	backbuffer = create_backbuffer(rect->w, rect->h);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	if (flags) SDL_HideCursor();
#if defined(__APPLE__)
	// a finger that rests on a trackpad while another moves is taken by the
	// Mac for a hand lying on it, and is cancelled: it is seen to lift, and
	// to land again when it moves. A finger that holds a thing still while
	// another works is the very thing to be tried, so the view is told it
	// wants fingers that rest
	if (host_gui_sdl3_trackpad())
	{
		id nswindow = (id)SDL_GetPointerProperty(SDL_GetWindowProperties(window),
			SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, nullptr);
		id view = nswindow ? ((id (*)(id, SEL))objc_msgSend)(nswindow, sel_registerName("contentView")) : nullptr;
		if (view) ((void (*)(id, SEL, BOOL))objc_msgSend)(view, sel_registerName("setWantsRestingTouches:"), YES);
	}
#endif
	// a desktop started from a shell opens behind the shell's window, on a
	// Mac, the shell is the program in front. It is asked to come forward,
	// unless it is hidden
	if (hidden) return;
	SDL_RaiseWindow(window);
	// CL_FULLSCREEN asks for the desktop to have the whole screen from the
	// start, as it has when its window is told to by hand. The GUI is told
	// the new size as it is of any other
	if (SDL_getenv("CL_FULLSCREEN")) SDL_SetWindowFullscreen(window, true);
}

void host_gui_deinit()
{
	gui_lend(false);
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
		//the device is the GPU driver's, it lasts as long as the host
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
#if HOST_GUI_GPU
	if (texture->fence && device) SDL_ReleaseGPUFence(device, texture->fence);
	if (texture->depth && device) SDL_ReleaseGPUTexture(device, texture->depth);
	if (texture->samples && device) SDL_ReleaseGPUTexture(device, texture->samples);
	gpu_forget(texture);
	if (gpu_waiting == texture) gpu_waiting = nullptr;
#endif
	for (int i = 0; i < gui_num_targets; ++i)
	{
		if (gui_targets[i] == texture) gui_targets[i--] = gui_targets[--gui_num_targets];
	}
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
	++gui_presents;
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

// a texture the GPU driver can draw triangles into, and that blits as any
// other does. What it holds is not set till it is drawn into

void *host_gui_target_texture(uint64_t w, uint64_t h)
{
	if (!device) return nullptr;
	auto t = SDL_CreateTexture(renderer, target_pixel_format, SDL_TEXTUREACCESS_TARGET, (int)w, (int)h);
	if (!t) return nullptr;
	SDL_SetTextureBlendMode(t, premul_blend_mode());
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	auto texture = new_texture(t, 0);
	// one more than there is room to know of is drawn into by this driver
	// alone, not lent
	if (gui_num_targets < 256) gui_targets[gui_num_targets++] = texture;
	return texture;
}

#if HOST_GUI_GPU
static bool gpu_may_draw(Texture *tex)
{
	// May this texture have a shader or triangle draw now ?
	//
	// One draw is on the go at a time for a texture. And a draw has to wait
	// for the GPU to be done with any other texture's draw, unless that
	// draw is fresh, handed over only a moment ago. The GUI is drawn by the
	// same GPU, and a GPU can not be stopped part way through a draw, so
	// on a slow one draws that pile up stop the desktop, a Raspberry Pi 4
	// went to 2 frames a second with two apps drawing. On a fast one every
	// draw is fresh, the GPU is done with it in a millisecond or two, and
	// two apps draw with no thought for each other.
	//
	// A texture that is refused has the next turn. Without that an app that
	// asks every tick takes every gap, and one that asks half as often is
	// never drawn.
	auto now = SDL_GetTicksNS();
	if (tex->fence)
	{
		if (!SDL_QueryGPUFence(device, tex->fence)) return false;
		SDL_ReleaseGPUFence(device, tex->fence);
		tex->fence = nullptr;
		gpu_forget(tex);
	}
	bool busy = false;
	for (int i = 0; i < gpu_num_flights; ++i)
	{
		auto other = gpu_flights[i];
		if (SDL_QueryGPUFence(device, other->fence))
		{
			SDL_ReleaseGPUFence(device, other->fence);
			other->fence = nullptr;
			gpu_flights[i--] = gpu_flights[--gpu_num_flights];
		}
		else if (now - other->drawn_at > gpu_fresh_ns) busy = true;
	}
	if (gpu_waiting && now - gpu_waiting_since > gpu_turn_ns) gpu_waiting = nullptr;
	if (busy || (gpu_waiting && gpu_waiting != tex) || gpu_num_flights >= 32)
	{
		if (!gpu_waiting)
		{
			gpu_waiting = tex;
			gpu_waiting_since = now;
		}
		return false;
	}
	if (gpu_waiting == tex) gpu_waiting = nullptr;
	return true;
}

static void gpu_drawing(Texture *tex, SDL_GPUFence *fence)
{
	// a draw has been handed to the GPU
	tex->fence = fence;
	tex->drawn_at = SDL_GetTicksNS();
	if (fence) gpu_flights[gpu_num_flights++] = tex;
}

// What is lent to the GPU driver, gpu_sdl3.h: a texture of this driver's for
// its triangles to be drawn straight into, by the same rule of who may draw
// and when as a draw of this driver's own

static int gui_tris_target(void *handle, SDL_GPUTexture **color, SDL_GPUTexture **depth, SDL_GPUTexture **samples,
	Uint32 *w, Uint32 *h)
{
	bool known = false;
	for (int i = 0; !known && i < gui_num_targets; ++i) known = gui_targets[i] == handle;
	if (!device || !renderer || !known) return -1;
	auto tex = (Texture*)handle;
	if (!gpu_may_draw(tex)) return 0;
	auto target = (SDL_GPUTexture*)SDL_GetPointerProperty(SDL_GetTextureProperties(tex->texture),
		SDL_PROP_TEXTURE_GPU_TEXTURE_POINTER, nullptr);
	if (!target) return 0;
	float fw = 0, fh = 0;
	SDL_GetTextureSize(tex->texture, &fw, &fh);
	Uint32 tw = (Uint32)fw, th = (Uint32)fh;
	if (depth && (!tex->depth || tex->depth_w != tw || tex->depth_h != th))
	{
		// the depth buffer of its own, and the samples if the device has more
		// than one a pixel, as the GPU driver's pairs are built for
		if (tex->depth) SDL_ReleaseGPUTexture(device, tex->depth);
		if (tex->samples) SDL_ReleaseGPUTexture(device, tex->samples);
		tex->samples = nullptr;
		SDL_GPUTextureCreateInfo info = {};
		info.type = SDL_GPU_TEXTURETYPE_2D;
		info.format = host_gpu_depth_format();
		info.usage = SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET;
		info.width = tw;
		info.height = th;
		info.layer_count_or_depth = 1;
		info.num_levels = 1;
		info.sample_count = host_gpu_sample_count();
		tex->depth = SDL_CreateGPUTexture(device, &info);
		tex->depth_w = tw;
		tex->depth_h = th;
		if (tex->depth && info.sample_count != SDL_GPU_SAMPLECOUNT_1)
		{
			info.format = target_gpu_format;
			info.usage = SDL_GPU_TEXTUREUSAGE_COLOR_TARGET;
			tex->samples = SDL_CreateGPUTexture(device, &info);
		}
		if (!tex->depth || (info.sample_count != SDL_GPU_SAMPLECOUNT_1 && !tex->samples))
		{
			if (tex->depth) SDL_ReleaseGPUTexture(device, tex->depth);
			tex->depth = nullptr;
			return 0;
		}
	}
	// what the renderer has drawn so far goes first
	SDL_FlushRenderer(renderer);
	*color = target;
	if (depth)
	{
		*depth = tex->depth;
		*samples = tex->samples;
	}
	*w = tw;
	*h = th;
	return 1;
}

static void gui_tris_drawn(void *handle, SDL_GPUFence *fence)
{
	gpu_drawing((Texture*)handle, fence);
}

static int gui_tris_done(void *handle)
{
	for (int i = 0; i < gui_num_targets; ++i)
	{
		if (gui_targets[i] != handle) continue;
		auto tex = (Texture*)handle;
		return !device || !tex->fence || SDL_QueryGPUFence(device, tex->fence) ? 1 : 0;
	}
	return 1;
}

static const HostGpuGui gui_tris_hooks = {gui_tris_target, gui_tris_drawn, gui_tris_done};
#endif

static void gui_lend(bool on)
{
#if HOST_GUI_GPU
	host_gpu_gui(on && device ? &gui_tris_hooks : nullptr);
#else
	(void)on;
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

// copy what was last drawn, a rectangle of the back buffer, into a buffer,
// 32 bit argb, opaque

#include "video.h"

uint64_t host_gui_read_screen(uint32_t *data, int64_t x, int64_t y, uint64_t w, uint64_t h, uint64_t stride)
{
	if (!renderer || !backbuffer) return 0;
	SDL_Rect rect = {(int)x, (int)y, (int)w, (int)h};
	auto target = SDL_GetRenderTarget(renderer);
	SDL_SetRenderTarget(renderer, backbuffer);
	auto surface = SDL_RenderReadPixels(renderer, &rect);
	SDL_SetRenderTarget(renderer, target);
	bool ok = surface && (uint64_t)surface->w == w && (uint64_t)surface->h == h
		&& SDL_ConvertPixels(surface->w, surface->h, surface->format, surface->pixels, surface->pitch,
			SDL_PIXELFORMAT_ARGB8888, data, (int)stride);
	if (surface) SDL_DestroySurface(surface);
	if (ok) for (uint64_t j = 0; j < h; j++)
	{
		auto row = (uint32_t*)((uint8_t*)data + j * stride);
		for (uint64_t i = 0; i < w; i++) row[i] |= 0xff000000;
	}
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
	(void*)host_gui_target_texture,
	(void*)host_gui_read_texture,
	(void*)host_gui_read_screen,
	(void*)host_video_open,
	(void*)host_video_decode,
	(void*)host_video_frame,
	(void*)host_video_close,
};

#endif
#endif

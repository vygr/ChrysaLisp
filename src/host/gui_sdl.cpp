#if defined(_HOST_GUI)
#if _HOST_GUI == 0

#include <SDL.h>
#include "gui_sdl2_event.h"

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

SDL_Window *window = nullptr;
SDL_Renderer *renderer = nullptr;
SDL_Texture *backbuffer = nullptr;

void host_gui_init(SDL_Rect *rect, uint64_t flags)
{
#if defined(__APPLE__)
	set_macos_activation_policy(0); // NSApplicationActivationPolicyRegular
#endif
	SDL_SetMainReady();
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	window = SDL_CreateWindow("ChrysaLisp GUI Window",
				SDL_WINDOWPOS_UNDEFINED,
				SDL_WINDOWPOS_UNDEFINED,
				rect->w, rect->h,
				SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
	renderer = SDL_CreateRenderer(window, -1,
				SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE);
	backbuffer = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGB888, SDL_TEXTUREACCESS_TARGET,
				rect->w, rect->h);
	SDL_SetTextureBlendMode(backbuffer, SDL_BLENDMODE_NONE);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	if (flags) SDL_ShowCursor(SDL_DISABLE);
}

void host_gui_deinit()
{
	SDL_ShowCursor(SDL_ENABLE);
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
	return host_gui_sdl2_poll(handle);
}

void *host_gui_create_texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto surface = SDL_CreateRGBSurfaceFrom(data, w, h, 32, s, 0xff0000, 0xff00, 0xff, 0xff000000);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	auto mode = SDL_ComposeCustomBlendMode(SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD,
		SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD);
	SDL_SetTextureBlendMode(t, mode);
	SDL_FreeSurface(surface);
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

void host_gui_flush(const SDL_Rect *rect)
{
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
	SDL_RenderCopy(renderer, backbuffer, 0, 0);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	SDL_RenderPresent(renderer);
}

void host_gui_box(const SDL_Rect *rect)
{
	SDL_RenderDrawRect(renderer, rect);
}

void host_gui_filled_box(const SDL_Rect *rect)
{
	SDL_RenderFillRect(renderer, rect);
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

void host_gui_blit(void *handle, const SDL_Rect *srect, const SDL_Rect *drect)
{
	auto t = ((Texture*)handle)->texture;
	SDL_RenderCopy(renderer, t, srect, drect);
}

void host_gui_set_clip(const SDL_Rect *rect)
{
	SDL_RenderSetClipRect(renderer, rect);
}

void host_gui_resize(uint64_t w, uint64_t h)
{
	SDL_DestroyTexture(backbuffer);
	backbuffer = SDL_CreateTexture(renderer,
				SDL_PIXELFORMAT_RGB888, SDL_TEXTUREACCESS_TARGET,
				w, h);
	SDL_SetTextureBlendMode(backbuffer, SDL_BLENDMODE_NONE);
}

uint64_t host_gui_clip_put(const char *text)
{
	return SDL_SetClipboardText(text);
}

char *host_gui_clip_get()
{
	return SDL_GetClipboardText();
}

void host_gui_clip_free(char *text)
{
	SDL_free(text);
}

// this driver can not draw a shader

uint64_t host_gui_shader_format()
{
	return 0;
}

void *host_gui_shader_create(const char *vertex, uint64_t vertex_size, const char *fragment, uint64_t fragment_size)
{
	return 0;
}

void host_gui_shader_destroy(void *handle)
{
}

void *host_gui_shader_texture(uint64_t w, uint64_t h)
{
	return 0;
}

void host_gui_shader_draw(void *handle, void *texture, const void *block, uint64_t size)
{
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
	SDL_RenderCopy(renderer, t, 0, 0);
	bool ok = SDL_RenderReadPixels(renderer, 0, SDL_PIXELFORMAT_ARGB8888, data, (int)stride) == 0;
	SDL_SetRenderTarget(renderer, target);
	SDL_SetTextureBlendMode(t, blend);
	SDL_SetTextureColorMod(t, r, g, b);
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

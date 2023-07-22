#if defined(_HOST_GUI)
#if _HOST_GUI == 0

#include <SDL.h>

SDL_Window *window;
SDL_Renderer *renderer;
SDL_Texture *backbuffer;

void host_gui_init(SDL_Rect *rect, uint64_t flags)
{
	SDL_SetMainReady();
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	window = SDL_CreateWindow("ChrysaLisp GUI Window",
				SDL_WINDOWPOS_UNDEFINED,
				SDL_WINDOWPOS_UNDEFINED,
				rect->w, rect->h,
				SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
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
	SDL_DestroyWindow(window);
	SDL_Quit();
}

uint64_t host_gui_poll_event(void *handle)
{
	SDL_PumpEvents();
	return SDL_PollEvent((SDL_Event*)handle);
}

void *host_gui_create_texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto surface = SDL_CreateRGBSurfaceFrom(data, w, h, 32, s, 0xff0000, 0xff00, 0xff, 0xff000000);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	auto mode = SDL_ComposeCustomBlendMode(SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD,
		SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD);
	SDL_SetTextureBlendMode(t, mode);
	SDL_FreeSurface(surface);
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
	auto t = (SDL_Texture*)handle;
	SDL_SetTextureColorMod(t, r, g, b);
}

void host_gui_blit(void *handle, const SDL_Rect *srect, const SDL_Rect *drect)
{
	auto t = (SDL_Texture*)handle;
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
};

#endif
#endif

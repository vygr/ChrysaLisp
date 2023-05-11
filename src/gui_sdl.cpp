#ifdef _GUI
#include <SDL.h>

SDL_Window *window;
SDL_Renderer *renderer;
SDL_Texture *backbuffer;

uint64_t Init()
{
	SDL_SetMainReady();
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	window = SDL_CreateWindow("ChrysaLisp GUI Window",
				SDL_WINDOWPOS_UNDEFINED,
				SDL_WINDOWPOS_UNDEFINED,
				1280,
				960,
				SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
	renderer = SDL_CreateRenderer(window, -1,
				SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE);
	backbuffer = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGB888, SDL_TEXTUREACCESS_TARGET,
				1280, 960);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	SDL_ShowCursor(0);
	return 0;
}

void DeInit()
{
	SDL_DestroyWindow(window);
	SDL_Quit();
}

uint64_t PollEvent(SDL_Event *data)
{
	SDL_PumpEvents();
	return SDL_PollEvent(data);
}

uint64_t Upload_Texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto surface = SDL_CreateRGBSurfaceFrom(data, w, h, 32, s, 0xff0000, 0xff00, 0xff, 0xff000000);
	auto tid = SDL_CreateTextureFromSurface(renderer, surface);
	auto mode = SDL_ComposeCustomBlendMode(SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD,
		SDL_BLENDFACTOR_ONE, SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD);
	SDL_SetTextureBlendMode(tid, mode);
	SDL_FreeSurface(surface);
	return (uint64_t)tid;
}

uint64_t Begin_Composite()
{
	SDL_SetRenderTarget(renderer, backbuffer);
	return 0;
}

uint64_t End_Composite()
{
	SDL_SetRenderTarget(renderer, 0);
	SDL_RenderCopy(renderer, backbuffer, 0, 0);
	return 0;
}

uint64_t Flush()
{
	SDL_RenderPresent(renderer);
	return 0;
}

void DrawRect(const SDL_Rect *rect)
{
	SDL_RenderDrawRect(renderer, rect);
}

void FillRect(const SDL_Rect *rect)
{
	SDL_RenderFillRect(renderer, rect);
}

void SetColor(Uint8 r, Uint8 g, Uint8 b, Uint8 a)
{
	SDL_SetRenderDrawColor(renderer, r, g, b, a);
}

void Blit(SDL_Texture *t, const SDL_Rect *srect, const SDL_Rect *drect)
{
	SDL_RenderCopy(renderer, t, srect, drect);
}

void SetClip(const SDL_Rect *rect)
{
	SDL_RenderSetClipRect(renderer, rect);
}

void Resize(uint64_t w, uint64_t h)
{
	SDL_DestroyTexture(backbuffer);
	backbuffer = SDL_CreateTexture(renderer,
				SDL_PIXELFORMAT_RGB888, SDL_TEXTUREACCESS_TARGET,
				w, h);
}

void (*host_gui_funcs[]) = {
	(void*)Init,
	(void*)DeInit,
	(void*)DrawRect,
	(void*)FillRect,
	(void*)Blit,
	(void*)SetClip,
	(void*)SetColor,
	(void*)SDL_SetTextureColorMod,
	(void*)SDL_DestroyTexture,
	(void*)Upload_Texture,
	(void*)Begin_Composite,
	(void*)End_Composite,
	(void*)Flush,
	(void*)Resize,
	(void*)PollEvent,
};

#endif

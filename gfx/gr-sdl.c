/*
 * Host GUI for ChrysaLisp SDL
 */
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "gr.h"

static SDL_Window *window;
static SDL_Renderer *renderer;
static SDL_Texture *backbuffer;
static float sdlZoom = 0.5;

uint64_t Init(struct rect *r)
{
    int pixtype, pixelformat;

    pixtype = MWPF_TRUECOLORARGB;
	SDL_SetMainReady();
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) < 0) {
		printf("Can't initialize SDL\n");
		return -1;
	}

	window = SDL_CreateWindow("ChrysaLisp GUI Window",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
		r->w*sdlZoom,
        r->h*sdlZoom,
        SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
	if (!window) {
		printf("SDL: Can't create window\n");
		return -1;
	}

	renderer = SDL_CreateRenderer(window, -1, 0);
	if (!renderer) {
		printf("SDL: Can't create renderer\n");
		return -1;
	}
	/* 
	 * Set the SDL texture pixel format to match the framebuffer format
	 * to eliminate pixel conversions.
	 */
    switch (pixtype) {
    case MWPF_TRUECOLORARGB:
        pixelformat = SDL_PIXELFORMAT_ARGB8888;
        break;
    case MWPF_TRUECOLORABGR:
        pixelformat = SDL_PIXELFORMAT_ABGR8888;
        break;
    case MWPF_TRUECOLORBGR:
        pixelformat = SDL_PIXELFORMAT_RGB24;
        break;
    case MWPF_TRUECOLOR565:
        pixelformat = SDL_PIXELFORMAT_RGB565;
        break;
    case MWPF_TRUECOLOR555:
        pixelformat = SDL_PIXELFORMAT_RGB555;
        break;
    default:
        printf("SDL: Unsupported pixel format %d\n", pixtype);
        return -1;
    }

	backbuffer = SDL_CreateTexture(renderer, pixelformat, SDL_TEXTUREACCESS_STREAMING, r->w, r->h);
	if (!backbuffer) {
		printf("SDL: Can't create texture\n");
		return -1;
	}

	/* setup zoom*/
	SDL_RenderSetLogicalSize(renderer, r->w, r->h);
	SDL_RenderSetScale(renderer, sdlZoom, sdlZoom);
    //SDL_ShowCursor(SDL_DISABLE);    /* hide SDL cursor */
    SDL_PumpEvents();

    return 0;
}

void DeInit(void)
{
    SDL_DestroyWindow(window);
	SDL_Quit();
}

uint64_t Flush(const struct rect *r)
{
    /* copy backbuffer to display*/
    //SDL_SetRenderDrawColor(renderer, 0x00, 0x00, 0x00, 0x00);
    //SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, backbuffer, NULL, NULL);
    SDL_RenderPresent(renderer);
    return 0;
}

/******************** Routines only required for CL *******************/

uint64_t PollEvent(SDL_Event *data)
{
	SDL_PumpEvents();
	return SDL_PollEvent(data);
}

uint64_t Upload_Texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	SDL_Surface * surface = SDL_CreateRGBSurfaceFrom(data, w, h, 32, s, 0xff0000, 0xff00, 0xff, 0xff000000);
	SDL_Texture * tid = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_BlendMode mode = SDL_ComposeCustomBlendMode(SDL_BLENDFACTOR_ONE,
        SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, SDL_BLENDOPERATION_ADD,
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

/******************** Routines not required for CL *******************/

/* convert keycode to shift value */
static SDL_Keycode key_shift(SDL_Keycode kc)
{
    if (kc >= 'a' && kc < 'z')
        return (kc ^ 0x20);  // upper case

    switch (kc) {
        case '`':  return '~';
        case '1':  return '!';
        case '2':  return '@';
        case '3':  return '#';
        case '4':  return '$';
        case '5':  return '%';
        case '6':  return '^';
        case '7':  return '&';
        case '8':  return '*';
        case '9':  return '(';
        case '0':  return ')';
        case '-':  return '_';
        case '=':  return '+';
        case '[':  return '{';
        case ']':  return '}';
        case '\\': return '|';
        case ';':  return ':';
        case '\'': return '"';
        case ',':  return '<';
        case '.':  return '>';
        case '/':  return '?';

        default: break;
    }

    return kc;
}


static int sdl_key(Uint8 state, SDL_Keysym sym)
{
    SDL_Scancode sc = sym.scancode;
    SDL_Keycode kc = sym.sym;
    Uint16 mod = sym.mod;

    switch (sc) {
    case SDL_SCANCODE_MINUS:  kc = '-'; break;
    case SDL_SCANCODE_PERIOD: kc = '.'; break;
    case SDL_SCANCODE_SLASH:  kc = '/'; break;
    default: break;
    }

    if (kc == SDLK_LSHIFT || kc == SDLK_RSHIFT ||
        kc == SDLK_LCTRL || kc == SDLK_RCTRL)
        return 0;

    if (kc < 256 && (mod & (KMOD_SHIFT | KMOD_CAPS)))
        kc = key_shift(kc);

    if (kc < 256 && (mod & KMOD_CTRL))
        kc &= 0x1f;  // convert to control char

    if (kc == 0x007F) kc = '\b';  // convert DEL to BS
    return kc;
}

/* send pixel data to SDL */
void DrawBits(struct drawable *d, int x, int y, int width, int height)
{
    SDL_Rect r;
    if (!width) width = d->width;
    if (!height) height = d->height;
    r.x = x;
    r.y = y;
    r.w = width;
    r.h = height;

    unsigned char *pixels = d->pixels + y * d->pitch + x * (d->bpp >> 3);
    SDL_UpdateTexture(backbuffer, &r, pixels, d->pitch);
}

uint64_t WaitEvent(void *data)
{
    SDL_Event *event = (SDL_Event *)data;

    if (SDL_WaitEvent(event)) {
        switch (event->type) {
        case SDL_KEYDOWN:
            event->key.keysym.sym = sdl_key(event->key.state, event->key.keysym);
            break;
        }
        return 1;
   }
   return 0;
}

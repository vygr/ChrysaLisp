#if defined(_HOST_GUI)
#if _HOST_GUI == -1 //demo template

#include <stdint.h>
#include <memory>
#include <iostream>

typedef uint32_t pixel_t;

struct Rect
{
	int32_t x, y, w, h;
};

struct Texture
{
	int32_t w, h, s;
	pixel_t color = 0xffffff;
	pixel_t r = 0xff;
	pixel_t g = 0xff;
	pixel_t b = 0xff;
	pixel_t *data;
};

const uint32_t SCREEN_WIDTH = 1280;
const uint32_t SCREEN_HEIGHT = 960;
const uint32_t SCREEN_STRIDE = SCREEN_WIDTH * sizeof(pixel_t);

pixel_t *screen = 0;
pixel_t *backbuffer = 0;
pixel_t color_a = 0;
pixel_t color_rb = 0;
pixel_t color_g = 0;

Rect clip;

#include <SDL.h>

SDL_Window *window;
SDL_Renderer *renderer;

////////////////////////////////
// screen setup/access functions
////////////////////////////////

void host_gui_init(Rect *rect)
{
	SDL_SetMainReady();
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
	window = SDL_CreateWindow("ChrysaLisp GUI Window",
				SDL_WINDOWPOS_UNDEFINED,
				SDL_WINDOWPOS_UNDEFINED,
				SCREEN_WIDTH, SCREEN_HEIGHT,
				SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
	renderer = SDL_CreateRenderer(window, -1,
				SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
	SDL_SetRenderTarget(renderer, 0);
	SDL_ShowCursor(0);

	screen = (pixel_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
	backbuffer = (pixel_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
	rect->w = SCREEN_WIDTH;
	rect->h = SCREEN_HEIGHT;
}

void host_gui_deinit()
{
	free(screen);
	free(backbuffer);

	SDL_DestroyWindow(window);
	SDL_Quit();
}

void host_gui_resize(uint64_t w, uint64_t h)
{
}

void host_gui_begin_composite()
{
}

void host_gui_end_composite()
{
}

void host_gui_flush(const Rect *rect)
{
	//no need to clip to screen I think !
	if (rect->w < 1 || rect->h < 1) return;
	pixel_t *dst = (pixel_t*)((uint8_t*)screen +
		rect->y * SCREEN_STRIDE + rect->x * sizeof(pixel_t));
	pixel_t *src = (pixel_t*)((uint8_t*)backbuffer +
		rect->y * SCREEN_STRIDE + rect->x * sizeof(pixel_t));
	pixel_t *src_end = (pixel_t*)((uint8_t*)src +
		rect->h * SCREEN_STRIDE);
	uint32_t span = rect->w * sizeof(pixel_t);
	uint32_t stride = SCREEN_STRIDE - span;
	do
	{
		pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
		do { *dst++ = *src++; } while (src != src_end_line);
		src = (pixel_t*)((uint8_t*)src + stride);
		dst = (pixel_t*)((uint8_t*)dst + stride);
	} while (src != src_end);

	auto surface = SDL_CreateRGBSurfaceFrom(screen, SCREEN_WIDTH, SCREEN_HEIGHT, 32, SCREEN_STRIDE, 0xff0000, 0xff00, 0xff, 0xff000000);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_SetTextureBlendMode(t, SDL_BLENDMODE_NONE);
	SDL_FreeSurface(surface);
	SDL_RenderCopy(renderer, t, 0, 0);
	SDL_DestroyTexture(t);
	SDL_RenderPresent(renderer);
}

////////////////////
// texture functions
////////////////////

Texture *host_gui_create_texture(pixel_t *src, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	Texture *t = (Texture*)malloc(sizeof(Texture));
	pixel_t *dst = (pixel_t *)malloc(w * h * sizeof(pixel_t));
	t->w = w;
	t->h = h;
	t->s = w * sizeof(pixel_t);
	t->r = 0xff;
	t->g = 0xff;
	t->b = 0xff;
	t->color = 0xffffff;
	t->data = dst;
	pixel_t *src_end = (pixel_t*)((uint8_t*)src + h * s);
	uint32_t span = w * sizeof(pixel_t);
	s -= span;
	do
	{
		pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
		do { *dst++ = *src++; } while (src != src_end_line);
		src = (pixel_t*)((uint8_t*)src + s);
	} while (src != src_end);
	return t;
}

void host_gui_destroy_texture(Texture *t)
{
	free(t->data);
	free(t);
}

void host_gui_set_texture_color(Texture *t, uint8_t r, uint8_t g, uint8_t b)
{
	//convert to premultiplied channels !, fast check for == white
	t->r = r + 1;
	t->g = g + 1;
	t->b = b + 1;
	t->color = (r << 16) + (g << 8) + b;
}

////////////////////
// drawing functions
////////////////////

void host_gui_set_clip(const Rect *rect)
{
	//store as x, y, x1, y1 !
	clip = *rect;
	clip.w += clip.x;
	clip.h += clip.y;
}

void host_gui_set_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
	//convert to premultiplied channels !
	color_a = a;
	color_rb = (((r * a) & 0xff00) << 8) + ((b * a) >> 8);
	color_g = (g * a) & 0xff00;
}

void host_gui_filled_box(const Rect *rect)
{
	//clip
	Rect r = *rect;
	if (color_a == 0) return;
	if (r.w < 1 || r.h < 1) return;
	r.w += r.x;
	r.h += r.y;
	if (r.x >= clip.w || r.y >= clip.h) return;
	if (r.w <= clip.x || r.h <= clip.y) return;
	if (clip.x > r.x) r.x = clip.x;
	if (clip.y > r.y) r.y = clip.y;
	if (r.w > clip.w) r.w = clip.w;
	if (r.h > clip.h) r.h = clip.h;
	//fill the rect
	pixel_t *dst = (pixel_t*)((uint8_t*)backbuffer +
		r.y * SCREEN_STRIDE + r.x * sizeof(pixel_t));
	pixel_t *dst_end = (pixel_t*)((uint8_t*)dst +
		(r.h - r.y) * SCREEN_STRIDE);
	uint32_t span = (r.w - r.x) * sizeof(pixel_t);
	uint32_t stride = SCREEN_STRIDE - span;
	if (color_a == 0xff)
	{
		pixel_t dcol = color_rb + color_g;
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do { *dst++ = dcol; } while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + stride);
		} while (dst != dst_end);
	}
	else
	{
		pixel_t da = 0xff - color_a;
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				pixel_t drb = *dst;
				pixel_t dg = drb & 0xff00;
				drb = drb & 0xff00ff;
				drb = ((drb * da >> 8) & 0xff00ff) + color_rb;
				dg = ((dg * da >> 8) & 0xff00) + color_g;
				*dst++ = drb + dg;
			} while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + stride);
		} while (dst != dst_end);
	}
}

void host_gui_box(const Rect *rect)
{
	//just call filled box and let it do the clipping and drawing
	Rect r = *rect;
	if (rect->w < 1 || rect->h < 1) return;
	r.h = 1;
	host_gui_filled_box(&r);
	if (rect->h <= 1) return;
	r.y = rect->y + rect->h - 1;
	host_gui_filled_box(&r);
	if (rect->h <= 2) return;
	r.y = rect->y + 1;
	r.w = 1;
	r.h = rect->h - 2;
	host_gui_filled_box(&r);
	if (rect->w <= 1) return;
	r.x = rect->x + rect->w - 1;
	host_gui_filled_box(&r);
}

void host_gui_blit(Texture *t, const Rect *srect, const Rect *drect)
{
	//clip
	Rect dr = *drect;
	Rect sr = *srect;
	if (dr.w < 1 || dr.h < 1) return;
	dr.w += dr.x;
	dr.h += dr.y;
	if (dr.x >= clip.w || dr.y >= clip.h) return;
	if (dr.w <= clip.x || dr.h <= clip.y) return;
	if (clip.x > dr.x)
	{
		sr.x += clip.x - dr.x;
		dr.x = clip.x;
	}
	if (clip.y > dr.y)
	{
		sr.y += clip.y - dr.y;
		dr.y = clip.y;
	}
	if (dr.w > clip.w) dr.w = clip.w;
	if (dr.h > clip.h) dr.h = clip.h;
	//blit the rect
	pixel_t *src = (pixel_t*)((uint8_t*)t->data +
		sr.y * t->s + sr.x * sizeof(pixel_t));
	pixel_t *dst = (pixel_t*)((uint8_t*)backbuffer +
		dr.y * SCREEN_STRIDE + dr.x * sizeof(pixel_t));
	pixel_t *dst_end = (pixel_t*)((uint8_t*)dst +
		(dr.h - dr.y) * SCREEN_STRIDE);
	uint32_t span = (dr.w - dr.x) * sizeof(pixel_t);
	uint32_t dstride = SCREEN_STRIDE - span;
	uint32_t sstride = t->s - span;
	if (t->color == 0xffffff)
	{
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				pixel_t sa = *src++;
				if (sa > 0xffffff)
				{
					if (sa < 0xff000000)
					{
						pixel_t srb = sa & 0xff00ff;
						pixel_t sg = sa & 0xff00;
						sa = sa >> 24;
						pixel_t drb = *dst;
						pixel_t dg = drb & 0xff00;
						drb = drb & 0xff00ff;
						pixel_t da = 0xff - sa;
						drb = ((drb * da >> 8) & 0xff00ff) + srb;
						dg = ((dg * da >> 8) & 0xff00) + sg;
						*dst = drb + dg;
					}
					else
					{
						*dst = sa & 0xffffff;
					}
				}
				dst++;
			} while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + dstride);
			src = (pixel_t*)((uint8_t*)src + sstride);
		} while (dst != dst_end);
	}
	else
	{
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				pixel_t sa = *src++;
				if (sa > 0xffffff)
				{
					pixel_t sr = sa & 0xff0000;
					pixel_t sg = sa & 0xff00;
					pixel_t sb = sa & 0xff;
					sr = (sr * t->r >> 8) & 0xff0000;
					sg = (sg * t->g >> 8) & 0xff00;
					sb = sb * t->b >> 8;
					if (sa < 0xff000000)
					{
						pixel_t da = 0xff - (sa >> 24);
						pixel_t drb = *dst;
						pixel_t dg = drb & 0xff00;
						drb = drb & 0xff00ff;
						drb = ((drb * da >> 8) & 0xff00ff) + sr + sb;
						dg = ((dg * da >> 8) & 0xff00) + sg;
						*dst = drb + dg;
					}
					else
					{
						*dst = sr + sg + sb;
					}
				}
				dst++;
			} while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + dstride);
			src = (pixel_t*)((uint8_t*)src + sstride);
		} while (dst != dst_end);
	}
}

//////////////////
// event functions
//////////////////

uint64_t host_gui_poll_event(uint64_t data)
{
	SDL_PumpEvents();
	return SDL_PollEvent((SDL_Event*)data);
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

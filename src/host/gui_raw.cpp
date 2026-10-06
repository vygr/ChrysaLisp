#if defined(_HOST_GUI)
#if _HOST_GUI == 2 || _HOST_GUI == 4

// raw GUI driver. All the drawing is done here, into a buffer, SDL only
// puts the buffer in a window and gives the events. _HOST_GUI 2 is on
// SDL2, _HOST_GUI 4 is on SDL3.

#include <stdint.h>
#include <memory>
#include <stdio.h>
#include <stdlib.h>

typedef uint32_t pixel_t;
typedef uint8_t alpha_t;

struct Texture
{
	int32_t w, h, s, m;
	pixel_t rb = ((0xff + 1) << 16) + (0xff + 1);
	pixel_t g = (0xff + 1) << 8;
	pixel_t data[0];
};

pixel_t *screen = 0;
pixel_t *backbuffer = 0;
pixel_t color_a = 0;
pixel_t color_rb = 0;
pixel_t color_g = 0;
uint32_t scr_width = 0;
uint32_t scr_height = 0;
uint32_t scr_stride = 0;

//this code is just so we can see the output !
#if _HOST_GUI == 4
#include <SDL3/SDL.h>
#include "gui_sdl3_event.h"
#else
#include <SDL.h>
#include "gui_sdl2_event.h"
#endif
SDL_Window *window;
SDL_Renderer *renderer;

#ifdef __APPLE__
#include <objc/message.h>
#include <objc/runtime.h>

static void set_macos_activation_policy(int policy = 0)
{
	id app = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSApplication"), sel_registerName("sharedApplication"));
	((void (*)(id, SEL, long))objc_msgSend)(app, sel_registerName("setActivationPolicy:"), policy);
	if (policy == 0)
	{
		((void (*)(id, SEL, bool))objc_msgSend)(app, sel_registerName("activateIgnoringOtherApps:"), true);
	}
}
#endif

////////////////////////////////
// screen setup/access functions
////////////////////////////////

host_gui_rect clip;

void host_gui_resize(uint64_t w, uint64_t h)
{
	scr_width = w;
	scr_height = h;
	scr_stride = w * sizeof(pixel_t);
	free(screen);
	free(backbuffer);
	screen = (pixel_t *)malloc(scr_height * scr_stride);
	backbuffer = (pixel_t *)malloc(scr_height * scr_stride);
}

void host_gui_init(host_gui_rect *rect, uint64_t flags)
{
	host_gui_resize(rect->w, rect->h);

	//this code is just so we can see the output !
#if _HOST_GUI == 4
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
#ifdef __APPLE__
	set_macos_activation_policy();
#endif
	window = SDL_CreateWindow("ChrysaLisp GUI Window", scr_width, scr_height, SDL_WINDOW_RESIZABLE);
	renderer = SDL_CreateRenderer(window, nullptr);
	SDL_SetRenderVSync(renderer, 1);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
	if (flags) SDL_HideCursor();
#else
	SDL_SetMainReady();
	SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
#ifdef __APPLE__
	set_macos_activation_policy();
#endif
	window = SDL_CreateWindow("ChrysaLisp GUI Window",
				SDL_WINDOWPOS_UNDEFINED,
				SDL_WINDOWPOS_UNDEFINED,
				scr_width, scr_height,
				SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
	renderer = SDL_CreateRenderer(window, -1,
				SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_TARGETTEXTURE);
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
	SDL_SetRenderTarget(renderer, 0);
	if (flags) SDL_ShowCursor(SDL_DISABLE);
#endif
}

void host_gui_deinit()
{
	free(screen);
	free(backbuffer);
	scr_width = 0;
	scr_height = 0;
	scr_stride = 0;
	screen = 0;
	backbuffer = 0;

	//this code is just so we can see the output !
#if _HOST_GUI == 4
	SDL_ShowCursor();
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_QuitSubSystem(SDL_INIT_VIDEO | SDL_INIT_EVENTS);
#else
	SDL_ShowCursor(SDL_ENABLE);
	SDL_DestroyWindow(window);
	SDL_Quit();
#endif
#ifdef __APPLE__
	set_macos_activation_policy(2);
#endif
}

void host_gui_begin_composite()
{
}

void host_gui_end_composite()
{
}

void host_gui_flush(const host_gui_rect *rect)
{
	//no need to clip to screen
	if (rect->w <= 0 || rect->h <= 0) return;
	pixel_t *dst = (pixel_t*)((uint8_t*)screen +
		rect->y * scr_stride + rect->x * sizeof(pixel_t));
	pixel_t *src = (pixel_t*)((uint8_t*)backbuffer +
		rect->y * scr_stride + rect->x * sizeof(pixel_t));
	pixel_t *src_end = (pixel_t*)((uint8_t*)src +
		rect->h * scr_stride);
	uint32_t span = rect->w * sizeof(pixel_t);
	uint32_t stride = scr_stride - span;
	do
	{
		pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
		do { *dst++ = *src++; } while (src != src_end_line);
		src = (pixel_t*)((uint8_t*)src + stride);
		dst = (pixel_t*)((uint8_t*)dst + stride);
	} while (src != src_end);

	//this code is just so we can see the output !
#if _HOST_GUI == 4
	auto surface = SDL_CreateSurfaceFrom(scr_width, scr_height, SDL_PIXELFORMAT_ARGB8888, screen, scr_stride);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_SetTextureBlendMode(t, SDL_BLENDMODE_NONE);
	SDL_SetTextureScaleMode(t, SDL_SCALEMODE_NEAREST);
	SDL_DestroySurface(surface);
	SDL_RenderTexture(renderer, t, 0, 0);
	SDL_DestroyTexture(t);
	SDL_RenderPresent(renderer);
#else
	auto surface = SDL_CreateRGBSurfaceFrom(screen, scr_width, scr_height, 32, scr_stride, 0xff0000, 0xff00, 0xff, 0xff000000);
	auto t = SDL_CreateTextureFromSurface(renderer, surface);
	SDL_SetTextureBlendMode(t, SDL_BLENDMODE_NONE);
	SDL_FreeSurface(surface);
	SDL_RenderCopy(renderer, t, 0, 0);
	SDL_DestroyTexture(t);
	SDL_RenderPresent(renderer);
#endif
}

////////////////////
// texture functions
////////////////////

void *host_gui_create_texture(pixel_t *src, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	auto tt = (m == 1) ? sizeof(alpha_t) : (m == 2 ? 2 : sizeof(pixel_t));
	Texture *t = (Texture*)malloc(sizeof(Texture) + w * h * tt);
	t->w = (int32_t)w;
	t->h = (int32_t)h;
	t->s = (int32_t)(w * tt);
	t->m = (int32_t)m;
	t->rb = ((0xff + 1) << 16) + (0xff + 1);
	t->g = (0xff + 1) << 8;
	pixel_t *src_end = (pixel_t*)((uint8_t*)src + h * s);
	uint32_t span = (uint32_t)(w * sizeof(pixel_t));
	s -= span;
	if (m == 1)
	{
		// glyph mode texture
		alpha_t *dst = (alpha_t*)t->data;
		do
		{
			pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
			do { *dst++ = *src++ >> 24; } while (src != src_end_line);
			src = (pixel_t*)((uint8_t*)src + s);
		} while (src != src_end);
	}
	else if (m == 2)
	{
		// colorized alpha blended mode
		uint16_t *dst = (uint16_t*)t->data;
		do
		{
			pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
			do {
				pixel_t p = *src++;
				*dst++ = (uint16_t)(((p >> 16) & 0xff00) | ((p >> 16) & 0xff));
			} while (src != src_end_line);
			src = (pixel_t*)((uint8_t*)src + s);
		} while (src != src_end);
	}
	else
	{
		// normal mode texture
		pixel_t *dst = (pixel_t*)t->data;
		do
		{
			pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
			do { *dst++ = *src++; } while (src != src_end_line);
			src = (pixel_t*)((uint8_t*)src + s);
		} while (src != src_end);
	}
	return t;
}

void host_gui_destroy_texture(void *handle)
{
	auto t = (Texture*)handle;
	free(t);
}

void host_gui_set_texture_color(void *handle, uint8_t r, uint8_t g, uint8_t b)
{
	//convert to premultiplied channels !
	auto t = (Texture*)handle;
	t->rb = ((r + 1) << 16) + (b + 1);
	t->g = (g + 1) << 8;
}

////////////////////
// drawing functions
////////////////////

void host_gui_set_clip(const host_gui_rect *rect)
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
	color_rb = ((((r + 1) * a) & 0xff00) << 8) + (((b + 1) * a) >> 8);
	color_g = ((g + 1) * a) & 0xff00;
}

void host_gui_filled_box(const host_gui_rect *rect)
{
	//clip
	host_gui_rect r = *rect;
	if (color_a == 0) return;
	if (r.w <= 0 || r.h <= 0) return;
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
		r.y * scr_stride + r.x * sizeof(pixel_t));
	pixel_t *dst_end = (pixel_t*)((uint8_t*)dst +
		(r.h - r.y) * scr_stride);
	uint32_t span = (r.w - r.x) * sizeof(pixel_t);
	uint32_t stride = scr_stride - span;
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

void host_gui_box(const host_gui_rect *rect)
{
	//just call filled box and let it do the clipping and drawing
	host_gui_rect r = *rect;
	if (rect->w <= 0 || rect->h <= 0) return;
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

void host_gui_blit(void *handle, const host_gui_rect *srect, const host_gui_rect *drect)
{
	auto t = (Texture*)handle;
	//clip
	host_gui_rect dr = *drect;
	host_gui_rect sr = *srect;
	if (dr.w <= 0 || dr.h <= 0) return;
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
	pixel_t *dst = (pixel_t*)((uint8_t*)backbuffer +
		dr.y * scr_stride + dr.x * sizeof(pixel_t));
	pixel_t *dst_end = (pixel_t*)((uint8_t*)dst +
		(dr.h - dr.y) * scr_stride);
	if (t->m == 1)
	{
		//texture mode 1 ie. glyph mode
		alpha_t *src = (alpha_t*)((uint8_t*)t->data +
			sr.y * t->s + sr.x * sizeof(alpha_t));
		uint32_t span = (dr.w - dr.x);
		uint32_t sstride = t->s - span * sizeof(alpha_t);
		span *= sizeof(pixel_t);
		uint32_t dstride = scr_stride - span;
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				alpha_t sa = *src++;
				if (sa != 0)
				{
					pixel_t sg = ((sa * t->g) >> 8) & 0xff00;
					pixel_t srb = ((sa * t->rb) >> 8) & 0xff00ff;
					if (sa != 0xff)
					{
						pixel_t da = 0xff - sa;
						pixel_t drb = *dst;
						pixel_t dg = drb & 0xff00;
						drb = drb & 0xff00ff;
						drb = ((drb * da >> 8) & 0xff00ff) + srb;
						dg = ((dg * da >> 8) & 0xff00) + sg;
						*dst = drb + dg;
					}
					else
					{
						*dst = srb + sg;
					}
				}
				dst++;
			} while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + dstride);
			src = (alpha_t*)((uint8_t*)src + sstride);
		} while (dst != dst_end);
	}
	else if (t->m == 2)
	{
		//texture mode 2 ie. colorized alpha blended mode
		uint16_t *src = (uint16_t*)((uint8_t*)t->data +
			sr.y * t->s + sr.x * 2);
		uint32_t span = (dr.w - dr.x);
		uint32_t sstride = t->s - span * 2;
		span *= sizeof(pixel_t);
		uint32_t dstride = scr_stride - span;
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				uint16_t sca = *src++;
				alpha_t sa = (alpha_t)(sca >> 8);
				if (sa != 0)
				{
					uint32_t tc = sca & 0xff;
					uint32_t combined_a = (sa * (tc + 1)) >> 8;
					pixel_t sg = ((combined_a * t->g) >> 8) & 0xff00;
					pixel_t srb = ((combined_a * t->rb) >> 8) & 0xff00ff;
					if (sa != 0xff)
					{
						pixel_t da = 0xff - sa;
						pixel_t drb = *dst;
						pixel_t dg = drb & 0xff00;
						drb = drb & 0xff00ff;
						drb = ((drb * da >> 8) & 0xff00ff) + srb;
						dg = ((dg * da >> 8) & 0xff00) + sg;
						*dst = drb + dg;
					}
					else
					{
						*dst = srb + sg;
					}
				}
				dst++;
			} while (dst != dst_end_line);
			dst = (pixel_t*)((uint8_t*)dst + dstride);
			src = (uint16_t*)((uint8_t*)src + sstride);
		} while (dst != dst_end);
	}
	else
	{
		//texture mode 0 ie. normal mode
		pixel_t *src = (pixel_t*)((uint8_t*)t->data +
			sr.y * t->s + sr.x * sizeof(pixel_t));
		uint32_t span = (dr.w - dr.x) * sizeof(pixel_t);
		uint32_t dstride = scr_stride - span;
		uint32_t sstride = t->s - span;
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
						pixel_t drb = *dst;
						pixel_t dg = drb & 0xff00;
						drb = drb & 0xff00ff;
						pixel_t da = 0xff - (sa >> 24);
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
}

//////////////////
// event functions
//////////////////

uint64_t host_gui_poll_event(void *handle)
{
#if _HOST_GUI == 4
	return host_gui_sdl3_poll(handle);
#else
	return host_gui_sdl2_poll(handle);
#endif
}

///////////////////////////
// clipboard text functions
///////////////////////////

uint64_t host_gui_clip_put(const char *text)
{
#if _HOST_GUI == 4
	return SDL_SetClipboardText(text) ? 0 : -1;
#else
	return SDL_SetClipboardText(text);
#endif
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

// copy a texture into a buffer, 32 bit premultiplied argb. A glyph texture
// has only alpha, and comes back as white, a greyscale one as its grey.

uint64_t host_gui_read_texture(void *handle, uint32_t *data, uint64_t w, uint64_t h, uint64_t stride)
{
	auto t = (Texture*)handle;
	if (!t || (uint64_t)t->w != w || (uint64_t)t->h != h) return 0;
	for (uint64_t y = 0; y < h; y++)
	{
		const uint8_t *src = (const uint8_t*)t->data + y * t->s;
		uint32_t *dst = (uint32_t*)((uint8_t*)data + y * stride);
		for (uint64_t x = 0; x < w; x++)
		{
			if (t->m == 1)
			{
				uint32_t a = src[x];
				dst[x] = (a << 24) | (a << 16) | (a << 8) | a;
			}
			else if (t->m == 2)
			{
				uint32_t p = ((const uint16_t*)src)[x];
				uint32_t a = p >> 8, c = p & 0xff;
				dst[x] = (a << 24) | (c << 16) | (c << 8) | c;
			}
			else dst[x] = ((const uint32_t*)src)[x];
		}
	}
	return 1;
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
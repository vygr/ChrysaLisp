#if defined(_HOST_GUI)
#if _HOST_GUI == 1

#include <stdint.h>
#include <memory>

struct Rect
{
	int32_t x, y, w, h;
};

typedef uint32_t pixel_t;

const uint32_t SCREEN_WIDTH = 1024;
const uint32_t SCREEN_HEIGHT = 768;
const uint32_t SCREEN_STRIDE = SCREEN_WIDTH * sizeof(pixel_t);

pixel_t *screen = 0;
pixel_t *backbuffer = 0;
pixel_t color_a = 0;
pixel_t color_r = 0;
pixel_t color_g = 0;
pixel_t color_b = 0;

Rect clip;

void host_gui_init(Rect *rect)
{
	screen = (pixel_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
	backbuffer = (pixel_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
	rect->w = SCREEN_WIDTH;
	rect->h = SCREEN_HEIGHT;
}

void host_gui_deinit()
{
	free(screen);
	free(backbuffer);
}

uint64_t host_gui_poll_event(uint64_t data)
{
	return 0;
}

uint64_t host_gui_create_texture(pixel_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
	return 0;
}

void host_gui_destroy_texture(uint64_t t)
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
		(rect->y * SCREEN_STRIDE + rect->x * sizeof(pixel_t)));
	pixel_t *src = (pixel_t*)((uint8_t*)backbuffer +
		(rect->y * SCREEN_STRIDE + rect->x * sizeof(pixel_t)));
	pixel_t *src_end = (pixel_t*)((uint8_t*)src +
		rect->h * SCREEN_STRIDE);
	uint32_t span = rect->w * sizeof(pixel_t);
	uint32_t stride = SCREEN_STRIDE - span;
	do
	{
		pixel_t *src_end_line = (pixel_t*)((uint8_t*)src + span);
		do
		{
			*dst++ = *src++;
		} while (src != src_end_line);
		src += stride;
		dst += stride;
	} while (src != src_end);
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
		(r.y * SCREEN_STRIDE + r.x * sizeof(pixel_t)));
	pixel_t *dst_end = (pixel_t*)((uint8_t*)dst +
		(r.h - r.y) * SCREEN_STRIDE);
	uint32_t span = (r.w - r.x) * sizeof(pixel_t);
	uint32_t stride = SCREEN_STRIDE - span;
	if (color_a == 0xff)
	{
		pixel_t dcol = color_r + color_g + color_b;
		do
		{
			pixel_t *dst_end_line = (pixel_t*)((uint8_t*)dst + span);
			do
			{
				*dst++ = dcol;
			} while (dst != dst_end_line);
			dst += stride;
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
				pixel_t dr = *dst;
				pixel_t dg = dr & 0xff00;
				pixel_t db = dr & 0xff;
				dr = dr & 0xff0000;
				dr = ((dr * da >> 8) & 0xff0000) + color_r;
				dg = ((dg * da >> 8) & 0xff00) + color_g;
				db = (db * da >> 8) + color_b;
				*dst++ = dr + dg + db;
			} while (dst != dst_end_line);
			dst += stride;
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

void host_gui_set_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
	//convert to premultiplied channels !
	color_a = a;
	color_r = ((r * a) & 0xff00) << 8;
	color_g = (g * a) & 0xff00;
	color_b = (b * a) >> 8;
}

void host_gui_set_texture_color(uint64_t t, uint8_t r, uint8_t g, uint8_t b)
{
}

void host_gui_blit(uint64_t t, const Rect *srect, const Rect *drect)
{
}

void host_gui_set_clip(const Rect *rect)
{
	//store as x, y, x1, y1 !
	clip = *rect;
	clip.w += clip.x;
	clip.h += clip.y;
}

void host_gui_resize(uint64_t w, uint64_t h)
{
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

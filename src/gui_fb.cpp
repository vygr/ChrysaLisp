#if defined(_HOST_GUI)
#if _HOST_GUI == 1

#include <stdint.h>
#include <memory>

struct Rect
{
	int32_t x, y, w, h;
};

const uint32_t SCREEN_WIDTH = 1024;
const uint32_t SCREEN_HEIGHT = 768;
const uint32_t SCREEN_STRIDE = SCREEN_WIDTH * sizeof(uint32_t);

uint32_t *screen = 0;
uint32_t *backbuffer = 0;
uint32_t color_a = 0;
uint32_t color_r = 0;
uint32_t color_g = 0;
uint32_t color_b = 0;

Rect clip;

void host_gui_init(Rect *rect)
{
	screen = (uint32_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
	backbuffer = (uint32_t *)malloc(SCREEN_HEIGHT * SCREEN_STRIDE);
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

uint64_t host_gui_create_texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
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
	//no need to clip ! I think ...
	uint32_t *dst = (uint32_t*)((uint8_t*)screen +
		(rect->y * SCREEN_STRIDE + rect->x * sizeof(uint32_t)));
	uint32_t *src = (uint32_t*)((uint8_t*)backbuffer +
		(rect->y * SCREEN_STRIDE + rect->x * sizeof(uint32_t)));
	uint32_t stride = (SCREEN_STRIDE - rect->w * sizeof(uint32_t));
	for (uint32_t y = 0 ; y < rect->h; y++)
	{
		for (uint32_t x = 0 ; x < rect->w; x++)
		{
			*dst++ = *src++;
		}
		src += stride;
		dst += stride;
	}
}

void host_gui_filled_box(const Rect *rect)
{
	Rect r = *rect;
	if (color_a == 0) return;
	if (r.w < 1 || r.h < 1) return;
	uint32_t *dst = (uint32_t*)((uint8_t*)backbuffer +
		(r.y * SCREEN_STRIDE + r.x * sizeof(uint32_t)));
	uint32_t stride = (SCREEN_STRIDE - r.w * sizeof(uint32_t));
	if (color_a == 0xff)
	{
		uint32_t dcol = color_r + color_g + color_b;
		for (uint32_t y = 0 ; y < r.h; y++)
		{
			for (uint32_t x = 0 ; x < r.w; x++)
			{
				*dst++ = dcol;
			}
			dst += stride;
		}
	}
	else
	{
		uint32_t da = 0xff - color_a;
		for (uint32_t y = 0 ; y < r.h; y++)
		{
			for (uint32_t x = 0 ; x < r.w; x++)
			{
				uint32_t dr = *dst;
				uint32_t dg = dr & 0xff00;
				uint32_t db = dr & 0xff;
				dr = dr & 0xff0000;
				dr = (dr * da >> 8) + color_r;
				dg = (dg * da >> 8) + color_g;
				db = (db * da >> 8) + color_b;
				*dst++ = dr + dg + db;
			}
			dst += stride;
		}
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
	clip = *rect;
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

#if defined(_HOST_GUI) && _HOST_GUI == 1
#include <stdint.h>

struct Rect
{
	int x, y, w, h;
};

void host_gui_init(Rect *rect)
{
}

void host_gui_deinit()
{
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
}

void host_gui_box(const Rect *rect)
{
}

void host_gui_filled_box(const Rect *rect)
{
}

void host_gui_set_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
}

void host_gui_set_texture_color(uint64_t t, uint8_t r, uint8_t g, uint8_t b)
{
}

void host_gui_blit(uint64_t t, const Rect *srect, const Rect *drect)
{
}

void host_gui_set_clip(const Rect *rect)
{
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

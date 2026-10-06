/* gui_sdl3_event.h - the events of SDL3 as the events of a GUI driver, include after SDL3/SDL.h */

#ifndef GUI_SDL3_EVENT_H
#define GUI_SDL3_EVENT_H

#include <string.h>
#include "gui_event.h"

/* the next event the GUI service takes, not every SDL event is one */

static bool host_gui_sdl3_next(host_gui_event *out)
{
	SDL_Event e;
	while (SDL_PollEvent(&e))
	{
		memset(out, 0, sizeof(host_gui_event));
		switch (e.type)
		{
		case SDL_EVENT_QUIT:
			out->type = host_gui_event_quit;
			return true;
		case SDL_EVENT_WINDOW_RESIZED:
			out->type = host_gui_event_resized;
			out->x = e.window.data1;
			out->y = e.window.data2;
			return true;
		case SDL_EVENT_WINDOW_SHOWN:
		case SDL_EVENT_WINDOW_RESTORED:
			out->type = host_gui_event_shown;
			return true;
		case SDL_EVENT_KEY_DOWN:
		case SDL_EVENT_KEY_UP:
			out->type = e.type == SDL_EVENT_KEY_DOWN ? host_gui_event_key_down : host_gui_event_key_up;
			out->scode = e.key.scancode;
			return true;
		case SDL_EVENT_MOUSE_MOTION:
			out->type = host_gui_event_mouse_motion;
			out->x = (int32_t)e.motion.x;
			out->y = (int32_t)e.motion.y;
			out->buttons = e.motion.state;
			return true;
		case SDL_EVENT_MOUSE_BUTTON_DOWN:
		case SDL_EVENT_MOUSE_BUTTON_UP:
			out->type = e.type == SDL_EVENT_MOUSE_BUTTON_DOWN ? host_gui_event_mouse_down : host_gui_event_mouse_up;
			out->x = (int32_t)e.button.x;
			out->y = (int32_t)e.button.y;
			out->buttons = e.button.button;
			out->count = e.button.clicks;
			return true;
		case SDL_EVENT_MOUSE_WHEEL:
			out->type = host_gui_event_mouse_wheel;
			out->x = e.wheel.integer_x;
			out->y = e.wheel.integer_y;
			out->direction = e.wheel.direction;
			// a wheel can turn less than a whole step
			if (out->x || out->y) return true;
			break;
		default:
			break;
		}
	}
	return false;
}

/* with no handle the question is only if there is an event, and it
   is kept for the call that takes it */

static uint64_t host_gui_sdl3_poll(void *handle)
{
	static host_gui_event pending;
	static bool have_pending = false;
	SDL_PumpEvents();
	if (!have_pending) have_pending = host_gui_sdl3_next(&pending);
	if (!have_pending) return 0;
	if (handle)
	{
		memcpy(handle, &pending, sizeof(host_gui_event));
		have_pending = false;
	}
	return 1;
}

#endif

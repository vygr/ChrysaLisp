/* gui_sdl2_event.h - the events of SDL2 as the events of a GUI driver, include after SDL.h */

#ifndef GUI_SDL2_EVENT_H
#define GUI_SDL2_EVENT_H

#include <string.h>
#include "gui_event.h"

/* the next event the GUI service takes, not every SDL event is one */

static bool host_gui_sdl2_next(host_gui_event *out)
{
	SDL_Event e;
	while (SDL_PollEvent(&e))
	{
		memset(out, 0, sizeof(host_gui_event));
		switch (e.type)
		{
		case SDL_QUIT:
			out->type = host_gui_event_quit;
			return true;
		case SDL_WINDOWEVENT:
			if (e.window.event == SDL_WINDOWEVENT_SIZE_CHANGED)
			{
				out->type = host_gui_event_resized;
				out->x = e.window.data1;
				out->y = e.window.data2;
				return true;
			}
			if (e.window.event == SDL_WINDOWEVENT_SHOWN || e.window.event == SDL_WINDOWEVENT_RESTORED)
			{
				out->type = host_gui_event_shown;
				return true;
			}
			break;
		case SDL_KEYDOWN:
		case SDL_KEYUP:
			out->type = e.type == SDL_KEYDOWN ? host_gui_event_key_down : host_gui_event_key_up;
			out->scode = e.key.keysym.scancode;
			return true;
		case SDL_MOUSEMOTION:
			out->type = host_gui_event_mouse_motion;
			out->x = e.motion.x;
			out->y = e.motion.y;
			out->buttons = e.motion.state;
			return true;
		case SDL_MOUSEBUTTONDOWN:
		case SDL_MOUSEBUTTONUP:
			out->type = e.type == SDL_MOUSEBUTTONDOWN ? host_gui_event_mouse_down : host_gui_event_mouse_up;
			out->x = e.button.x;
			out->y = e.button.y;
			out->buttons = e.button.button;
			out->count = e.button.clicks;
			return true;
		case SDL_MOUSEWHEEL:
			out->type = host_gui_event_mouse_wheel;
			out->x = e.wheel.x;
			out->y = e.wheel.y;
			out->direction = e.wheel.direction;
			return true;
		default:
			break;
		}
	}
	return false;
}

/* with no handle the question is only if there is an event, and it
   is kept for the call that takes it */

static uint64_t host_gui_sdl2_poll(void *handle)
{
	static host_gui_event pending;
	static bool have_pending = false;
	SDL_PumpEvents();
	if (!have_pending) have_pending = host_gui_sdl2_next(&pending);
	if (!have_pending) return 0;
	if (handle)
	{
		memcpy(handle, &pending, sizeof(host_gui_event));
		have_pending = false;
	}
	return 1;
}

#endif

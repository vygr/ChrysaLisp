/* gui_sdl3_event.h - the events of SDL3 as the events of a GUI driver, include after SDL3/SDL.h */

#ifndef GUI_SDL3_EVENT_H
#define GUI_SDL3_EVENT_H

#include <string.h>
#include <stdlib.h>
#include "gui_event.h"

/* which way a wheel scrolls. On a desktop the window system has turned the
   wheel the way its user set it before SDL sees it. On a bare display there
   is nobody to do that, and the driver sets this to -1, so a wheel moves
   what is under it the way the hand moves, as on a Mac. */

static int32_t host_gui_sdl3_wheel = 1;

/* A finger on a screen is a pointer. A finger on a trackpad is not, it moves
   the mouse, and its place on the pad is not a place in the window. With
   CL_TOUCH_TRACKPAD set in the environment it is taken as one all the same,
   the pad is the window, to try many fingers on a machine with no touch
   screen. */

static bool host_gui_sdl3_trackpad(void)
{
	static int asked = -1;
	if (asked < 0) asked = getenv("CL_TOUCH_TRACKPAD") != NULL;
	return asked != 0;
}

/* how hard each pen is pressed. It is told on its own, as an axis of the
   pen, and not with where the pen is, so the last of it is kept */

#define HOST_GUI_SDL3_PENS 8
static struct { uint32_t id; uint32_t pressure; } host_gui_sdl3_pens[HOST_GUI_SDL3_PENS];

static uint32_t *host_gui_sdl3_pen(uint32_t id)
{
	int spare = 0;
	for (int i = 0; i < HOST_GUI_SDL3_PENS; i++)
	{
		if (host_gui_sdl3_pens[i].id == id) return &host_gui_sdl3_pens[i].pressure;
		if (!host_gui_sdl3_pens[i].id) spare = i;
	}
	host_gui_sdl3_pens[spare].id = id;
	host_gui_sdl3_pens[spare].pressure = 0;
	return &host_gui_sdl3_pens[spare].pressure;
}

/* a pen as a pointer event: where it is, what it is and what it has held */

static void host_gui_sdl3_pen_event(host_gui_event *out, uint32_t type, uint32_t which,
	uint32_t state, float x, float y)
{
	uint32_t id = 0x20000u + (which & 0xffffu);
	uint32_t *pressure = host_gui_sdl3_pen(id);
	out->type = type;
	out->x = (int32_t)x;
	out->y = (int32_t)y;
	out->id = id;
	out->kind = (state & SDL_PEN_INPUT_ERASER_TIP) ? host_gui_kind_eraser : host_gui_kind_pen;
	/* only a pen that touches has buttons held. A button of its barrel
	   makes it the right or the middle button and not the left */
	out->buttons = !(state & SDL_PEN_INPUT_DOWN) ? 0
		: (state & SDL_PEN_INPUT_BUTTON_1) ? host_gui_buttons_right
		: (state & SDL_PEN_INPUT_BUTTON_2) ? host_gui_buttons_middle
		: host_gui_buttons_left;
	/* a pen that does not say how hard is pressed as hard as can be */
	out->pressure = out->buttons ? (*pressure ? *pressure : 65535u) : 0;
}

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
		case SDL_EVENT_FINGER_DOWN:
		case SDL_EVENT_FINGER_MOTION:
		case SDL_EVENT_FINGER_UP:
		case SDL_EVENT_FINGER_CANCELED:
		{
			/* not a finger that SDL made out of a pen or the mouse */
			if (e.tfinger.touchID == SDL_PEN_TOUCHID || e.tfinger.touchID == SDL_MOUSE_TOUCHID) break;
			if (SDL_GetTouchDeviceType(e.tfinger.touchID) != SDL_TOUCH_DEVICE_DIRECT
				&& !host_gui_sdl3_trackpad()) break;
			/* where it is comes as a part of the window, 0 to 1 */
			SDL_Window *window = SDL_GetWindowFromID(e.tfinger.windowID);
			if (!window) window = SDL_GetKeyboardFocus();
			if (!window) break;
			int w = 0, h = 0;
			SDL_GetWindowSize(window, &w, &h);
			bool up = e.type == SDL_EVENT_FINGER_UP || e.type == SDL_EVENT_FINGER_CANCELED;
			out->type = e.type == SDL_EVENT_FINGER_DOWN ? host_gui_event_pointer_down
				: up ? host_gui_event_pointer_up : host_gui_event_pointer_motion;
			out->x = (int32_t)(e.tfinger.x * (float)w);
			out->y = (int32_t)(e.tfinger.y * (float)h);
			out->id = 0x10000u + (uint32_t)(e.tfinger.fingerID & 0xffffu);
			out->kind = host_gui_kind_touch;
			out->buttons = up ? 0 : host_gui_buttons_left;
			out->pressure = up ? 0 : e.tfinger.pressure > 0.0f ? (uint32_t)(e.tfinger.pressure * 65535.0f) : 65535u;
			return true;
		}
		case SDL_EVENT_PEN_DOWN:
		case SDL_EVENT_PEN_UP:
			host_gui_sdl3_pen_event(out, e.type == SDL_EVENT_PEN_DOWN ? host_gui_event_pointer_down
				: host_gui_event_pointer_up, e.ptouch.which, e.ptouch.pen_state, e.ptouch.x, e.ptouch.y);
			return true;
		case SDL_EVENT_PEN_MOTION:
			host_gui_sdl3_pen_event(out, host_gui_event_pointer_motion,
				e.pmotion.which, e.pmotion.pen_state, e.pmotion.x, e.pmotion.y);
			return true;
		case SDL_EVENT_PEN_BUTTON_DOWN:
		case SDL_EVENT_PEN_BUTTON_UP:
			host_gui_sdl3_pen_event(out, host_gui_event_pointer_motion,
				e.pbutton.which, e.pbutton.pen_state, e.pbutton.x, e.pbutton.y);
			return true;
		case SDL_EVENT_PEN_AXIS:
			if (e.paxis.axis == SDL_PEN_AXIS_PRESSURE)
			{
				*host_gui_sdl3_pen(0x20000u + (e.paxis.which & 0xffffu)) = (uint32_t)(e.paxis.value * 65535.0f);
				host_gui_sdl3_pen_event(out, host_gui_event_pointer_motion,
					e.paxis.which, e.paxis.pen_state, e.paxis.x, e.paxis.y);
				return true;
			}
			break;
		case SDL_EVENT_MOUSE_MOTION:
			/* not a mouse that SDL made out of a finger or a pen, that
			   finger or pen is told as itself */
			if (e.motion.which == SDL_TOUCH_MOUSEID || e.motion.which == SDL_PEN_MOUSEID) break;
			out->type = host_gui_event_mouse_motion;
			out->x = (int32_t)e.motion.x;
			out->y = (int32_t)e.motion.y;
			out->buttons = e.motion.state;
			return true;
		case SDL_EVENT_MOUSE_BUTTON_DOWN:
		case SDL_EVENT_MOUSE_BUTTON_UP:
			if (e.button.which == SDL_TOUCH_MOUSEID || e.button.which == SDL_PEN_MOUSEID) break;
			out->type = e.type == SDL_EVENT_MOUSE_BUTTON_DOWN ? host_gui_event_mouse_down : host_gui_event_mouse_up;
			out->x = (int32_t)e.button.x;
			out->y = (int32_t)e.button.y;
			out->buttons = e.button.button;
			out->count = e.button.clicks;
			return true;
		case SDL_EVENT_MOUSE_WHEEL:
			out->type = host_gui_event_mouse_wheel;
#if SDL_VERSION_ATLEAST(3, 4, 0)
			out->x = e.wheel.integer_x * host_gui_sdl3_wheel;
			out->y = e.wheel.integer_y * host_gui_sdl3_wheel;
#else
			// before SDL 3.4 there are only the float amounts
			out->x = (int32_t)e.wheel.x * host_gui_sdl3_wheel;
			out->y = (int32_t)e.wheel.y * host_gui_sdl3_wheel;
#endif
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

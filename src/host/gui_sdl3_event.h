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

/* Has a finger, or a pen, been told of as a pointer ? Only then is the mouse
   that SDL makes of one left out. On a machine that has only ever given a
   mouse, whatever SDL says that mouse is made of, it is passed on. */

static bool host_gui_sdl3_fingers = false;
static bool host_gui_sdl3_pens_seen = false;

/* Devices and contacts. Everything that points is a device: the mouse, each
   pen, each touch panel. A device has contacts, the places it touches at
   once. A pen has one, contact 0. A panel has one for each finger, and each
   finger that lands is given the next number, counting up, and no number is
   given again: a number is a touch, from when it lands to when it lifts, and
   no other touch. So a later one is a bigger number, what is above this can
   tell which came first by it, and nothing said late of a touch that has
   gone can be taken for one that came after. Numbers that were freed and
   given again, the lowest first, put a new finger before one that had been
   down all along. The id of a pointer is the two together, device << 24 |
   contact, and the mouse is device 0. The count goes round after sixteen
   million touches of a device. */

#define HOST_GUI_SDL3_DEVICES 16
#define HOST_GUI_SDL3_CONTACTS 32
#define HOST_GUI_SDL3_DEVICE_TOUCH 1
#define HOST_GUI_SDL3_DEVICE_PEN 2
static struct
{
	uint32_t kind;	/* 0 for a slot not used */
	uint64_t key;	/* what SDL calls the device */
	uint32_t next;	/* the number the next contact to land is given */
	uint32_t number[HOST_GUI_SDL3_CONTACTS];	/* the number each contact that is down was given */
	uint64_t finger[HOST_GUI_SDL3_CONTACTS];	/* what SDL calls each contact that is down */
	bool down[HOST_GUI_SDL3_CONTACTS];
} host_gui_sdl3_devices[HOST_GUI_SDL3_DEVICES];

/* the number of a device, 1 up, it is given one the first time it is seen */

static uint32_t host_gui_sdl3_device(uint32_t kind, uint64_t key)
{
	int spare = -1;
	for (int i = 0; i < HOST_GUI_SDL3_DEVICES; i++)
	{
		if (host_gui_sdl3_devices[i].kind == kind && host_gui_sdl3_devices[i].key == key) return (uint32_t)i + 1;
		if (!host_gui_sdl3_devices[i].kind && spare < 0) spare = i;
	}
	/* more devices than there is room for share the last */
	if (spare < 0) return HOST_GUI_SDL3_DEVICES;
	host_gui_sdl3_devices[spare].kind = kind;
	host_gui_sdl3_devices[spare].key = key;
	return (uint32_t)spare + 1;
}

/* the number of a contact of a device, the one this finger has if it is
   down, or the next there is. With up the finger is no longer down after this */

static uint32_t host_gui_sdl3_contact(uint32_t device, uint64_t finger, bool up)
{
	auto &dev = host_gui_sdl3_devices[device - 1];
	int at = -1;
	for (int i = 0; i < HOST_GUI_SDL3_CONTACTS; i++)
	{
		if (dev.down[i] && dev.finger[i] == finger) { at = i; break; }
	}
	if (at < 0)
	{
		for (int i = 0; i < HOST_GUI_SDL3_CONTACTS; i++)
		{
			if (!dev.down[i]) { at = i; break; }
		}
		/* more fingers than there is room for share the last */
		if (at < 0) at = HOST_GUI_SDL3_CONTACTS - 1;
		dev.finger[at] = finger;
		dev.number[at] = dev.next;
		dev.next = (dev.next + 1) & 0xffffffu;
	}
	dev.down[at] = !up;
	return dev.number[at];
}

/* Is a finger down on a trackpad that is being taken as a touch panel ? The
   Mac goes on making a mouse of the pad whatever SDL is asked: one finger
   moves the mouse, and two that move together are a wheel, which scrolls
   what is under the mouse. A panel does neither. So while a finger is down
   on the pad the moves of the mouse and the wheel are the pad's, and are
   left out. A mouse that is moved while a finger is on the pad is left out
   too, they can not be told apart. */

static Uint64 host_gui_sdl3_pad_last = 0;

static bool host_gui_sdl3_pad_down(void)
{
	if (!host_gui_sdl3_trackpad()) return false;
	for (int i = 0; i < HOST_GUI_SDL3_DEVICES; i++)
	{
		if (host_gui_sdl3_devices[i].kind != HOST_GUI_SDL3_DEVICE_TOUCH) continue;
		for (int j = 0; j < HOST_GUI_SDL3_CONTACTS; j++)
		{
			if (host_gui_sdl3_devices[i].down[j])
			{
				host_gui_sdl3_pad_last = SDL_GetTicks();
				return true;
			}
		}
	}
	return false;
}

/* The wheel the Mac makes of two fingers runs on a while after they lift,
   as a thing thrown would. That is the pad's too, for a second after. */

static bool host_gui_sdl3_pad_wheel(void)
{
	if (host_gui_sdl3_pad_down()) return true;
	return host_gui_sdl3_trackpad() && host_gui_sdl3_pad_last
		&& SDL_GetTicks() - host_gui_sdl3_pad_last < 1000;
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
	/* a pen is a device of its own, with the one contact */
	uint32_t id = host_gui_sdl3_device(HOST_GUI_SDL3_DEVICE_PEN, which) << 24;
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
	host_gui_sdl3_pens_seen = true;
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
			out->id = host_gui_sdl3_device(HOST_GUI_SDL3_DEVICE_TOUCH, (uint64_t)e.tfinger.touchID);
			out->id = (out->id << 24) | host_gui_sdl3_contact(out->id, (uint64_t)e.tfinger.fingerID, up);
			out->kind = host_gui_kind_touch;
			out->buttons = up ? 0 : host_gui_buttons_left;
			out->pressure = up ? 0 : e.tfinger.pressure > 0.0f ? (uint32_t)(e.tfinger.pressure * 65535.0f) : 65535u;
			host_gui_sdl3_fingers = true;
			if (host_gui_sdl3_trackpad()) host_gui_sdl3_pad_last = SDL_GetTicks();
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
				*host_gui_sdl3_pen(host_gui_sdl3_device(HOST_GUI_SDL3_DEVICE_PEN, e.paxis.which) << 24) = (uint32_t)(e.paxis.value * 65535.0f);
				host_gui_sdl3_pen_event(out, host_gui_event_pointer_motion,
					e.paxis.which, e.paxis.pen_state, e.paxis.x, e.paxis.y);
				return true;
			}
			break;
		case SDL_EVENT_MOUSE_MOTION:
			/* not a mouse that SDL made out of a finger or a pen, that
			   finger or pen is told as itself */
			if ((e.motion.which == SDL_TOUCH_MOUSEID && host_gui_sdl3_fingers)
				|| (e.motion.which == SDL_PEN_MOUSEID && host_gui_sdl3_pens_seen)) break;
			if (host_gui_sdl3_pad_down()) break;
			out->type = host_gui_event_mouse_motion;
			out->x = (int32_t)e.motion.x;
			out->y = (int32_t)e.motion.y;
			out->buttons = e.motion.state;
			return true;
		case SDL_EVENT_MOUSE_BUTTON_DOWN:
		case SDL_EVENT_MOUSE_BUTTON_UP:
			if ((e.button.which == SDL_TOUCH_MOUSEID && host_gui_sdl3_fingers)
				|| (e.button.which == SDL_PEN_MOUSEID && host_gui_sdl3_pens_seen)) break;
			out->type = e.type == SDL_EVENT_MOUSE_BUTTON_DOWN ? host_gui_event_mouse_down : host_gui_event_mouse_up;
			out->x = (int32_t)e.button.x;
			out->y = (int32_t)e.button.y;
			out->buttons = e.button.button;
			out->count = e.button.clicks;
			return true;
		case SDL_EVENT_MOUSE_WHEEL:
			if (host_gui_sdl3_pad_wheel()) break;
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

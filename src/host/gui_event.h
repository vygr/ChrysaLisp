/* gui_event.h - what every GUI driver gives the GUI service, an event and a rect */

#ifndef GUI_EVENT_H
#define GUI_EVENT_H

#include <stdint.h>

typedef struct host_gui_rect
{
	int32_t x, y, w, h;
} host_gui_rect;

/* the type of an event, none is no event at all */
enum
{
	host_gui_event_none,
	host_gui_event_quit,
	host_gui_event_shown,
	host_gui_event_resized,
	host_gui_event_key_down,
	host_gui_event_key_up,
	host_gui_event_mouse_motion,
	host_gui_event_mouse_down,
	host_gui_event_mouse_up,
	host_gui_event_mouse_wheel,
};

/* the button of a mouse down or up */
#define host_gui_button_left 1
#define host_gui_button_middle 2
#define host_gui_button_right 3

/* the buttons held, in a mouse motion */
#define host_gui_buttons_left 1
#define host_gui_buttons_middle 2
#define host_gui_buttons_right 4

typedef struct host_gui_event
{
	uint32_t type;
	int32_t x, y;		/* mouse position, wheel steps, or the new size */
	uint32_t buttons;	/* the buttons held in a motion, the button in a down or up */
	uint32_t count;		/* clicks, in a down or up */
	uint32_t scode;		/* key, a USB HID usage number */
	uint32_t direction;	/* wheel, 0 normal, 1 flipped */
	uint32_t pad;
} host_gui_event;

#endif

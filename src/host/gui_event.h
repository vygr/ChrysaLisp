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
	/* a pen or a finger, there can be many at once, each with an id */
	host_gui_event_pointer_down,
	host_gui_event_pointer_motion,
	host_gui_event_pointer_up,
};

/* the button of a mouse down or up */
#define host_gui_button_left 1
#define host_gui_button_middle 2
#define host_gui_button_right 3

/* the buttons held, in a mouse motion */
#define host_gui_buttons_left 1
#define host_gui_buttons_middle 2
#define host_gui_buttons_right 4

/* the buttons of a pen, held in a pointer event, the same bits as a mouse's:
   its tip, the lower button of its barrel and the upper */
#define host_gui_buttons_tip 1
#define host_gui_buttons_barrel 4
#define host_gui_buttons_barrel2 2

/* the kind of a pointer */
#define host_gui_kind_mouse 0
#define host_gui_kind_pen 1
#define host_gui_kind_eraser 2
#define host_gui_kind_touch 3

/* A pointer event is of a pen or a finger. x and y are where it is. Down
   and up are when it touches and lifts, a motion between them is of one
   that touches, any other of one only near. buttons is those it has, each
   its own bit: a pen's tip while it touches and the buttons of its barrel,
   held together; a finger has none. The eraser end of a pen is a device of
   its own, of the eraser kind. The mouse is not told this way, it has its own events above, and where the
   window system makes a mouse out of a finger or a pen for programs that
   know no better, that mouse is not passed on: what takes these events
   makes a mouse of them itself for whatever wants one. The three fields
   after direction are only set in a pointer event. They were added, the
   record was 32 bytes with the last 4 not used, so a GUI service that
   does not know of them is given the events it knows as it was. */

typedef struct host_gui_event
{
	uint32_t type;
	int32_t x, y;		/* mouse position, wheel steps, or the new size */
	uint32_t buttons;	/* the buttons held in a motion, the button in a down or up */
	uint32_t count;		/* clicks, in a down or up */
	uint32_t scode;		/* key, a USB HID usage number */
	uint32_t direction;	/* wheel, 0 normal, 1 flipped */
	uint32_t id;		/* pointer, which one, never 0: its device << 24 | which contact of it */
	uint32_t kind;		/* pointer, pen, eraser or touch */
	uint32_t pressure;	/* pointer, how hard, 0 to 65535 */
} host_gui_event;

#endif

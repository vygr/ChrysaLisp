/*
 * MAME Input Adapter Implementation
 *
 * Maps ChrysaLisp GUI events to MAME input events.
 * Handles keyboard, joystick, and mouse input.
 */

#include "../../include/mame_pii_adapter.h"
#include <string.h>

// Input event queue (simple ring buffer)
#define MAX_INPUT_EVENTS 256

static mame_input_event_t g_event_queue[MAX_INPUT_EVENTS];
static uint32_t g_queue_head = 0;
static uint32_t g_queue_tail = 0;
static uint32_t g_queue_count = 0;

/*
 * Initialize input subsystem
 */
int mame_input_init(void)
{
    mame_log(MAME_LOG_INFO, "Initializing MAME input");

    g_queue_head = 0;
    g_queue_tail = 0;
    g_queue_count = 0;

    return 0;
}

/*
 * Shutdown input subsystem
 */
void mame_input_shutdown(void)
{
    mame_log(MAME_LOG_INFO, "Shutting down input");
}

/*
 * Queue an input event
 */
static void queue_event(const mame_input_event_t* event)
{
    if (g_queue_count >= MAX_INPUT_EVENTS) {
        // Queue full, drop oldest event
        g_queue_head = (g_queue_head + 1) % MAX_INPUT_EVENTS;
        g_queue_count--;
    }

    g_event_queue[g_queue_tail] = *event;
    g_queue_tail = (g_queue_tail + 1) % MAX_INPUT_EVENTS;
    g_queue_count++;
}

/*
 * Poll for input events
 */
int mame_input_poll(mame_input_event_t* event)
{
    if (!event || g_queue_count == 0) {
        return 0;
    }

    *event = g_event_queue[g_queue_head];
    g_queue_head = (g_queue_head + 1) % MAX_INPUT_EVENTS;
    g_queue_count--;

    return 1;
}

/*
 * Process ChrysaLisp GUI event and convert to MAME input event
 *
 * ChrysaLisp GUI event structure (from gui/lisp.inc):
 * Message format varies by event type, but key events have:
 * - +ev_msg_type (offset 0, int): Event type
 * - +ev_msg_key_key (offset for key code)
 * - +ev_msg_key_scode (offset for scan code)
 * - +ev_msg_key_mod (offset for modifiers)
 */

// ChrysaLisp event types (from gui service)
#define EV_TYPE_KEY_DOWN    0x01
#define EV_TYPE_KEY_UP      0x02
#define EV_TYPE_MOUSE_DOWN  0x10
#define EV_TYPE_MOUSE_UP    0x11
#define EV_TYPE_MOUSE_MOVE  0x12

// Key scan codes that map to common arcade controls
#define SCODE_5         0x1D  // Coin insert (5 key)
#define SCODE_1         0x19  // Player 1 start (1 key)
#define SCODE_2         0x1A  // Player 2 start (2 key)
#define SCODE_UP        0x52  // Up arrow
#define SCODE_DOWN      0x51  // Down arrow
#define SCODE_LEFT      0x50  // Left arrow
#define SCODE_RIGHT     0x4F  // Right arrow
#define SCODE_LCTRL     0xE0  // Left control (button 1)
#define SCODE_LALT      0xE2  // Left alt (button 2)
#define SCODE_SPACE     0x2C  // Space (button 3)
#define SCODE_ESC       0x29  // Escape (exit)

void mame_input_process_chrysalisp_event(void* chrysalisp_msg)
{
    if (!chrysalisp_msg) {
        return;
    }

    // Cast to uint64_t array for easier field access
    uint64_t* msg = (uint64_t*)chrysalisp_msg;

    // Get event type (first field)
    uint32_t event_type = (uint32_t)msg[0];

    mame_input_event_t input_event;
    input_event.type = MAME_INPUT_KEYBOARD;
    input_event.pressed = 0;

    switch (event_type) {
        case EV_TYPE_KEY_DOWN:
        case EV_TYPE_KEY_UP:
        {
            // Key event
            // Field offsets (these are approximate, may need adjustment):
            // msg[0] = type
            // msg[1] = target_id
            // msg[2] = key code
            // msg[3] = scan code
            // msg[4] = modifiers

            uint32_t scan_code = (uint32_t)msg[3];
            input_event.code = scan_code;
            input_event.pressed = (event_type == EV_TYPE_KEY_DOWN) ? 1 : 0;

            // Map special keys to arcade controls
            switch (scan_code) {
                case SCODE_5:
                    input_event.type = MAME_INPUT_COIN;
                    input_event.code = 0;  // Coin slot 0
                    break;

                case SCODE_1:
                case SCODE_2:
                    input_event.type = MAME_INPUT_START;
                    input_event.code = (scan_code == SCODE_1) ? 0 : 1;
                    break;

                case SCODE_UP:
                case SCODE_DOWN:
                case SCODE_LEFT:
                case SCODE_RIGHT:
                case SCODE_LCTRL:
                case SCODE_LALT:
                case SCODE_SPACE:
                    input_event.type = MAME_INPUT_JOYSTICK;
                    break;

                default:
                    input_event.type = MAME_INPUT_KEYBOARD;
                    break;
            }

            queue_event(&input_event);
            break;
        }

        case EV_TYPE_MOUSE_DOWN:
        case EV_TYPE_MOUSE_UP:
        {
            // Mouse button event
            input_event.type = MAME_INPUT_MOUSE;
            input_event.code = 0;  // Left button
            input_event.pressed = (event_type == EV_TYPE_MOUSE_DOWN) ? 1 : 0;
            queue_event(&input_event);
            break;
        }

        case EV_TYPE_MOUSE_MOVE:
            // Mouse movement - could be used for trackball games
            // Not implemented yet
            break;

        default:
            // Ignore other event types
            break;
    }
}

/**
 * MAME OSD Input Implementation for ChrysaLisp
 *
 * This module implements MAME's input interface using the ChrysaLisp
 * PII adapter layer. It handles keyboard, joystick, and mouse input,
 * translating ChrysaLisp events into MAME input codes.
 *
 * Key Features:
 * - Keyboard input with scan code mapping
 * - Arcade control mapping (coins, start, directions, buttons)
 * - Input state tracking for polling
 * - Event-driven architecture using adapter layer
 */

#include "../../../include/mame_pii_adapter.h"
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

// MAME input codes (subset of what MAME uses)
// These would normally come from MAME headers, but we define the essential ones
#define MAME_KEYCODE_INVALID    0x00000000

// Arrow keys
#define MAME_KEYCODE_UP         0x00000001
#define MAME_KEYCODE_DOWN       0x00000002
#define MAME_KEYCODE_LEFT       0x00000003
#define MAME_KEYCODE_RIGHT      0x00000004

// Action buttons (mapped to various keys)
#define MAME_KEYCODE_LCONTROL   0x00000010
#define MAME_KEYCODE_LALT       0x00000011
#define MAME_KEYCODE_SPACE      0x00000012
#define MAME_KEYCODE_LSHIFT     0x00000013
#define MAME_KEYCODE_Z          0x00000014
#define MAME_KEYCODE_X          0x00000015
#define MAME_KEYCODE_C          0x00000016

// Special keys
#define MAME_KEYCODE_1          0x00000020
#define MAME_KEYCODE_2          0x00000021
#define MAME_KEYCODE_3          0x00000022
#define MAME_KEYCODE_4          0x00000023
#define MAME_KEYCODE_5          0x00000024
#define MAME_KEYCODE_6          0x00000025

#define MAME_KEYCODE_ENTER      0x00000030
#define MAME_KEYCODE_ESC        0x00000031
#define MAME_KEYCODE_TAB        0x00000032
#define MAME_KEYCODE_F1         0x00000040
#define MAME_KEYCODE_F2         0x00000041
#define MAME_KEYCODE_F3         0x00000042
#define MAME_KEYCODE_F10        0x00000049
#define MAME_KEYCODE_F12        0x0000004B

// Mouse buttons
#define MAME_MOUSECODE_BUTTON1  0x00001000
#define MAME_MOUSECODE_BUTTON2  0x00001001
#define MAME_MOUSECODE_BUTTON3  0x00001002

// Joystick codes
#define MAME_JOYCODE_BUTTON1    0x00002000
#define MAME_JOYCODE_BUTTON2    0x00002001
#define MAME_JOYCODE_BUTTON3    0x00002002
#define MAME_JOYCODE_BUTTON4    0x00002003

// Maximum tracked keys
#define MAX_INPUT_STATES 256

// Input state tracking
typedef struct {
    uint32_t code;        // MAME input code
    bool pressed;         // Current state
    bool previous;        // Previous frame state
} input_state_t;

// Global input state
static bool g_input_initialized = false;
static input_state_t g_input_states[MAX_INPUT_STATES];
static uint32_t g_num_tracked_inputs = 0;

// Mouse state
static int32_t g_mouse_x = 0;
static int32_t g_mouse_y = 0;
static int32_t g_mouse_delta_x = 0;
static int32_t g_mouse_delta_y = 0;

/**
 * Map ChrysaLisp input type to MAME input code
 */
static uint32_t map_input_type_to_mame_code(uint32_t input_type, uint32_t param) {
    switch (input_type) {
        // Directional controls
        case MAME_INPUT_UP:    return MAME_KEYCODE_UP;
        case MAME_INPUT_DOWN:  return MAME_KEYCODE_DOWN;
        case MAME_INPUT_LEFT:  return MAME_KEYCODE_LEFT;
        case MAME_INPUT_RIGHT: return MAME_KEYCODE_RIGHT;

        // Action buttons
        case MAME_INPUT_BUTTON1: return MAME_KEYCODE_LCONTROL;
        case MAME_INPUT_BUTTON2: return MAME_KEYCODE_LALT;
        case MAME_INPUT_BUTTON3: return MAME_KEYCODE_SPACE;
        case MAME_INPUT_BUTTON4: return MAME_KEYCODE_LSHIFT;
        case MAME_INPUT_BUTTON5: return MAME_KEYCODE_Z;
        case MAME_INPUT_BUTTON6: return MAME_KEYCODE_X;

        // Arcade controls
        case MAME_INPUT_COIN:   return MAME_KEYCODE_5;
        case MAME_INPUT_START:  return MAME_KEYCODE_1;

        // Special
        case MAME_INPUT_PAUSE:  return MAME_KEYCODE_ESC;
        case MAME_INPUT_MENU:   return MAME_KEYCODE_TAB;

        default: return MAME_KEYCODE_INVALID;
    }
}

/**
 * Find or create input state entry
 */
static input_state_t* get_input_state(uint32_t code) {
    // Search existing states
    for (uint32_t i = 0; i < g_num_tracked_inputs; i++) {
        if (g_input_states[i].code == code) {
            return &g_input_states[i];
        }
    }

    // Create new state if room available
    if (g_num_tracked_inputs < MAX_INPUT_STATES) {
        input_state_t* state = &g_input_states[g_num_tracked_inputs++];
        state->code = code;
        state->pressed = false;
        state->previous = false;
        return state;
    }

    return nullptr;
}

/**
 * Input event callback
 */
static void input_event_callback(const mame_input_event_t* event, void* user_data) {
    (void)user_data; // Unused

    uint32_t mame_code = map_input_type_to_mame_code(event->type, event->param);
    if (mame_code == MAME_KEYCODE_INVALID) {
        return;
    }

    input_state_t* state = get_input_state(mame_code);
    if (state) {
        state->pressed = event->pressed;
    }

    // Handle mouse events
    if (event->type == MAME_INPUT_MOUSE_MOVE) {
        g_mouse_delta_x = event->param - g_mouse_x;
        g_mouse_delta_y = event->param2 - g_mouse_y;
        g_mouse_x = event->param;
        g_mouse_y = event->param2;
    }
}

/**
 * Initialize the OSD input system
 */
extern "C" int osd_input_init(void) {
    if (g_input_initialized) {
        return 0; // Already initialized
    }

    // Initialize the adapter input system
    if (mame_input_init() != 0) {
        return -1;
    }

    // Clear input state
    memset(g_input_states, 0, sizeof(g_input_states));
    g_num_tracked_inputs = 0;
    g_mouse_x = 0;
    g_mouse_y = 0;
    g_mouse_delta_x = 0;
    g_mouse_delta_y = 0;

    g_input_initialized = true;
    return 0;
}

/**
 * Shutdown the OSD input system
 */
extern "C" void osd_input_shutdown(void) {
    if (!g_input_initialized) {
        return;
    }

    mame_input_shutdown();
    g_input_initialized = false;
}

/**
 * Poll for input events (called each frame)
 */
extern "C" void osd_input_poll(void) {
    if (!g_input_initialized) {
        return;
    }

    // Update previous states
    for (uint32_t i = 0; i < g_num_tracked_inputs; i++) {
        g_input_states[i].previous = g_input_states[i].pressed;
    }

    // Reset mouse deltas
    g_mouse_delta_x = 0;
    g_mouse_delta_y = 0;

    // Poll events from adapter (processes event queue)
    mame_input_poll(input_event_callback, nullptr);
}

/**
 * Check if a key is currently pressed
 */
extern "C" bool osd_input_is_pressed(uint32_t code) {
    if (!g_input_initialized) {
        return false;
    }

    for (uint32_t i = 0; i < g_num_tracked_inputs; i++) {
        if (g_input_states[i].code == code) {
            return g_input_states[i].pressed;
        }
    }

    return false;
}

/**
 * Check if a key was just pressed this frame
 */
extern "C" bool osd_input_pressed_this_frame(uint32_t code) {
    if (!g_input_initialized) {
        return false;
    }

    for (uint32_t i = 0; i < g_num_tracked_inputs; i++) {
        if (g_input_states[i].code == code) {
            return g_input_states[i].pressed && !g_input_states[i].previous;
        }
    }

    return false;
}

/**
 * Check if a key was just released this frame
 */
extern "C" bool osd_input_released_this_frame(uint32_t code) {
    if (!g_input_initialized) {
        return false;
    }

    for (uint32_t i = 0; i < g_num_tracked_inputs; i++) {
        if (g_input_states[i].code == code) {
            return !g_input_states[i].pressed && g_input_states[i].previous;
        }
    }

    return false;
}

/**
 * Get mouse position
 */
extern "C" void osd_input_get_mouse_pos(int32_t* x, int32_t* y) {
    if (x) *x = g_mouse_x;
    if (y) *y = g_mouse_y;
}

/**
 * Get mouse delta (movement since last poll)
 */
extern "C" void osd_input_get_mouse_delta(int32_t* dx, int32_t* dy) {
    if (dx) *dx = g_mouse_delta_x;
    if (dy) *dy = g_mouse_delta_y;
}

/**
 * Get list of all active input codes (for UI display)
 */
extern "C" uint32_t osd_input_get_active_codes(uint32_t* codes, uint32_t max_codes) {
    uint32_t count = 0;

    for (uint32_t i = 0; i < g_num_tracked_inputs && count < max_codes; i++) {
        if (g_input_states[i].pressed) {
            codes[count++] = g_input_states[i].code;
        }
    }

    return count;
}

/**
 * Get input code name (for UI display and configuration)
 */
extern "C" const char* osd_input_code_name(uint32_t code) {
    switch (code) {
        case MAME_KEYCODE_UP:       return "Up";
        case MAME_KEYCODE_DOWN:     return "Down";
        case MAME_KEYCODE_LEFT:     return "Left";
        case MAME_KEYCODE_RIGHT:    return "Right";
        case MAME_KEYCODE_LCONTROL: return "Left Ctrl";
        case MAME_KEYCODE_LALT:     return "Left Alt";
        case MAME_KEYCODE_SPACE:    return "Space";
        case MAME_KEYCODE_LSHIFT:   return "Left Shift";
        case MAME_KEYCODE_Z:        return "Z";
        case MAME_KEYCODE_X:        return "X";
        case MAME_KEYCODE_C:        return "C";
        case MAME_KEYCODE_1:        return "1";
        case MAME_KEYCODE_2:        return "2";
        case MAME_KEYCODE_3:        return "3";
        case MAME_KEYCODE_4:        return "4";
        case MAME_KEYCODE_5:        return "5";
        case MAME_KEYCODE_6:        return "6";
        case MAME_KEYCODE_ENTER:    return "Enter";
        case MAME_KEYCODE_ESC:      return "Escape";
        case MAME_KEYCODE_TAB:      return "Tab";
        case MAME_KEYCODE_F1:       return "F1";
        case MAME_KEYCODE_F2:       return "F2";
        case MAME_KEYCODE_F3:       return "F3";
        case MAME_KEYCODE_F10:      return "F10";
        case MAME_KEYCODE_F12:      return "F12";
        default:                    return "Unknown";
    }
}

/**
 * Enable/disable input grabbing (for UI vs. emulation mode)
 */
extern "C" void osd_input_set_grab(bool enable) {
    // In ChrysaLisp, we don't really have input grabbing like SDL
    // This is a no-op for now, but could be used to show/hide cursor
    (void)enable;
}

/**
 * Lightgun support (future enhancement)
 */
extern "C" void osd_input_get_lightgun_pos(int player, int32_t* x, int32_t* y) {
    // For now, just return mouse position for player 0
    if (player == 0) {
        if (x) *x = g_mouse_x;
        if (y) *y = g_mouse_y;
    } else {
        if (x) *x = 0;
        if (y) *y = 0;
    }
}

/**
 * Analog input support (for games with analog controls)
 */
extern "C" float osd_input_get_analog(uint32_t code) {
    // Not implemented yet - would need joystick/gamepad support
    // Return 0.0 (neutral) for now
    (void)code;
    return 0.0f;
}

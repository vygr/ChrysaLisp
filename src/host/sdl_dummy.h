/* sdl_dummy.h - faked up SDL header for non-SDL video */

#include <inttypes.h>
typedef uint8_t         Uint8;
typedef uint16_t        Uint16;
typedef uint32_t        Uint32;
typedef int32_t         Sint32;

struct SDL_Rect
{
	int32_t x, y, w, h;
};

/* keyboard */
typedef struct keysym   SDL_Keysym;
typedef Sint32          SDL_Keycode;
enum scancode {
    SDL_SCANCODE_INSERT = 73,
    SDL_SCANCODE_HOME = 74,
    SDL_SCANCODE_PAGEUP = 75,
    SDL_SCANCODE_DELETE = 76,
    SDL_SCANCODE_END = 77,
    SDL_SCANCODE_PAGEDOWN = 78,
    SDL_SCANCODE_RIGHT = 79,
    SDL_SCANCODE_LEFT = 80,
    SDL_SCANCODE_DOWN = 81,
    SDL_SCANCODE_UP = 82,
};
typedef enum scancode   SDL_Scancode;

typedef enum {
    KMOD_LSHIFT = 0x0001,
    KMOD_RSHIFT = 0x0002,
    KMOD_LCTRL = 0x0040,
    KMOD_RCTRL = 0x0080,
} SDL_KeyMod;
#define KMOD_CTRL               (KMOD_LCTRL|KMOD_RCTRL)
#define KMOD_SHIFT              (KMOD_LSHIFT|KMOD_RSHIFT)

/*  mouse */
#define SDL_RELEASED            0
#define SDL_PRESSED             1

#define SDL_BUTTON_LEFT         1
#define SDL_BUTTON_RIGHT        3

#define SDL_BUTTON_LMASK        01
#define SDL_BUTTON_RMASK        04

#define SDL_MOUSEWHEEL_NORMAL   0
#define SDL_MOUSEWHEEL_FLIPPED  1

/* event */
enum eventtype {
    SDL_QUIT           = 0x100, /**< User-requested quit */
    SDL_WINDOWEVENT    = 0x200, /**< Window state change */
    SDL_KEYDOWN        = 0x300, /**< Key pressed */
    SDL_KEYUP,                  /**< Key released */
    SDL_MOUSEMOTION    = 0x400, /**< Mouse moved */
    SDL_MOUSEBUTTONDOWN,        /**< Mouse button pressed */
    SDL_MOUSEBUTTONUP,          /**< Mouse button released */
    SDL_MOUSEWHEEL,             /**< Mouse wheel motion */
};

enum eventid {
	SDL_WINDOWEVENT_SHOWN = 0x1,
	SDL_WINDOWEVENT_SIZE_CHANGED = 0x6,
	SDL_WINDOWEVENT_RESTORED = 0x9,
};

struct quitevent {
    Uint32 type;        /**< ::SDL_QUIT */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
};

struct windowevent {
    Uint32 type;        /**< ::SDL_WINDOWEVENT */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
    Uint32 windowID;    /**< The associated window */
    Uint8 event;        /**< ::SDL_WindowEventID */
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    Sint32 data1;       /**< event dependent data */
    Sint32 data2;       /**< event dependent data */
};

struct keysym {
    enum scancode scancode; /**< SDL physical key code - see ::SDL_Scancode for details */
    SDL_Keycode sym;        /**< SDL virtual key code - see ::SDL_Keycode for details */
    Uint16 mod;             /**< current key modifiers */
    Uint32 unused;
};

struct keyboardevent {
    Uint32 type;        /**< ::SDL_KEYDOWN or ::SDL_KEYUP */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
    Uint32 windowID;    /**< The window with keyboard focus, if any */
    Uint8 state;        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    Uint8 repeat;       /**< Non-zero if this is a key repeat */
    Uint8 padding2;
    Uint8 padding3;
    struct keysym keysym; /**< The key that was pressed or released */
};

struct mousemotionevent {
    Uint32 type;        /**< ::SDL_MOUSEMOTION */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
    Uint32 windowID;    /**< The window with mouse focus, if any */
    Uint32 which;       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    Uint32 state;       /**< The current button state */
    Sint32 x;           /**< X coordinate, relative to window */
    Sint32 y;           /**< Y coordinate, relative to window */
    Sint32 xrel;        /**< The relative motion in the X direction */
    Sint32 yrel;        /**< The relative motion in the Y direction */
};

struct mousebuttonevent {
    Uint32 type;        /**< ::SDL_MOUSEBUTTONDOWN or ::SDL_MOUSEBUTTONUP */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
    Uint32 windowID;    /**< The window with mouse focus, if any */
    Uint32 which;       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    Uint8 button;       /**< The mouse button index */
    Uint8 state;        /**< ::SDL_PRESSED or ::SDL_RELEASED */
    Uint8 clicks;       /**< 1 for single-click, 2 for double-click, etc. */
    Uint8 padding1;
    Sint32 x;           /**< X coordinate, relative to window */
    Sint32 y;           /**< Y coordinate, relative to window */
};

struct mousewheelevent {
    Uint32 type;        /**< ::SDL_MOUSEWHEEL */
    Uint32 timestamp;   /**< In milliseconds, populated using SDL_GetTicks() */
    Uint32 windowID;    /**< The window with mouse focus, if any */
    Uint32 which;       /**< The mouse instance id, or SDL_TOUCH_MOUSEID */
    Sint32 x;           /**< The amount scrolled horizontally, positive to the right and negative to the left */
    Sint32 y;           /**< The amount scrolled vertically, positive away from the user and negative toward the user */
    Uint32 direction;   /**< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back */
};

union event {
    Uint32 type;                        /**< Event type, shared with all events */
    struct windowevent window;          /**< Window event data */
    struct keyboardevent key;           /**< Keyboard event data */
    struct mousemotionevent motion;     /**< Mouse motion event data */
    struct mousebuttonevent button;     /**< Mouse button event data */
    struct mousewheelevent wheel;       /**< Mouse wheel event data */
    struct quitevent quit;              /**< Quit request event data */
    Uint8 padding[56];
};

typedef union event SDL_Event;
typedef struct windowevent SDL_WindowEvent;
typedef struct keyboardevent SDL_KeyboardEvent;
typedef struct mousemotionevent SDL_MouseMotionEvent;
typedef struct mousebuttonevent SDL_MouseButtonEvent;
typedef struct mousewheelevent SDL_MouseWheelEvent;

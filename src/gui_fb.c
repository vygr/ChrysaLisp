#if defined(_HOST_GUI)
#if _HOST_GUI == 1
/*
 * Host GUI for ChrysaLisp Framebuffer
 *
 * May 2023 Greg Haerr
 */
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <termios.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/fb.h>
#include <linux/keyboard.h>
#include <linux/kd.h>
#include <linux/vt.h>
#include "sdl-dummy.h"

#define DEBUG   1      /* exit on ESC, don't change to graphics console */

#define PATH_FRAMEBUFFER    "/dev/fb0"          /* or env "FRAMEBUFFER" */
#define PATH_KEYBOARD       "/dev/tty"          /* or env "CONSOLE" */
#define PATH_MOUSE          "/dev/input/mice"

#define SCROLLFACTOR        4   /* multiply factor for scrollwheel */

/* pixel formats, -1 means unsupported */
#define PF_UNKNOWN          0
#define PF_ARGB8888         1   /* 32bpp, memory byte order B, G, R, A */
#define PF_ABGR8888         -2  /* 32bpp, memory byte order R, G, B, A */
#define PF_BGR888           -3  /* 24bpp, memory byte order R, G, B */
#define PF_RGB565           4   /* 16bpp, le unsigned short 5/6/5 RGB */
#define PF_RGB555           -5  /* 15bpp, le unsigned short 5/5/5 RGB */
#define PF_PALETTE          -6  /*  8bpp palette */

typedef uint32_t pixel_t;       /* fixed ARGB8888 for now */
typedef uint8_t alpha_t;        /* size of alpha channel */

typedef struct rect {
    int32_t x, y;
    int32_t w, h;
} Rect;

typedef struct drawable {
    int32_t pixtype;            /* pixel format */
    int32_t bpp;                /* bits per pixel */
    int32_t bytespp;            /* bytes per pixel */
    int32_t width;              /* width in pixels */
    int32_t height;             /* height in pixels */
    int32_t pitch;              /* stride in bytes, offset to next pixel row */
    int32_t size;               /* total size in bytes */
    uint8_t *pixels;            /* pixel data */
    uint32_t color;             /* rgb color for glyph blit mode */
    int32_t mode;               /* blit mode: 1 = colormod */
    pixel_t data[];             /* texture data allocated in single malloc */
} Drawable, Texture;

static struct termios orig;
static int frame_fd = -1;           /* framebuffer file handle */
static int mouse_fd = -1;
static int keybd_fd = -1;;
static int posx, posy;              /* cursor position */
static Drawable fb;                 /* hardware framebuffer */
static Drawable bb;                 /* back buffer */
static Rect clip;
static pixel_t draw_color = -1;      /* default color */

void host_gui_deinit(void);
static int open_framebuffer(void);
static int open_keyboard(void);
static int open_mouse(void);
static int read_mouse(int *dx, int *dy, int *dw, int *bp);
static void close_framebuffer(void);
static void close_keyboard(void);
static void close_mouse(void);
static void blit_blend(Drawable *src, const Rect *srect, Drawable *dst, const Rect *drect);
static void blit_srccopy(Drawable *src, const Rect *srect, Drawable *dst, const Rect *drect);
static void blit_srccopy_rgb565(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect);

#if DEBUG
#define unassert(a)   if (!(a)) unassert_handler(#a,__FILE__, __LINE__)

/* exit graphics mode for error message */
static void unassert_handler(char *msg, char *file, int line)
{
    host_gui_deinit();
    printf("Assertion failed: %s at %s:%d\n", msg, file, line);
	exit(255);
}

static void catch_signals(int signo)
{
    host_gui_deinit();
    printf("SIGNAL %d\n", signo);
	exit(1);
}
#else
#define unassert(a)
#endif

void host_gui_begin_composite(void)
{
}

void host_gui_end_composite(void)
{
}

void host_gui_flush(const Rect *r)
{
    Rect cr;
    if (r == NULL) {
        cr.x = 0;
        cr.y = 0;
        cr.w = fb.width;
        cr.h = fb.height;
        r = &cr;
    }
    if (fb.pixtype == PF_RGB565) {
        blit_srccopy_rgb565(&bb, r, &fb, r);
    } else {
        blit_srccopy(&bb, r, &fb, r);
    }
}

void host_gui_resize(uint64_t w, uint64_t h)
{
}

/* return rect adjusted to current clip rectangle */
static Rect *get_clip_rect(const Rect *rect)
{
    static Rect r;

    r = *rect;
    if (r.w <= 0 || r.h <= 0) return NULL;
    r.w += r.x;
    r.h += r.y;
    if (r.x >= clip.w || r.y >= clip.h) return NULL;
    if (r.w <= clip.x || r.h <= clip.y) return NULL;
    if (clip.x > r.x) r.x = clip.x;
    if (clip.y > r.y) r.y = clip.y;
    if (r.w > clip.w) r.w = clip.w;
    if (r.h > clip.h) r.h = clip.h;
    r.w -= r.x;
    r.h -= r.y;
    return &r;
}

/* set global clipping rectangle */
void host_gui_set_clip(const Rect *rect)
{
    /* store as x, y, x1, y1 */
    if (rect) {
        clip = *rect;
        clip.w += clip.x;
        clip.h += clip.y;
        if (clip.x < 0) clip.x = 0;
        if (clip.y < 0) clip.y = 0;
        if (clip.w >= fb.width) clip.w = fb.width - 1;
        if (clip.h >= fb.height) clip.h = fb.height - 1;
    } else {
        clip.x = 0;
        clip.w = fb.width - 1;
        clip.y = 0;
        clip.h = fb.height - 1;
    }
}

/* set color for Drawables */
void host_gui_set_color(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
    /* premul colors with alpha */
    r = ((r + 1) * a) >> 8;
    g = ((g + 1) * a) >> 8;
    b = ((b + 1) * a) >> 8;
    draw_color = (a << 24) + (r << 16) + (g << 8)  + b;
}

void host_gui_texture_color(Texture *texture, uint8_t r, uint8_t g, uint8_t b)
{
    /* colors used in color mod blit */
    texture->color = r | (g << 8) | (b << 16);
}

/* allocate drawable for passed data and return a handle to it */
Texture *host_gui_create_texture(pixel_t *src, uint64_t width, uint64_t height, uint64_t pitch, uint64_t mode)
{
    Texture *t;
    int s = (mode == 1)? sizeof(alpha_t): sizeof(pixel_t);
    int size = height * width * s;

    t = malloc(sizeof(Texture) + size);
    unassert(t);
    t->pixtype = bb.pixtype;
    t->bpp = bb.bpp;
    t->bytespp = s;
    t->width = width;
    t->height = height;
    t->pitch = width * s;
    t->size = size;
    t->pixels = (uint8_t *)t->data;
    t->color = 0xffffff;
    t->mode = mode;

    pixel_t *src_end = (pixel_t*)((uint8_t*)src + height * pitch);
    uint32_t span = width * sizeof(pixel_t);
    pitch -= span;
    if (mode == 1) {        // glyph mode texture
        alpha_t *dst = (alpha_t *)t->pixels;
        do {
            pixel_t *src_end_line = (pixel_t *)((uint8_t *)src + span);
            do {
                *dst++ = *src++ >> 24;
            } while (src != src_end_line);
            src = (pixel_t *)((uint8_t *)src + pitch);
        } while (src != src_end);
    } else {                // normal mode texture
        pixel_t *dst = (pixel_t *)t->pixels;
        do {
            pixel_t *src_end_line = (pixel_t *)((uint8_t *)src + span);
            do {
                *dst++ = *src++;
            } while (src != src_end_line);
            src = (pixel_t *)((uint8_t *)src + pitch);
        } while (src != src_end);
    }
    return t;
}

void host_gui_destroy_texture(Texture *texture)
{
    unassert(texture);
    free(texture);
}

/* fill rectangle with current color */
void host_gui_filled_box(const Rect *rect)
{
    pixel_t color_a = draw_color >> 24;
    if (color_a == 0) return;
    Rect *r = get_clip_rect(rect);
    if (!r) return;
    pixel_t *dst = (pixel_t *)(bb.pixels + r->y * bb.pitch + r->x * bb.bytespp);
    int span = (bb.pitch >> 2) - r->w;      /* in pixels, not bytes */
    
    int h = r->h;
    if (color_a == 0xff) {  /* source copy */
        do {
            int w = r->w;
            do {
                *dst++ = draw_color;
            } while (--w > 0);
            dst += span;
        } while (--h > 0);
    } else {                /* premul blend with global color */
        pixel_t da = 0xff - color_a;
        do {
            int w = r->w;
            do {
                pixel_t drb = *dst;
                pixel_t dg = drb & 0x00ff00;
                       drb = drb & 0xff00ff;
                drb = ((drb * da >> 8) & 0xff00ff) + (draw_color & 0xff00ff);
                dg =   ((dg * da >> 8) & 0x00ff00) + (draw_color & 0x00ff00);
                *dst++ = drb + dg;
            } while (--w > 0);
            dst += span;
        } while (--h > 0);
    }
}

/* draw rectangle - this function isn't required in a driver */
void host_gui_box(const Rect *rect)
{
    /* just call filled box and let it do the clipping and drawing */
    Rect r = *rect;
    if (rect->w <= 0 || rect->h <= 0) return;
    r.h = 1;
    host_gui_filled_box(&r);
    if (rect->h <= 1) return;

    r.y = rect->y + rect->h - 1;
    host_gui_filled_box(&r);
    if (rect->h <= 2) return;

    r.y = rect->y + 1;
    r.w = 1;
    r.h = rect->h - 2;
    host_gui_filled_box(&r);
    if (rect->w <= 1) return;

    r.x = rect->x + rect->w - 1;
    host_gui_filled_box(&r);
}

/* fast srccopy blit, no clipping */
static void blit_srccopy(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    pixel_t *dst = (pixel_t *)(td->pixels + drect->y * td->pitch + drect->x * td->bytespp);
    pixel_t *src = (pixel_t *)(ts->pixels + srect->y * ts->pitch + srect->x * ts->bytespp);
    int span = drect->w * td->bytespp;
    int dspan = td->pitch - span;
    int sspan = ts->pitch - span;
    int y = drect->h;
    do {
        int x = drect->w;
        do {
            *dst++ = *src++;
        } while (--x > 0);
        dst = (pixel_t *)((uint8_t *)dst + dspan);
        src = (pixel_t *)((uint8_t *)src + sspan);
    } while (--y > 0);
}

/* source copy conversion blit ARGB888 -> RGB565, no clipping */
static void blit_srccopy_rgb565(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    unassert(srect->w == drect->w);
    unassert(srect->h == drect->h);
    uint16_t *dst = (uint16_t *)(td->pixels + drect->y * td->pitch + drect->x * td->bytespp);
    pixel_t *src =   (pixel_t *)(ts->pixels + srect->y * ts->pitch + srect->x * ts->bytespp);
    int sspan = ts->pitch - (srect->w * ts->bytespp);
    int dspan = td->pitch - (drect->w * td->bytespp);
    int y = drect->h;
    do {
        int x = drect->w;
        do {
            pixel_t s = *src++;
            *dst++ = (((s >> 16) & 0xf8) << 8) | (((s >> 8) & 0xfc) << 3) | ((s & 0xf8) >> 3);
        } while (--x > 0);
        dst = (uint16_t *)((uint8_t *)dst + dspan);
        src = (pixel_t *)((uint8_t *)src + sspan);
    } while (--y > 0);
}

/* premultiplied alpha blend blit, no clipping */
static void blit_blend(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    //unassert(srect->w == drect->w);   //FIXME check why src width can != dst width
    /* src and dst height can differ, will use dst height for drawing */
    pixel_t *dst = (pixel_t *)(td->pixels + drect->y * td->pitch + drect->x * td->bytespp);
    pixel_t *src = (pixel_t *)(ts->pixels + srect->y * ts->pitch + srect->x * ts->bytespp);
    int span = drect->w * td->bytespp;
    int dspan = td->pitch - span;
    int sspan = ts->pitch - span;
    int y = drect->h;
    do {
        int x = drect->w;
        do {
            pixel_t sa = *src++;
            if (sa > 0x00ffffff) {
                if (sa < 0xff000000) {          /* premul blend from source */
                    pixel_t drb = *dst;
                    pixel_t dg = drb & 0x00ff00;
                           drb = drb & 0xff00ff;
                    pixel_t da = 0xff - (sa >> 24);
                    drb = ((drb * da >> 8) & 0xff00ff) + (sa & 0xff00ff);
                    dg =   ((dg * da >> 8) & 0x00ff00) + (sa & 0x00ff00);
                    *dst = drb + dg;
                 } else {                       /* source copy */
                    *dst = sa & 0xffffff;
                 }
            }
            dst++;
        } while (--x > 0);
        dst = (pixel_t *)((uint8_t *)dst + dspan);
        src = (pixel_t *)((uint8_t *)src + sspan);
    } while (--y > 0);
}

/* color mod blit, no clipping */
static void blit_colormod(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    //unassert(srect->w == drect->w);   //FIXME check why src width can != dst width
    /* src and dst height can differ, will use dst height for drawing */
    pixel_t *dst = (pixel_t *)(td->pixels + drect->y * td->pitch + drect->x * td->bytespp);
    alpha_t *src = (alpha_t *)(ts->pixels + srect->y * ts->pitch + srect->x * ts->bytespp);
    int dspan = td->pitch - (drect->w * td->bytespp);
    int sspan = ts->pitch - drect->w;
    int y = drect->h;
    do {
        int x = drect->w;
        do {
            alpha_t sa = *src++;
            if (sa != 0) {
                pixel_t srb = ((sa * (ts->color & 0xff00ff)) >> 8) & 0xff00ff;
                pixel_t sg =  ((sa * (ts->color & 0x00ff00)) >> 8) & 0x00ff00;
                if (sa != 0xff) {
                    pixel_t da = 0xff - sa;
                    pixel_t drb = *dst;
                    pixel_t dg = drb & 0x00ff00;
                           drb = drb & 0xff00ff;
                    drb = ((drb * da >> 8) & 0xff00ff) + srb;
                    dg =   ((dg * da >> 8) & 0x00ff00) + sg;
                    *dst = drb + dg;
                } else {
                    *dst = srb + sg;
                }
            }
            dst++;
        } while (--x > 0);
        dst = (pixel_t *)((uint8_t *)dst + dspan);
        src = (alpha_t *)((uint8_t *)src + sspan);
    } while (--y > 0);
}

/* draw pixels from passed texture handle */
void host_gui_blit(Texture *texture, const Rect *srect, const Rect *drect)
{
    unassert(texture);
    unassert(srect->w == drect->w);
    unassert(srect->h == drect->h);
    Rect *cr = get_clip_rect(drect);
    if (!cr) return;
    Rect sr2 = *srect;
    if (clip.x > drect->x) sr2.x += clip.x - drect->x;
    if (clip.y > drect->y) sr2.y += clip.y - drect->y;
    if (texture->mode == 1)
        blit_colormod(texture, &sr2, &bb, cr);
    else blit_blend(texture, &sr2, &bb, cr);
}

#define BUTTON_L        0x01      /* left button*/
#define BUTTON_R        0x02      /* right button*/
#define BUTTON_M        0x10      /* middle*/
#define BUTTON_SCROLLUP 0x20      /* wheel up*/
#define BUTTON_SCROLLDN 0x40      /* wheel down*/

const uint8_t scan_code_to_hid_table[] =
{
    0x00, // 0x00: Nul
    0x29, // 0x01: Escape
    0x1E, // 0x02: 1 !
    0x1F, // 0x03: 2 @
    0x20, // 0x04: 3 #
    0x21, // 0x05: 4 $
    0x22, // 0x06: 5 %
    0x23, // 0x07: 6 ^

    0x24, // 0x08: 7 &
    0x25, // 0x09: 8 *
    0x26, // 0x0A: 9 (
    0x27, // 0x0B: 0 )
    0x2D, // 0x0C: - _
    0x2E, // 0x0D: = +
    0x2A, // 0x0E: Backspace
    0x2B, // 0x0F: Tab

    0x14, // 0x10: q Q
    0x1A, // 0x11: w W
    0x08, // 0x12: e E
    0x15, // 0x13: r R
    0x17, // 0x14: t T
    0x1C, // 0x15: y Y
    0x18, // 0x16: u U
    0x0C, // 0x17: i I

    0x12, // 0x18: o O
    0x13, // 0x19: p P
    0x2F, // 0x1A: [ {
    0x30, // 0x1B: ] }
    0x28, // 0x1C: Return
    0xE0, // 0x1D: Left Control
    0x04, // 0x1E: a A
    0x16, // 0x1F: s S

    0x07, // 0x20: d D
    0x09, // 0x21: f F
    0x0A, // 0x22: g G
    0x0B, // 0x23: h H
    0x0D, // 0x24: j J
    0x0E, // 0x25: k K
    0x0F, // 0x26: l L
    0x33, // 0x27: ; :

    0x34, // 0x28: ' "
    0x35, // 0x29: ` ~
    0xE1, // 0x2A: Left Shift
    0x31, // 0x2B: \ |
    0x1D, // 0x2C: z Z
    0x1B, // 0x2D: x X
    0x06, // 0x2E: c C
    0x19, // 0x2F: v V

    0x05, // 0x30: b B
    0x11, // 0x31: n N
    0x10, // 0x32: m M
    0x36, // 0x33: , <
    0x37, // 0x34: . >
    0x38, // 0x35: / ?
    0xE5, // 0x36: Right Shift
    0x55, // 0x37: Keypad *

    0xE2, // 0x38: Left Alt
    0x2C, // 0x39: Space
    0x39, // 0x3A: Caps Lock
    0x3A, // 0x3B: F1
    0x3B, // 0x3C: F2
    0x3C, // 0x3D: F3
    0x3D, // 0x3E: F4
    0x3E, // 0x3F: F5

    0x3F, // 0x40: F6
    0x40, // 0x41: F7
    0x41, // 0x42: F8
    0x42, // 0x43: F9
    0x43, // 0x44: F10
    0x53, // 0x45: Num Lock
    0x47, // 0x46: Scroll Lock
    0x5F, // 0x47: Keypad 7

    0x60, // 0x48: Keypad 8
    0x61, // 0x49: Keypad 9
    0x56, // 0x4A: Keypad -
    0x5C, // 0x4B: Keypad 4
    0x5D, // 0x4C: Keypad 5
    0x5E, // 0x4D: Keypad 6
    0x57, // 0x4E: Keypad +
    0x59, // 0x4F: Keypad 1

    0x5A, // 0x50: Keypad 2
    0x5B, // 0x51: Keypad 3
    0x62, // 0x52: Keypad 0
    0x63, // 0x53: Keypad .
    0x00, // 0x54: Nul
    0x00, // 0x55: Nul
    0x00, // 0x56: Nul
    0x44, // 0x57: F11

    0x45, // 0x58: F12
    0x67, // 0x59: Keypad =
    0x00, // 0x5A: Nul
    0xE3, // 0x5B: Left GUI
    0xE7, // 0x5C: Right GUI
    0x68, // 0x5D: F13
    0x69, // 0x5E: F14
    0x6A, // 0x5F: F15

    0x58, // 0x60: Enter
    0x00, // 0x61: Nul
    0x54, // 0x62: Keypad /
    0x00, // 0x63: Nul
    0x00, // 0x64: Nul
    0x00, // 0x65: Nul
    0x4A, // 0x66: Home
    0x52, // 0x67: Up Arrow

    0x4B, // 0x68: Page Up
    0x50, // 0x69: Left Arrow
    0x4F, // 0x6A: Right Arrow
    0x4D, // 0x6B: End
    0x51, // 0x6C: Down Arrow
    0x4E, // 0x6D: Page Down
    0x00, // 0x6E: Nul
    0x4C, // 0x6F: Delete

    0x00, // 0x70: Nul
    0x00, // 0x71: Nul
    0x00, // 0x72: Nul
    0x00, // 0x73: Nul
    0x00, // 0x74: Nul
    0x00, // 0x75: Nul
    0x00, // 0x76: Nul
    0x00, // 0x77: Nul

    0x00, // 0x78: Nul
    0x00, // 0x79: Nul
    0x00, // 0x7A: Nul
    0x00, // 0x7B: Nul
    0x00, // 0x7C: Nul
    0x00, // 0x7D: Nul
    0x00, // 0x7E: Nul
    0x00, // 0x7F: Nul
};

/* msec timeout 0 to poll, timeout -1 to block */
static uint64_t get_event_timeout(void *data, int timeout)
{
    struct pollfd fds[2];
    SDL_Event *event = (SDL_Event *)data;

    fds[0].fd = keybd_fd;
    fds[0].events = POLLIN;
    fds[1].fd = mouse_fd;
    fds[1].events = POLLIN;
    if (poll(fds, 2, timeout) > 0)
	{
        memset(event, 0, sizeof(SDL_Event));
        if (fds[0].revents & POLLIN)
		{
            int c;
            unsigned char buf[1];
            if (read(keybd_fd, buf, sizeof(buf)) > 0)
			{
				c = scan_code_to_hid_table[buf[0] & 0x7f];
#if DEBUG
				if (c == 41) exit(1);      /* exit on ESC! */
#endif
				event->key.keysym.scancode = c;
				event->type = (buf[0] & 0x80) ? SDL_KEYUP : SDL_KEYDOWN;
				return 1;
            }
        }
        if (fds[1].revents & POLLIN)
		{
            int x, y, w, b;
            static int lastx = -1, lasty = -1, lastb = 0;
            if (read_mouse(&x, &y, &w, &b))
			{
                if (b & (BUTTON_SCROLLUP|BUTTON_SCROLLDN))
				{
                    event->wheel.type = SDL_MOUSEWHEEL;
                    event->wheel.direction = SDL_MOUSEWHEEL_NORMAL;
                    if (b & BUTTON_M) event->wheel.x = w * SCROLLFACTOR;
					else event->wheel.y = w * SCROLLFACTOR;
                    lastb = b;
                    return 1;
                }
                if (b != lastb)
				{
                    if ((b & BUTTON_L) ^ (lastb & BUTTON_L))
					{
                        event->button.button = SDL_BUTTON_LEFT;
                        event->type = (b & BUTTON_L)? SDL_MOUSEBUTTONDOWN: SDL_MOUSEBUTTONUP;
                        event->button.state = (b & BUTTON_L)? SDL_PRESSED: SDL_RELEASED;
                        event->button.x = posx;
                        event->button.y = posy;
                        event->button.clicks = 1;
                    }
					else if ((b & BUTTON_R) ^ (lastb & BUTTON_R))
					{
                        event->button.button = SDL_BUTTON_RIGHT;
                        event->type = (b & BUTTON_R)? SDL_MOUSEBUTTONDOWN: SDL_MOUSEBUTTONUP;
                        event->button.state = (b & BUTTON_R)? SDL_PRESSED: SDL_RELEASED;
                        event->button.x = posx;
                        event->button.y = posy;
                        event->button.clicks = 1;
                    }
                    lastb = b;
                    return 1;
                }
                if (x != lastx || y != lasty)
				{
                    event->type = SDL_MOUSEMOTION;
                    posx += x;
                    posy += y;
                    if (posx < 0) posx = 0;
                    if (posy < 0) posy = 0;
                    if (posx >= fb.width) posx = fb.width - 1;
                    if (posy >= fb.height) posy = fb.height - 1;
                    event->motion.x = posx;
                    event->motion.y = posy;
                    event->motion.xrel = x;
                    event->motion.yrel = y;
                    if (b & BUTTON_L) event->motion.state |= SDL_BUTTON_LMASK;
                    if (b & BUTTON_R) event->motion.state |= SDL_BUTTON_RMASK;
                    lastx = x;
                    lasty = y;
                    return 1;
                }
            }
        }
    }
    event->type = 0;
    return 1;
}

uint64_t host_gui_poll_event(SDL_Event *event)
{
    static SDL_Event ev; /* ev.type inited to 0 ! */

    if (event == NULL)
	{
        /* only indicate whether event found, don't dequeue */
        if (!ev.type) get_event_timeout(&ev, 0);
		return ev.type;
    }

    /* always deqeue if event found */
    if (ev.type)
	{
        *event = ev;
        ev.type = 0;
    }
	else get_event_timeout(event, 0);
    return event->type;
}

uint64_t host_gui_init(Rect *r)
{
    if (open_keyboard() < 0)     /* must be before FB open for KDSETMODE to work */
        return -1;
    if (open_mouse() < 0)
        return -1;
    if (open_framebuffer() < 0)  /* printf display won't work after this */
        return -1;
    posx = fb.width / 2;
    posy = fb.height / 2;
    r->w = fb.width;
    r->h = fb.height;

    bb.pixtype = PF_ARGB8888;
    bb.bpp = 32;
    bb.bytespp = 4;
    bb.width = fb.width;
    bb.height = fb.height;
    bb.pitch = bb.width * bb.bytespp;
    bb.size = bb.height * bb.pitch;
    bb.color = 0xffffff;
    bb.mode = 0;
    bb.pixels = malloc(bb.size);
    unassert(bb.pixels);
    memset(bb.pixels, 0, bb.size);

	atexit(host_gui_deinit);
#if DEBUG
    signal(SIGHUP, catch_signals);
    signal(SIGABRT, catch_signals);
    signal(SIGSEGV, catch_signals);
#endif
    return 0;
}

void host_gui_deinit(void)
{
    close_mouse();
    close_framebuffer();
    close_keyboard();        /* must be after FB close for KDSETMODE to work */
    if (bb.pixels) {
        free(bb.pixels);
        bb.pixels = NULL;
    }
}

void (*host_gui_funcs[]) = {
    (void*)host_gui_init,
    (void*)host_gui_deinit,
    (void*)host_gui_box,
    (void*)host_gui_filled_box,
    (void*)host_gui_blit,
    (void*)host_gui_set_clip,
    (void*)host_gui_set_color,
    (void*)host_gui_texture_color,
    (void*)host_gui_destroy_texture,
    (void*)host_gui_create_texture,
    (void*)host_gui_begin_composite,
    (void*)host_gui_end_composite,
    (void*)host_gui_flush,
    (void*)host_gui_resize,
    (void*)host_gui_poll_event,
};

/* open linux framebuffer*/
static int open_framebuffer(void)
{
    int type, visual, extra;
    struct fb_fix_screeninfo  fb_fix;
    struct fb_var_screeninfo fb_var;

    char *env = getenv("FRAMEBUFFER");
    frame_fd = open(env? env: PATH_FRAMEBUFFER, O_RDWR);
    if (frame_fd < 0) {
        printf("Error opening framebuffer %s: %m. Check kernel config\n",
                env? env: PATH_FRAMEBUFFER);
        return -1;
    }
    /* get dynamic framebuffer info*/
    if (ioctl(frame_fd, FBIOGET_FSCREENINFO, &fb_fix) == -1 ||
        ioctl(frame_fd, FBIOGET_VSCREENINFO, &fb_var) == -1) {
            printf("Can't get framebuffer info: %m\n");
            goto fail;
    }

    /* setup screen device from framebuffer info*/
    type = fb_fix.type;
    visual = fb_fix.visual;

    fb.pixtype = PF_UNKNOWN;
    fb.width = fb_var.xres;
    fb.height = fb_var.yres;
    fb.bpp = fb_var.bits_per_pixel;
    fb.bytespp = fb.bpp >> 3;
    fb.pitch = fb_fix.line_length;
    fb.size = fb.height * fb.pitch;

    /* set pixel format*/
    if (type == FB_TYPE_PACKED_PIXELS &&
       (visual == FB_VISUAL_TRUECOLOR || visual == FB_VISUAL_DIRECTCOLOR)) {
        switch (fb.bpp) {
        case 8:
            fb.pixtype = PF_PALETTE;
            break;
        case 16:
            if (fb_var.green.length == 5) {
                fb.pixtype = PF_RGB555;
                fb.bpp = 15;
            } else {
                fb.pixtype = PF_RGB565;
            }
            break;
        case 18:
        case 24:
            fb.pixtype = PF_BGR888;
            break;
        case 32:
            fb.pixtype = PF_ARGB8888;
            break;
        }
    }
    printf("%dx%dx%dbpp (a%d:%d, r%d:%d, g%d:%d, b%d:%d) ", fb.width, fb.height, fb.bpp,
            fb_var.transp.offset, fb_var.transp.length,
            fb_var.red.offset, fb_var.red.length,
            fb_var.green.offset, fb_var.green.length,
            fb_var.blue.offset, fb_var.blue.length);
    printf("pitch %d type %d visual %d pixtype %d\n",
            fb.pitch, type, visual, fb.pixtype);

    if (fb.pixtype <= PF_UNKNOWN) {
        printf("Unsupported framebuffer type\n");
        goto fail;
    }

    /* mmap framebuffer into this address space*/
    extra = getpagesize() - 1;
    fb.size = (fb.size + extra) & ~extra;       /* extend to page boundary*/

    fb.pixels = mmap(NULL, fb.size, PROT_READ|PROT_WRITE, MAP_SHARED, frame_fd, 0);
    if (fb.pixels == NULL || fb.pixels == (unsigned char *)-1) {
        printf("Can't mmap %s: %m\n", PATH_FRAMEBUFFER);
        goto fail;
    }
    fb.color = 0xffffff;
    fb.mode = 0;

    /* switch console to graphic mode, no more printf error messages */
    if (keybd_fd >= 0) {
#if !DEBUG
        ioctl(keybd_fd, KDSETMODE, KD_GRAPHICS);
#endif
    }

    memset(fb.pixels, 0, fb.size);
    host_gui_set_clip(NULL);
    return frame_fd;

fail:
    close(frame_fd);
    frame_fd = -1;
    host_gui_deinit();
    exit(1);
}

static void close_framebuffer(void)
{
    if (frame_fd >= 0) {
        munmap(fb.pixels, fb.size);
        if (keybd_fd >= 0) {
            ioctl(keybd_fd, KDSETMODE, KD_TEXT);
        }
        close(frame_fd);
     }
    frame_fd = -1;
}

static int open_mouse(void)
{
    /* sequence to mouse device to send ImPS/2 events*/
    static const unsigned char imps2[] = { 0xf3, 200, 0xf3, 100, 0xf3, 80 };
    unsigned char buf[4];

    mouse_fd = open(PATH_MOUSE, O_RDWR | O_NONBLOCK);
    if (mouse_fd < 0) {
        printf("Can't open mouse %s: %m\n", PATH_MOUSE);
        return -1;
    }

    /* try to switch the mouse to ImPS/2 protocol*/
    if (write(mouse_fd, imps2, sizeof(imps2)) != sizeof(imps2))
        /*printf("Can't switch to ImPS/2 protocol\n")*/;
    if (read(mouse_fd, buf, 4) != 1 || buf[0] != 0xF4)
        /*printf("Failed to switch to ImPS/2 protocol.\n")*/;

    return mouse_fd;
}

static void close_mouse(void)
{
    if (mouse_fd >= 0)
        close(mouse_fd);
    mouse_fd = -1;
}

/* IntelliMouse PS/2 protocol uses four byte reports
 * (PS/2 protocol omits last byte):
 *      Bit   7     6     5     4     3     2     1     0
 * --------+-----+-----+-----+-----+-----+-----+-----+-----
 *  Byte 0 |  0     0   Neg-Y Neg-X   1    Mid  Right Left
 *  Byte 1 |  X     X     X     X     X     X     X     X
 *  Byte 2 |  Y     Y     Y     Y     Y     Y     Y     Y
 *  Byte 3 |  W     W     W     W     W     W     W     W
 *
 * XXXXXXXX, YYYYYYYY, and WWWWWWWW are 8-bit two's complement values
 * indicating changes in x-coordinate, y-coordinate, and scroll wheel.
 * That is, 0 = no change, 1..127 = positive change +1 to +127,
 * and 129..255 = negative change -127 to -1.
 *
 * Left, Right, and Mid are the three button states, 1 if being depressed.
 * Neg-X and Neg-Y are set if XXXXXXXX and YYYYYYYY are negative, respectively.
 */
static int read_mouse(int *dx, int *dy, int *dw, int *bp)
{
    int n, x, y, w, left, middle, right, button;
    unsigned char data[4];

    n = read(mouse_fd, data, sizeof(data));
    if (n != 3 && n != 4)
        return 0;

    button = 0;
    left = data[0] & 0x1;
    right = data[0] & 0x2;
    middle = data[0] & 0x4;

    if (left)   button |= BUTTON_L;
    if (middle) button |= BUTTON_M;
    if (right)  button |= BUTTON_R;

    x =   (signed char) data[1];
    y = - (signed char) data[2];  /* y axis flipped between conventions */
    if (n == 4) {
        w = (signed char) data[3];
        if (w > 0)
            button |= BUTTON_SCROLLUP;
        if (w < 0)
            button |= BUTTON_SCROLLDN;
    }
    *dx = x;
    *dy = y;       
    *dw = w;
    *bp = button;
    return 1;
}

static int old_kbd_mode;

static int open_keyboard(void)
{
    char *path;
    struct termios new;

    if (!(path = getenv("CONSOLE")))
        path = PATH_KEYBOARD;
    keybd_fd = open(path, O_NONBLOCK);
    if (keybd_fd < 0) {
        printf("Can't open %s, may need root or tty group permissions\n", path);
        return -1;
    }

	ioctl(keybd_fd, KDGKBMODE, &old_kbd_mode);
    tcgetattr(keybd_fd, &orig);
	ioctl(keybd_fd, KDSKBMODE, K_MEDIUMRAW);
    new = orig;
    new.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    new.c_iflag &= ~(ICRNL | INPCK | ISTRIP | IXON | BRKINT);
    new.c_cflag &= ~(CSIZE | PARENB);
    new.c_cflag |= CS8;
    new.c_cc[VMIN] = 1;     /* =1 required for lone ESC key processing */
    new.c_cc[VTIME] = 0;
    tcsetattr(keybd_fd, TCSAFLUSH, &new);
    return keybd_fd;
}

static void close_keyboard(void)
{
    if (keybd_fd >= 0)
	{
		ioctl(keybd_fd, KDSKBMODE, old_kbd_mode);
        tcsetattr(keybd_fd, TCSANOW, &orig);
        close(keybd_fd);
    }
    keybd_fd = -1;
}

#endif /* _HOST_GUI == 1 */
#endif /* defined(_HOST_GUI) */

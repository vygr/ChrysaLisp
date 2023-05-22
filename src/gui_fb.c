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
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/fb.h>
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

typedef struct rect {
    int x, y;
    int w, h;
} Rect;

typedef struct drawable {
    int pixtype;                /* pixel format */
    int bpp;                    /* bits per pixel */
    int bytespp;                /* bytes per pixel */
    int width;                  /* width in pixels */
    int height;                 /* height in pixels */
    int pitch;                  /* stride in bytes, offset to next pixel row */
    int size;                   /* total size in bytes */
    unsigned char *pixels;      /* pixel data */
    uint32_t r, g, b;           /* premul colors to use for color mod blit */
    uint32_t color;             /* combined premul colors or 0x00fffff for source blend */
    uint8_t data[];             /* texture data allocated in single malloc */
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
    if (r.w < 1 || r.h < 1) return NULL;
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
    texture->r = r;
    texture->g = g;
    texture->b = b;
    texture->color = (r << 16) + (g << 8)  + b;
}

/* allocate drawable for passed data and return a handle to it */
Texture *host_gui_create_texture(void *data, uint64_t width, uint64_t height, uint64_t pitch, uint64_t mode)
{
    Texture *t;

    int size = height * pitch;
    t = malloc(sizeof(Texture) + size);
    unassert(t);
    t->pixtype = bb.pixtype;
    t->bpp = bb.bpp;
    t->bytespp = bb.bytespp;
    t->width = width;
    t->height = height;
    t->pitch = pitch;
    t->size = size;
    t->pixels = t->data;
    t->r = t->g = t->b = 0xff;
    t->color = 0xffffff;
    memcpy(t->pixels, data, t->size);
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
    if (rect->w < 1 || rect->h < 1) return;
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

/* fast source copy blit, no clipping */
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

/* source copy conversion blit ARGB888 -> RGB565 */
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

/* premultiplied alpha blend or color mod blit, no clipping */
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
                if (ts->color == 0xffffff) {        /* premul blend from source */
                    if (sa < 0xff000000) {
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
                } else {                            /* color mod blend (glyphs) */
                    pixel_t sr = sa & 0xff0000;
                    pixel_t sg = sa & 0x00ff00;
                    pixel_t sb = sa & 0x0000ff;
                    sr = (sr * ts->r >> 8) & 0xff0000;
                    sg = (sg * ts->g >> 8) & 0x00ff00;
                    sb =  sb * ts->b >> 8;
                    if (sa < 0xff000000) {
                        pixel_t da = 0xff - (sa >> 24);
                        pixel_t drb = *dst;
                        pixel_t dg = drb & 0x00ff00;
                               drb = drb & 0xff00ff;
                        drb = ((drb * da >> 8) & 0xff00ff) + sr + sb;
                        dg =   ((dg * da >> 8) & 0x00ff00) + sg;
                        *dst = drb + dg;
                    } else {
                        *dst = sr + sg + sb;
                    }
                }
            }
            dst++;
        } while (--x > 0);
        dst = (pixel_t *)((uint8_t *)dst + dspan);
        src = (pixel_t *)((uint8_t *)src + sspan);
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
    blit_blend(texture, &sr2, &bb, cr);
}

#define BUTTON_L        0x01      /* left button*/
#define BUTTON_R        0x02      /* right button*/
#define BUTTON_M        0x10      /* middle*/
#define BUTTON_SCROLLUP 0x20      /* wheel up*/
#define BUTTON_SCROLLDN 0x40      /* wheel down*/


/*
 * Lookup table to convert ascii characters in to keyboard scan codes
 * Format: most signifficant bit indicates if scan code should be sent with shift modifier
 * remaining 7 bits are to be used as scan code number.
 */
const uint8_t ascii_to_scan_code_table[128] = {
  /* ASCII:   0 */ 0,
  /* ASCII:   1 */ 0,
  /* ASCII:   2 */ 0,
  /* ASCII:   3 */ 0,
  /* ASCII:   4 */ 0,
  /* ASCII:   5 */ 0,
  /* ASCII:   6 */ 0,
  /* ASCII:   7 */ 0,
  /* ASCII:   8 */ 42,
  /* ASCII:   9 */ 43,
  /* ASCII:  10 */ 40,
  /* ASCII:  11 */ 0,
  /* ASCII:  12 */ 0,
  /* ASCII:  13 */ 0,
  /* ASCII:  14 */ 0,
  /* ASCII:  15 */ 0,
  /* ASCII:  16 */ 0,
  /* ASCII:  17 */ 0,
  /* ASCII:  18 */ 0,
  /* ASCII:  19 */ 0,
  /* ASCII:  20 */ 0,
  /* ASCII:  21 */ 0,
  /* ASCII:  22 */ 0,
  /* ASCII:  23 */ 0,
  /* ASCII:  24 */ 0,
  /* ASCII:  25 */ 0,
  /* ASCII:  26 */ 0,
  /* ASCII:  27 */ 41,
  /* ASCII:  28 */ 0,
  /* ASCII:  29 */ 0,
  /* ASCII:  30 */ 0,
  /* ASCII:  31 */ 0,
  /* ASCII:  32 */ 44,
  /* ASCII:  33 */ 158,
  /* ASCII:  34 */ 180,
  /* ASCII:  35 */ 160,
  /* ASCII:  36 */ 161,
  /* ASCII:  37 */ 162,
  /* ASCII:  38 */ 164,
  /* ASCII:  39 */ 52,
  /* ASCII:  40 */ 166,
  /* ASCII:  41 */ 167,
  /* ASCII:  42 */ 165,
  /* ASCII:  43 */ 174,
  /* ASCII:  44 */ 54,
  /* ASCII:  45 */ 45,
  /* ASCII:  46 */ 55,
  /* ASCII:  47 */ 56,
  /* ASCII:  48 */ 39,
  /* ASCII:  49 */ 30,
  /* ASCII:  50 */ 31,
  /* ASCII:  51 */ 32,
  /* ASCII:  52 */ 33,
  /* ASCII:  53 */ 34,
  /* ASCII:  54 */ 35,
  /* ASCII:  55 */ 36,
  /* ASCII:  56 */ 37,
  /* ASCII:  57 */ 38,
  /* ASCII:  58 */ 179,
  /* ASCII:  59 */ 51,
  /* ASCII:  60 */ 182,
  /* ASCII:  61 */ 46,
  /* ASCII:  62 */ 183,
  /* ASCII:  63 */ 184,
  /* ASCII:  64 */ 159,
  /* ASCII:  65 */ 132,
  /* ASCII:  66 */ 133,
  /* ASCII:  67 */ 134,
  /* ASCII:  68 */ 135,
  /* ASCII:  69 */ 136,
  /* ASCII:  70 */ 137,
  /* ASCII:  71 */ 138,
  /* ASCII:  72 */ 139,
  /* ASCII:  73 */ 140,
  /* ASCII:  74 */ 141,
  /* ASCII:  75 */ 142,
  /* ASCII:  76 */ 143,
  /* ASCII:  77 */ 144,
  /* ASCII:  78 */ 145,
  /* ASCII:  79 */ 146,
  /* ASCII:  80 */ 147,
  /* ASCII:  81 */ 148,
  /* ASCII:  82 */ 149,
  /* ASCII:  83 */ 150,
  /* ASCII:  84 */ 151,
  /* ASCII:  85 */ 152,
  /* ASCII:  86 */ 153,
  /* ASCII:  87 */ 154,
  /* ASCII:  88 */ 155,
  /* ASCII:  89 */ 156,
  /* ASCII:  90 */ 157,
  /* ASCII:  91 */ 47,
  /* ASCII:  92 */ 49,
  /* ASCII:  93 */ 48,
  /* ASCII:  94 */ 163,
  /* ASCII:  95 */ 173,
  /* ASCII:  96 */ 53,
  /* ASCII:  97 */ 4,
  /* ASCII:  98 */ 5,
  /* ASCII:  99 */ 6,
  /* ASCII: 100 */ 7,
  /* ASCII: 101 */ 8,
  /* ASCII: 102 */ 9,
  /* ASCII: 103 */ 10,
  /* ASCII: 104 */ 11,
  /* ASCII: 105 */ 12,
  /* ASCII: 106 */ 13,
  /* ASCII: 107 */ 14,
  /* ASCII: 108 */ 15,
  /* ASCII: 109 */ 16,
  /* ASCII: 110 */ 17,
  /* ASCII: 111 */ 18,
  /* ASCII: 112 */ 19,
  /* ASCII: 113 */ 20,
  /* ASCII: 114 */ 21,
  /* ASCII: 115 */ 22,
  /* ASCII: 116 */ 23,
  /* ASCII: 117 */ 24,
  /* ASCII: 118 */ 25,
  /* ASCII: 119 */ 26,
  /* ASCII: 120 */ 27,
  /* ASCII: 121 */ 28,
  /* ASCII: 122 */ 29,
  /* ASCII: 123 */ 175,
  /* ASCII: 124 */ 177,
  /* ASCII: 125 */ 176,
  /* ASCII: 126 */ 181,
  /* ASCII: 127 */ 0
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
    if (poll(fds, 2, timeout) >= 0) {
        memset(event, 0, sizeof(SDL_Event));
        if (fds[0].revents & POLLIN) {
            unsigned char buf[1];
            if (read(keybd_fd, buf, 1) == 1) {
                int c = buf[0];
#if DEBUG
                if (c == 033) exit(0);      /* exit on ESC! */
#endif
                if (c == 0x7F) c = '\b';
                if (c == '\r') c = '\n';
                event->type = SDL_KEYDOWN;
                event->key.state = SDL_PRESSED;
                if (c <= ' ' & c != '\n' && c != '\b' && c != '\t') {
                    event->key.keysym.mod = KMOD_CTRL;
                    c += 'a' - 1;
                }
                event->key.keysym.sym = c;
                int scancode = ascii_to_scan_code_table[c & 0x7f];
                event->key.keysym.scancode = scancode & 127;
                if (scancode & 0x80) event->key.keysym.mod |= KMOD_SHIFT;
                return 1;
            }
        }
        if (fds[1].revents & POLLIN) {
            int x, y, w, b;
            static int lastx = -1, lasty = -1, lastb = 0;
            if (read_mouse(&x, &y, &w, &b)) {
                if (b & (BUTTON_SCROLLUP|BUTTON_SCROLLDN)) {
                    event->wheel.type = SDL_MOUSEWHEEL;
                    event->wheel.direction = SDL_MOUSEWHEEL_NORMAL;
                    if (b & BUTTON_M) {
                        event->wheel.x = w * SCROLLFACTOR;
                    } else {
                        event->wheel.y = w * SCROLLFACTOR;
                    }
                    lastb = b;
                    return 1;
                }
                if (b != lastb) {
                    if ((b & BUTTON_L) ^ (lastb & BUTTON_L)) {
                        event->button.button = SDL_BUTTON_LEFT;
                        event->type = (b & BUTTON_L)? SDL_MOUSEBUTTONDOWN: SDL_MOUSEBUTTONUP;
                        event->button.state = (b & BUTTON_L)? SDL_PRESSED: SDL_RELEASED;
                        event->button.x = posx;
                        event->button.y = posy;
                        event->button.clicks = 1;
                    } else if ((b & BUTTON_R) ^ (lastb & BUTTON_R)) {
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
                if (x != lastx || y != lasty) {
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
    static SDL_Event ev;
    static int saved = 0;

    if (event == NULL) {
        /* only indicate whether event found, don't dequeue */
        if (!saved)
            get_event_timeout(&ev, 0);
        if (ev.type != 0) {
            saved = 1;
            return 1;
        }
        return 0;
    }

    /* alwaus deqeue if event found */
    if (saved) {
        *event = ev;
        saved = 0;
    } else {
        get_event_timeout(event, 0);
    }
    return event->type != 0;
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
    bb.r = bb.g = bb.b = 0xff;
    bb.color = 0xffffff;
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
    fb.r = fb.g = fb.b = 0xff;
    fb.color = 0xffffff;

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

    tcgetattr(keybd_fd, &orig);
    new = orig;
    new.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    new.c_iflag &= ~(ICRNL | INPCK | ISTRIP | IXON | BRKINT);
    new.c_cflag &= ~(CSIZE | PARENB);
    new.c_cflag |= CS8;
    new.c_cc[VMIN] = 0;
    new.c_cc[VTIME] = 0;
    tcsetattr(keybd_fd, TCSAFLUSH, &new);
    return keybd_fd;
}

static void close_keyboard(void)
{
    if (keybd_fd >= 0) {
        tcsetattr(keybd_fd, TCSANOW, &orig);
        close(keybd_fd);
    }
    keybd_fd = -1;
}
#endif /* _HOST_GUI == 1 */
#endif /* defined(_HOST_GUI) */

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

#include "gr.h"
#include "sdl-dummy.h"

#define PATH_FRAMEBUFFER    "/dev/fb0"          /* or env "FRAMEBUFFER" */
#define PATH_MOUSE          "/dev/input/mice"
#define PATH_KEYBOARD       "/dev/tty"          /* or env "CONSOLE" */

static struct termios orig;
static int frame_fd = -1;           /* framebuffer file handle */
static int mouse_fd = -1;
static int keybd_fd = -1;;
static Drawable fb;                 /* hardware framebuffer */
static Drawable bb;                 /* back buffer */
static Rect clip;

static int OpenFramebuffer(void);
static int OpenMouse(void);
static int ReadMouse(int *dx, int *dy, int *bp);
static int OpenKeyboard(void);
static void CloseKeyboard(void);
static void blit_blend(Drawable *src, const Rect *srect, Drawable *dst, const Rect *drect);
static void blit_srccopy(Drawable *src, const Rect *srect, Drawable *dst, const Rect *drect);

#define DEBUG   1

/* debug routines exit graphics mode for error message */
void unassert_handler(char *msg, char *file, int line);
#define unassert(a)   if (!(a)) unassert_handler(#a,__FILE__, __LINE__)

void unassert_handler(char *msg, char *file, int line)
{
    DeInit();
    printf("Assertion failed: %s at %s:%d\n", msg, file, line);
    exit(255);
}

void sighup(int signo)
{
    DeInit();
    printf("SIGNAL %d\n", signo);
    exit(1);
}

void host_gui_begin_composite(void)
{
}

void host_gui_end_composite(void)
{
}

void Flush(const Rect *r)
{
    Rect cr;
    cr.x = 0;
    cr.y = 0;
    cr.w = fb.width;
    cr.h = fb.height;
    unassert(r);
    blit_srccopy(&bb, &cr, &fb, &cr);
    //memcpy(fb.pixels, bb.pixels, fb.size);
}

void host_gui_resize(uint64_t w, uint64_t h)
{
    /* FB display cannot be resized */
}

/* adjust passed rect to current clip rectangle */
static Rect *ClipRect(const Rect *rect)
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
void SetClip(const Rect *rect)
{
    // store as x, y, x1, y1
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

typedef uint32_t COLORVAL;      /* color in 0xAARRGGBB register format */
typedef uint32_t PIXELVAL;      /* pixel format B, G, R, A in memory */

/* create COLORVAL (0xAARRGGBB register format)*/
#define ARGB(a,r,g,b) ((COLORVAL)                  \
                (((uint32_t)(uint8_t)(b)) <<  0) | \
                (((uint32_t)(uint8_t)(g)) <<  8) | \
                (((uint32_t)(uint8_t)(r)) << 16) | \
                (((uint32_t)(uint8_t)(a)) << 24))
#define RGB(r,g,b)    ARGB(255,(r),(g),(b))     /* ARGB 255 alpha */
#define COLORVAL_TO_PIXELVAL(c)     (c)         /* no conversion! */

static PIXELVAL defColor = COLORVAL_TO_PIXELVAL(RGB(0, 0, 255));

/* set color for Drawables */
void SetColor(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
    //FIXME premul colors with alpha
    defColor = COLORVAL_TO_PIXELVAL(ARGB(a, r, g, b));
}

void SetTextureColorMod(Texture *texture, uint8_t r, uint8_t g, uint8_t b)
{
    texture->r = r;
    texture->g = g;
    texture->b = b;
    texture->color = (r << 16) + (g << 8)  + b;
}

/* allocate drawable for passed data and return a handle to it */
Texture *CreateTexture(void *data, uint64_t width, uint64_t height, uint64_t pitch, uint64_t mode)
{
    Texture *t;

    t = malloc(sizeof(Texture));
    unassert(t);
    t->pixtype = fb.pixtype;
    t->bpp = fb.bpp;
    t->width = width;
    t->height = height;
    t->pitch = pitch;
    t->size = height * t->pitch;
    t->pixels = malloc(t->size);
    unassert(t->pixels);
    t->r = t->g = t->b = 0xff;
    t->color = 0xffffff;
    memcpy(t->pixels, data, t->size);
    return t;
}

void DestroyTexture(Texture *texture)
{
    unassert(texture);
    free(texture->pixels);
    free(texture);
}

/* fill rectangle with current color */
void FillRect(const Rect *rect)
{
    pixel_t color_a = defColor >> 24;
    if (color_a == 0) return;
    Rect *r = ClipRect(rect);
    if (!r) return;
    pixel_t *dst = (pixel_t *)(bb.pixels + r->y * bb.pitch + r->x * (bb.bpp >> 3));
    int span = (bb.pitch >> 2) - r->w;      /* in pixels, not bytes */
    
    int h = r->h;
    if (color_a == 0xff) {  /* source copy */
        do {
            int w = r->w;
            do {
                *dst++ = defColor;
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
				drb = ((drb * da >> 8) & 0xff00ff) + (defColor & 0xff00ff);
				dg =   ((dg * da >> 8) & 0x00ff00) + (defColor & 0x00ff00);
				*dst++ = drb + dg;
            } while (--w > 0);
            dst += span;
        } while (--h > 0);
    }
    //UpdateRect(r);
}

/* draw rectangle - this function isn't required in a driver */
void host_gui_box(const Rect *rect)
{
	/* just call filled box and let it do the clipping and drawing */
	Rect r = *rect;
	if (rect->w < 1 || rect->h < 1) return;
	r.h = 1;
	FillRect(&r);
	if (rect->h <= 1) return;

	r.y = rect->y + rect->h - 1;
	FillRect(&r);
	if (rect->h <= 2) return;

	r.y = rect->y + 1;
	r.w = 1;
	r.h = rect->h - 2;
	FillRect(&r);
	if (rect->w <= 1) return;

	r.x = rect->x + rect->w - 1;
	FillRect(&r);
}

/* fast source copy blit, no clipping */
static void blit_srccopy(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    pixel_t *dst = (pixel_t *)(td->pixels + drect->y * td->pitch + drect->x * (td->bpp >> 3));
    pixel_t *src = (pixel_t *)(ts->pixels + srect->y * ts->pitch + srect->x * (ts->bpp >> 3));
    int span = drect->w * (td->bpp >> 3);
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
    //UpdateRect(drect);
}

/* premultiplied alpha blend or color mod blit, no clipping done */
static void blit_blend(Drawable *ts, const Rect *srect, Drawable *td, const Rect *drect)
{
    //unassert(srect->w == drect->w);   //FIXME check why needs commenting out
    /* src and dst height can differ, will use dst height for drawing */
    pixel_t *dst = (pixel_t *)(td->pixels + drect->y * td->pitch + drect->x * (td->bpp >> 3));
    pixel_t *src = (pixel_t *)(ts->pixels + srect->y * ts->pitch + srect->x * (ts->bpp >> 3));
    int span = drect->w * (td->bpp >> 3);
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
					pixel_t sg = sa & 0xff00;
                    pixel_t sb = sa & 0xff;
                    sr = (sr * ts->r >> 8) & 0xff0000;
                    sg = (sg * ts->g >> 8) & 0xff00;
                    sb = sb * ts->b >> 8;
                    if (sa < 0xff000000) {
                        pixel_t da = 0xff - (sa >> 24);
                        pixel_t drb = *dst;
                        pixel_t dg = drb & 0xff00;
                        drb = drb & 0xff00ff;
                        drb = ((drb * da >> 8) & 0xff00ff) + sr + sb;
                        dg = ((dg * da >> 8) & 0xff00) + sg;
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
    //UpdateRect(drect);
}

/* draw pixels from passed texture handle */
void BlitTexture(Texture *texture, const Rect *srect, const Rect *drect)
{
    unassert(texture);
    unassert(srect->w == drect->w);
    unassert(srect->h == drect->h);
    Rect *cr = ClipRect(drect);
    if (!cr) return;
    Rect sr2 = *srect;
    if (clip.x > drect->x) sr2.x += clip.x - drect->x;
    if (clip.y > drect->y) sr2.y += clip.y - drect->y;
    blit_blend(texture, &sr2, &bb, cr);
}

/* draw pixels from passed drawable (not used by CL) */
void BlitDrawable(Drawable *d, int x, int y, int width, int height)
{
    Rect r;
    if (!width) width = d->width;
    if (!height) height = d->height;
    r.x = x;
    r.y = y;
    r.w = width;
    r.h = height;

#if 1
    Rect *cr = ClipRect(&r);
    if (cr) {
        d->r = d->g = d->b = 0xff;
        d->color = 0xffffff;
        blit_blend(d, cr, &bb, cr);
    }
#else   /* used to test code for textures */
    Texture *t = CreateTexture(d->pixels, d->width, d->height, d->pitch, 0);
    unassert(t);
    BlitTexture(t, cr, cr);
    DestroyTexture(t);
#endif
}

#define MWBUTTON_L        0x01      /* left button*/
#define MWBUTTON_R        0x02      /* right button*/
#define MWBUTTON_M        0x10      /* middle*/
#define MWBUTTON_SCROLLUP 0x20      /* wheel up*/
#define MWBUTTON_SCROLLDN 0x40      /* wheel down*/


// Lookup table to convert ascii characters in to keyboard scan codes
// Format: most signifficant bit indicates if scan code should be sent with shift modifier
// remaining 7 bits are to be used as scan code number.

const uint8_t ascii_to_scan_code_table[] = {
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
static uint64_t GetEventTimeout(void *data, int timeout)
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
                if (c == 033) exit(1);
                if (c == 0x7F) c = '\b';
                if (c == '\r') c = '\n';
                event->type = SDL_KEYDOWN;
                event->key.state = SDL_PRESSED;
                int scancode = ascii_to_scan_code_table[c & 0x7f];
                event->key.keysym.scancode = scancode & 127;
                event->key.keysym.sym = c;
                if (scancode & 0x80) event->key.keysym.mod = KMOD_SHIFT;
                return 1;
            }
        }
        if (fds[1].revents & POLLIN) {
            int x, y, b;
            static int lastx = -1, lasty = -1, lastb = 0;
            static int posx, posy;
            if (ReadMouse(&x, &y, &b)) {
                if (b != lastb) {
                    event->button.clicks = 1;
                    event->button.x = posx;
                    event->button.y = posy;
                    if ((b & MWBUTTON_L) ^ (lastb & MWBUTTON_L)) {
                        event->button.button = SDL_BUTTON_LEFT;
                        event->type = (b & MWBUTTON_L)? SDL_MOUSEBUTTONDOWN: SDL_MOUSEBUTTONUP;
                    } else if ((b & MWBUTTON_R) ^ (lastb & MWBUTTON_R)) {
                        event->button.button = SDL_BUTTON_RIGHT;
                        event->type = (b & MWBUTTON_R)? SDL_MOUSEBUTTONDOWN: SDL_MOUSEBUTTONUP;
                    }
                    lastb = b;
                    if (event->button.button) {
                        event->button.state = (event->type == SDL_MOUSEBUTTONDOWN)? SDL_PRESSED: SDL_RELEASED;
                        //printf("Mouse %d button %d\n", event->button.button, event->type);
                        return 1;
                    }
                    event->type = 0;
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
                    if (b & MWBUTTON_L) event->motion.state |= 1;
                    if (b & MWBUTTON_R) event->motion.state |= 4;
                    //printf("Mouse motion %d,%d\n", posx, posy);
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

uint64_t PollEvent(SDL_Event *event)
{
    static SDL_Event ev;
    static int saved = 0;

    if (event == NULL) {
        /* only indicate whether event found, don't dequeue */
        if (!saved)
            GetEventTimeout(&ev, 0);
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
        GetEventTimeout(event, 0);
    }
    return event->type != 0;
}

/* block for next event (not used by CL) */
uint64_t WaitEvent(void *event)
{
    GetEventTimeout(event, -1);
    return 1;
}

uint64_t Init(Rect *r)
{
    if (OpenKeyboard() < 0)     /* must be before FB open for KDSETMODE to work */
        return -1;
    if (OpenMouse() < 0)
        return -1;
    if (OpenFramebuffer() < 0)  /* printf display won't work after this */
        return -1;
    bb = fb;
    bb.pixels = malloc(bb.size);
    unassert(bb.pixels);
    memset(bb.pixels, 0, bb.size);
    atexit(DeInit);
    signal(SIGHUP, sighup);
    signal(SIGABRT, sighup);
    signal(SIGSEGV, sighup);
    return 0;
}

void (*host_gui_funcs[]) = {
    (void*)Init,
    (void*)DeInit,
    (void*)host_gui_box,
    (void*)FillRect,
    (void*)BlitTexture,
    (void*)SetClip,
    (void*)SetColor,
    (void*)SetTextureColorMod,
    (void*)DestroyTexture,
    (void*)CreateTexture,
    (void*)host_gui_begin_composite,
    (void*)host_gui_end_composite,
    (void*)Flush,
    (void*)host_gui_resize,
    (void*)PollEvent,
};

/* open linux framebuffer*/
static int OpenFramebuffer(void)
{
    int type, visual;
    int extra = getpagesize() - 1;
    struct fb_fix_screeninfo  fb_fix;
    struct fb_var_screeninfo fb_var;

    char *env = getenv("FRAMEBUFFER");
    frame_fd = open(env? env: PATH_FRAMEBUFFER, O_RDWR);
    if(frame_fd < 0) {
        printf("Error opening %s: %m. Check kernel config\n", env? env: PATH_FRAMEBUFFER);
        return -1;
    }
    /* get dynamic framebuffer info*/
    if (ioctl(frame_fd, FBIOGET_FSCREENINFO, &fb_fix) == -1 ||
        ioctl(frame_fd, FBIOGET_VSCREENINFO, &fb_var) == -1) {
            printf("Can't get framebuffer specs: %m\n");
            goto fail;
    }

    /* setup screen device from framebuffer info*/
    type = fb_fix.type;
    visual = fb_fix.visual;

    fb.width = fb_var.xres;
    fb.height = fb_var.yres;
    fb.bpp = fb_var.bits_per_pixel;
    fb.pitch = fb_fix.line_length;
    fb.size = fb.height * fb.pitch;

    /* set pixel format*/
    if(type == FB_TYPE_PACKED_PIXELS &&
       (visual == FB_VISUAL_TRUECOLOR || visual == FB_VISUAL_DIRECTCOLOR)) {
        switch(fb.bpp) {
        case 16:
            if (fb_var.green.length == 5) {
                fb.pixtype = MWPF_TRUECOLOR555;
            } else {
                fb.pixtype = MWPF_TRUECOLOR565;
            }
            break;
        case 18:
        case 24:
            fb.pixtype = MWPF_TRUECOLORBGR;
            break;
        case 32:
            fb.pixtype = MWPF_TRUECOLORARGB;
            break;
        default:
            printf("Unsupported framebuffer bpp: %d\n", fb.bpp);
            goto fail;
        }
    } else {
        printf("Palette modes not supported\n");
        goto fail;
    }
    printf("%dx%dx%dbpp pitch %d type %d visual %d pixtype %d\n", fb.width, fb.height,
        (fb.pixtype == MWPF_TRUECOLOR555)? 15: fb.bpp, fb.pitch, type, visual,
        fb.pixtype);

    /* mmap framebuffer into this address space*/
    fb.size = (fb.size + extra) & ~extra;       /* extend to page boundary*/

    fb.pixels = mmap(NULL, fb.size, PROT_READ|PROT_WRITE, MAP_SHARED, frame_fd, 0);
    if(fb.pixels == NULL || fb.pixels == (unsigned char *)-1) {
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
    SetClip(NULL);
    return frame_fd;

fail:
    close(frame_fd);
    frame_fd = -1;
    return -1;
}

static void CloseFramebuffer(void)
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

static int OpenMouse(void)
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

static void CloseMouse(void)
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
static int ReadMouse(int *dx, int *dy, int *bp)
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

    if (left)   button |= MWBUTTON_L;
    if (middle) button |= MWBUTTON_M;
    if (right)  button |= MWBUTTON_R;

    x =   (signed char) data[1];
    y = - (signed char) data[2];  // y axis flipped between conventions
    if (n == 4) {
        w = (signed char) data[3];
        if (w > 0)
            button |= MWBUTTON_SCROLLUP;
        if (w < 0)
            button |= MWBUTTON_SCROLLDN;
    }
    *dx = x;
    *dy = y;       
    *bp = button;
    return 1;
}

static int OpenKeyboard(void)
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

static void CloseKeyboard(void)
{
    if (keybd_fd >= 0) {
        tcsetattr(keybd_fd, TCSANOW, &orig);
        close(keybd_fd);
    }
    keybd_fd = -1;
}

void DeInit(void)
{
    CloseMouse();
    CloseFramebuffer();
    CloseKeyboard();        /* must be after FB close for KDSETMODE to work */
}

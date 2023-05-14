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

static struct drawable fb;      /* hardware framebuffer */
static struct termios orig;
static int frame_fd = -1;       /* framebuffer file handle */
static int mouse_fd = -1;
static int keybd_fd = -1;;
static int clipminx, clipmaxx;
static int clipminy, clipmaxy;

static int OpenFramebuffer(void);
static int OpenMouse(void);
static int ReadMouse(int *dx, int *dy, int *bp);
static int OpenKeyboard(void);
static void CloseKeyboard(void);

/* Mouse button bits */
#define MWBUTTON_L        0x01      /* left button*/
#define MWBUTTON_R        0x02      /* right button*/
#define MWBUTTON_M        0x10      /* middle*/
#define MWBUTTON_SCROLLUP 0x20      /* wheel up*/
#define MWBUTTON_SCROLLDN 0x40      /* wheel down*/

#define MIN(a,b)      ((a) < (b) ? (a) : (b))
#define MAX(a,b)      ((a) > (b) ? (a) : (b))
#define CLAMP(x,a,b)  ((x) > (b) ? (b) : ((x) < (a) ? (a) : (x)))

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

void Begin_Composite(void)
{
}

void End_Composite(void)
{
}

void Flush(const Rect *r)
{
    // backbuffer not yet implemented
}

/* adjust passed rect to current clip rectangle */
static Rect *ClipRect(const Rect *rect)
{
    int x2, y2;
    static Rect r;

    r.x = CLAMP(rect->x, clipminx, clipmaxx);
    x2 = rect->x + rect->w;
    x2 = CLAMP(x2, clipminx, clipmaxx);
    r.w = x2 - r.x + 1;
    r.y = CLAMP(rect->y, clipminy, clipmaxy);
    y2 = rect->y + rect->h - 1;
    y2 = CLAMP(y2, clipminy, clipmaxy);
    r.h = y2 - r.y + 1;
    if (r.w < 1 || r.h < 1)
        return 0;
    return &r;
}

/* set global clipping rectangle */
void SetClip(const Rect *rect)
{
    if (rect) {
        int maxx = fb.width - 1;
        int maxy = fb.height - 1;
        clipminx = CLAMP(rect->x, 0, maxx);
        clipmaxx = rect->x + rect->w;
        clipmaxx = CLAMP(clipmaxx, 0, maxx);
        clipminy = CLAMP(rect->y, 0, maxy);
        clipmaxy = rect->y + rect->h;
        clipmaxy = CLAMP(clipmaxy, 0, maxy);
    } else {
        clipminx = 0;
        clipmaxx = fb.width - 1;
        clipminy = 0;
        clipmaxy = fb.height - 1;
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
static uint8_t defColorModR = 255;
static uint8_t defColorModG = 255;
static uint8_t defColorModB = 255;

/* set color for Drawables */
void SetColor(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
    defColor = COLORVAL_TO_PIXELVAL(ARGB(a, r, g, b));
}

void SetTextureColorMod(Texture *texture, uint8_t r, uint8_t g, uint8_t b)
{
    defColorModR = r;
    defColorModR = g;
    defColorModR = b;
}

/* Draw horizontal line from x1,y to x2,y including final point */
static void DrawHLine32(Drawable *d, int x1, int x2, int y, PIXELVAL c)
{
    uint8_t *addr = d->pixels + y * d->pitch + x1 * (d->bpp >> 3);
    int width = x2 - x1 + 1;
    int w = width;
    unassert(x1 >= 0 && x1 < d->width);
    unassert(x2 >= 0 && x2 < d->width);
    unassert(x2 >= x1);
    unassert(y >= 0 && y < d->height);

    while (--w >= 0) {
        *((uint32_t *)addr) = c;    //FIXME needs alpha blending
        addr += 4;
    }
    //UpdateRect(x1, y, width, 1);
}

/* Draw a vertical line from x,y1 to x,y2 including final point */
static void DrawVLine32(Drawable *d, int x, int y1, int y2, PIXELVAL c)
{
    int pitch = d->pitch;
    uint8_t *addr = d->pixels + y1 * pitch + x * (d->bpp >> 3);
    int height = y2 - y1 + 1;
    int h = height;
    unassert(x >= 0 && x < d->width);
    unassert(y1 >= 0 && y1 < d->height);
    unassert(y2 >= 0 && y2 < d->height);
    unassert(y2 >= y1);

    while (--h >= 0) {
        *((uint32_t *)addr) = c;    //FIXME needs alpha blending
        addr += pitch;
    }
    //UpdateRect(x, y1, 1, height);
}

void DrawRect(const Rect *rect)
{
    Rect *r = ClipRect(rect);
    if (!r) return;
    int x = r->x;
    int y = r->y;
    int width = r->w;
    int height = r->h;
    int maxx = x + width - 1;
    int maxy = y + height - 1;

    if (width <= 0 || height <= 0)
        return;

    DrawHLine32(&fb, x, maxx, y, defColor);
    if (height > 1)
        DrawHLine32(&fb, x, maxx, maxy, defColor);
    if (height < 3)
        return;
    ++y;
    --maxy;
    DrawVLine32(&fb, x, y, maxy, defColor);
    if (width > 1)
        DrawVLine32(&fb, maxx, y, maxy, defColor);
}


void FillRect(const Rect *rect)
{
    Rect *r = ClipRect(rect);
    if (!r) return;
    int x2 = r->x + r->w - 1;
    int y1 = r->y;
    int y2 = y1 + r->h - 1;
    
    while (y1 <= y2)
        DrawHLine32(&fb, r->x, x2, y1++, defColor);
    //int X1 = r->x;
    //int Y1 = r->y;
    //UpdateRect(X1, Y1, x2-X1+1, y2-Y1+1);
}

void Resize(uint64_t w, uint64_t h)
{
    /* FB display cannot be resized */
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
    memcpy(t->pixels, data, t->size);
    return t;
}

void DestroyTexture(Texture *texture)
{
    unassert(texture);
    free(texture->pixels);
    free(texture);
}

/* hardcoded MWPF_TRUECOLORARGB format */
#define B   0
#define G   1
#define R   2
#define A   3

//#define muldiv255(a,b)    (((a)*(b))/255)         /* slow divide, exact*/
#define muldiv255(a,b)      ((((a)+1)*(b))>>8)      /* very fast, 92% accurate*/

/* actually copy data, no clipping done */
static void blit(Drawable *src, const Rect *srect, Drawable *dst, const Rect *drect)
{
    //unassert(srect->w == drect->w);   //FIXME check why needs commenting out
    /* src and dst height can differ, will use dst height for drawing */
    uint8_t *dstaddr = dst->pixels + drect->y * dst->pitch + drect->x * (dst->bpp >> 3);
    uint8_t *srcaddr = src->pixels + srect->y * src->pitch + srect->x * (src->bpp >> 3);
    int y = drect->h;
    //ZZZ
    while (y-- > 0) {
        //memcpy(dstaddr, srcaddr, drect->w * (dst->bpp >> 3));
        int x = drect->w;
        uint8_t *s = srcaddr;
        uint8_t *d = dstaddr;
        while (x-- > 0) {
            uint8_t alpha;
#define FORCE   1
            if (FORCE || (alpha = s[A]) == 255) {
                d[B] = s[B] * defColorModB / 255;
                d[G] = s[G] * defColorModG / 255;
                d[R] = s[R] * defColorModR / 255;
                d[A] = s[A];
            } else if (alpha != 0) {
#if 1
                /* d += muldiv255(a, s - d) */
                d[B] += muldiv255(alpha, (s[B] * defColorModB / 255) - d[B]);
                d[G] += muldiv255(alpha, (s[G] * defColorModG / 255) - d[G]);
                d[R] += muldiv255(alpha, (s[R] * defColorModR / 255) - d[R]);

                /* d += muldiv255(a, 255 - d)*/
                d[A] += muldiv255(alpha, 255 - d[A]);
#else
                d[B] = s[B] * defColorModB / 255;
                d[G] = s[G] * defColorModG / 255;
                d[R] = s[R] * defColorModR / 255;
                d[A] = s[A];
#endif
            }
            d += 4;
            s += 4;
        }
        dstaddr += dst->pitch;
        srcaddr += src->pitch;
    }
    //UpdateRect(drect->x, drect->y, drect->w, drect->h);
}

/* draw pixels from passed texture handle */
void BlitTexture(Texture *texture, const Rect *srect, const Rect *drect)
{
    unassert(texture);
    unassert(srect->w == drect->w);
    unassert(srect->h == drect->h);
    Rect *cr = ClipRect(drect);
#if 0
    if (srect->h != cr->h) {
        return;
        DeInit();
        printf("src %d,%d %d,%d dst %d,%d %d,%d\n",
            drect->x, drect->y, drect->w, drect->h,
            cr->x, cr->y, cr->w, cr->h);
        exit(1);
    }
#endif
    if (cr) {
        blit(texture, srect, &fb, cr);
    }
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
        blit(d, cr, &fb, cr);
    }
#else   /* used to test code for textures */
    Texture *t = CreateTexture(d->pixels, d->width, d->height, d->pitch, 0);
    unassert(t);
    BlitTexture(t, cr, cr);
    DestroyTexture(t);
#endif
}

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
                if (buf[0] == 033) exit(1);
                if (buf[0] == 0x7F) buf[0] = '\b';
                event->type = SDL_KEYDOWN;
                event->key.state = SDL_PRESSED;
                event->key.keysym.sym = buf[0];
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
    atexit(DeInit);
    signal(SIGHUP, sighup);
    signal(SIGABRT, sighup);
    signal(SIGSEGV, sighup);
    return 0;
}

void (*host_gui_funcs[]) = {
    (void*)Init,
    (void*)DeInit,
    (void*)DrawRect,
    (void*)FillRect,
    (void*)BlitTexture,
    (void*)SetClip,
    (void*)SetColor,
    (void*)SetTextureColorMod,
    (void*)DestroyTexture,
    (void*)CreateTexture,
    (void*)Begin_Composite,
    (void*)End_Composite,
    (void*)Flush,
    (void*)Resize,
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

    /* switch console to graphic mode, no more printf error messages */
    if (keybd_fd >= 0) {
        //ioctl(keybd_fd, KDSETMODE, KD_GRAPHICS);    //FIXME comment out for debug text screen during exec
    }

    memset(fb.pixels, 0, fb.size);
    SetClip(0);
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

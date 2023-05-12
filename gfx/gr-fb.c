/*
 * Host GUI for ChrysaLisp Framebuffer
 *
 * May 2023 Greg Haerr
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <inttypes.h>
#include <unistd.h>
#include <string.h>
#include <poll.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/fb.h>
#include <linux/kd.h>
#include <linux/vt.h>

#include "gr.h"
#include "sdl-dummy.h"

#define PATH_FRAMEBUFFER    "/dev/fb0"
#define PATH_MOUSE          "/dev/input/mice"

static struct drawable fb;      /* hardware framebuffer */
static int mouse_fd = -1;
static int frame_fd = -1;       /* framebuffer file handle */

static int OpenFramebuffer(void);
static int OpenMouse(void);
static int ReadMouse(int *dx, int *dy, int *bp);

/* Mouse button bits */
#define MWBUTTON_L		  0x01		/* left button*/
#define MWBUTTON_R		  0x02		/* right button*/
#define MWBUTTON_M		  0x10		/* middle*/
#define MWBUTTON_SCROLLUP 0x20		/* wheel up*/
#define MWBUTTON_SCROLLDN 0x40		/* wheel down*/

uint64_t Flush(const struct rect *r)
{
    // backbuffer not yet implemented
    return 0;
}

/******************** Routines only required for CL *******************/

uint64_t Upload_Texture(uint32_t *data, uint64_t w, uint64_t h, uint64_t s, uint64_t m)
{
    struct drawable *t;

    t = malloc(sizeof(struct drawable));
    assert(t);
    t->pixtype = fb.pixtype;
    t->bpp = fb.bpp;
    t->width = w;
    t->height = h;
    t->pitch = s;
    t->size = h * t->pitch;
    t->pixels = malloc(t->size);
    assert(t->pixels);
    return (uint64_t)t;
}

void DestroyTexture(uint64_t texture)
{
    struct drawable *t = (void *)texture;
    assert(t);
    free(t->pixels);
    free(t);
}

uint64_t Begin_Composite()
{
    return 0;
}

uint64_t End_Composite()
{
    return 0;
}

/* set color for DrawRect and FillRect */
void SetColor(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
{
    //coming shortly
}

void DrawRect(const struct rect *rect)
{
    //coming shortly
}

void FillRect(const struct rect *rect)
{
    //coming shortly
}

void SetClip(const struct rect *rect)
{
    //SDL_RenderSetClipRect(renderer, rect);
}

void Resize(uint64_t w, uint64_t h)
{
}

void SetTextureColorMod(uint64_t texture, uint8_t r, uint8_t g, uint8_t b)
{
}

static void blit(struct drawable *dst, struct drawable *src, 
    const struct rect *srect, const struct rect *drect)
{
    assert(srect->w == drect->w);
    assert(srect->h == drect->h);
    char *dstaddr = dst->pixels + drect->y * dst->pitch + drect->x * (dst->bpp >> 3);
    char *srcaddr = src->pixels + srect->y * src->pitch + srect->x * (src->bpp >> 3);
    int i;
    for (i=0; i < srect->h; i++) {
        memcpy(dstaddr, srcaddr, srect->w * dst->bpp >> 3);
        dstaddr += dst->pitch;
        srcaddr += src->pitch;
    }
}

/* copy pixel data */
void DrawBits(struct drawable *d, int x, int y, int width, int height)
{
    struct rect r;
    if (!width) width = d->width;
    if (!height) height = d->height;
    r.x = x;
    r.y = y;
    r.w = width;
    r.h = height;

    //unsigned char *pixels = d->pixels + y * d->pitch + x * (d->bpp >> 3);
    //SDL_UpdateTexture(backbuffer, &r, pixels, d->pitch);
    blit(&fb, d, &r, &r);
}

void Blit(uint64_t texture, const struct rect *srect, const struct rect *drect)
{
    struct drawable *d = (struct drawable *)texture;
    assert(d);
    assert(srect->w == drect->w);
    assert(srect->h == drect->h);
    blit(&fb, d, drect, srect);
    //SDL_RenderCopy(renderer, texture, srect, drect);
}

/* msec timeout 0 to poll, timeout -1 to block */
static uint64_t GetEventTimeout(void *data, int timeout)
{
    struct pollfd fds[2];
    SDL_Event *event = (SDL_Event *)data;

    fds[0].fd = 0;
    fds[0].events = POLLIN;
    fds[1].fd = mouse_fd;
    fds[1].events = POLLIN;
    if (poll(fds, 2, -1) >= 0) {
        memset(event, 0, sizeof(SDL_Event));
        if (fds[0].revents & POLLIN) {
            unsigned char buf[1];
            if (read(0, buf, 1) == 1) {
                //printf("Got %c\n", buf[0]);
                event->type = SDL_KEYDOWN;
                event->key.keysym.sym = buf[0];
                return 1;
            }
        }
        if (fds[1].revents & POLLIN) {
            int x, y, b;
            static int lastx = -1, lasty = -1, lastb;
            static int posx, posy;
            if (ReadMouse(&x, &y, &b)) {
                if (x != lastx || y != lasty) {
                    event->type = SDL_MOUSEMOTION;
                    posx += x;
                    posy += y;
                    if (posx < 0) posx = 0;
                    if (posy < 0) posy = 0;
                    if (posx >= fb.width) posx = fb.width - 1;
                    if (posy >= fb.height) posy = fb.height - 1;
                    event->motion.x = posx;
                    event->motion.y = posx;
                    event->motion.xrel = x;
                    event->motion.yrel = y;
                    //printf("Mouse motion %d,%d\n", posx, posy);
                    lastx = x;
                    lasty = y;
                    return 1;
                }
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
                    if (event->button.button) {
                        event->button.state = (event->type == SDL_MOUSEBUTTONDOWN)? SDL_PRESSED: SDL_RELEASED;
                        //printf("Mouse %d button %d\n", event->button.button, event->type);
						lastb = b;
                        return 1;
                    }
                }
            }
        }
    }

    event->type = 0;
    return 1;
}

uint64_t PollEvent(SDL_Event *event)
{
    GetEventTimeout(event, 0);
    return event->type != 0;
}

uint64_t WaitEvent(void *event)
{
    GetEventTimeout(event, -1);
    return 1;
}

void (*host_gui_funcs[]) = {
    (void*)Init,
    (void*)DeInit,
    (void*)DrawRect,
    (void*)FillRect,
    (void*)Blit,
    (void*)SetClip,
    (void*)SetColor,
    (void*)SetTextureColorMod,
    (void*)DestroyTexture,
    (void*)Upload_Texture,
    (void*)Begin_Composite,
    (void*)End_Composite,
    (void*)Flush,
    (void*)Resize,
    (void*)PollEvent,
};

uint64_t Init(struct rect *r)
{
    if (OpenFramebuffer() < 0)
        return -1;
    if (OpenMouse() < 0)
        return -1;
    return 0;
}

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
            printf("Error reading framebuffer info: %m\n");
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
                fb.pixtype = MWPF_TRUECOLOR555; // FIXME must also set MWPF_PIXELFORMAT in config
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
            printf("Unsupported %d bpp truecolor framebuffer\n", fb.bpp);
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
        printf("Error mmaping %s: %m\n", PATH_FRAMEBUFFER);
        goto fail;
    }

#if HAVE_TEXT
    int tty = 0;
    //tty = open ("/dev/tty0", O_RDWR);
    if(tty < 0) {
        printf("Error can't open /dev/tty0: %m\n");
        goto fail;
    }
    if(ioctl (tty, KDSETMODE, KD_GRAPHICS) == -1) {
        printf("Error setting graphics mode: %m\n");
        //close(tty);
        goto fail;
    }
    //close(tty);
#endif

    memset(fb.pixels, 0, fb.size);
    return frame_fd; /* success*/

fail:
    close(frame_fd);
    frame_fd = -1;
    return -1;
}

static void CloseFramebuffer(void)
{
    if (frame_fd >= 0) {
        munmap(fb.pixels, fb.size);
#if HAVE_TEXT
        int tty = 0;
        //int tty = open ("/dev/tty0", O_RDWR);
        ioctl(tty, KDSETMODE, KD_TEXT);
        //close(tty);
#endif
        close(frame_fd);
     }
    frame_fd = -1;
}

static void CloseMouse(void);

void DeInit(void)
{
    CloseMouse();
    CloseFramebuffer();
}

static int OpenMouse(void)
{
    /* sequence to mouse device to send ImPS/2 events*/
    static const unsigned char imps2[] = { 0xf3, 200, 0xf3, 100, 0xf3, 80 };
    unsigned char buf[4];

    mouse_fd = open(PATH_MOUSE, O_RDWR | O_NONBLOCK);
    if (mouse_fd < 0) {
        printf("Can't open mouse %s\n", PATH_MOUSE);
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
static int
ReadMouse(int *dx, int *dy, int *bp)
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

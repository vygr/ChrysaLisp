/*
 * Text console emulator drawing bitmap glyphs using SDL and framebuffer.
 *
 * See con_init() function for customization of emulation.
 *
 * Currently just displays bitmap glyphs echoed from keypresses.
 * Newline will result in scrolling screen.
 *
 * Sep 2022 Greg Haerr <greg@censoft.com>
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sdl-dummy.h"
#include "gr.h"

typedef unsigned short MWIMAGEBITS; /* bitmap image unit size*/

struct console {
    /* configurable parameters */
    int cols;                   /* # text columns */
    int lines;                  /* # text rows */
    int char_width;             /* glyph width in pixels */
    int char_height;            /* glyph height in pixels */
    MWIMAGEBITS *fontbits;      /* glyph bits (currently for 256 glyphs) */

    int curx;                   /* cursor x position */
    int cury;                   /* cursor y position */
    unsigned short *text_ram;   /* emulated adaptor RAM (= cols * lines * 2) */
    struct drawable scr;        /* emulated screen pixel buffer */
};
#define page_offset()  0    /* text mode page offset */

/* default display attribute (for testing)*/
//#define ATTR_DEFAULT 0x01 /* blue */
#define ATTR_DEFAULT 0x07   /* ltgray */

#define RGBDEF(r,g,b)   { r,g,b }
struct rgb { unsigned char r, g, b; };

/* 16 color palette for attribute mapping */
static struct rgb ega_colormap[16] = {
    RGBDEF( 0  , 0  , 0   ),    /* 0 black*/
    RGBDEF( 0  , 0  , 192 ),    /* 1 blue*/
    RGBDEF( 0  , 192, 0   ),    /* 2 green*/
    RGBDEF( 0  , 192, 192 ),    /* 3 cyan*/
    RGBDEF( 192, 0  , 0   ),    /* 4 red*/
    RGBDEF( 192, 0  , 192 ),    /* 5 magenta*/
    RGBDEF( 192, 128 , 0  ),    /* 6 adjusted brown*/
    RGBDEF( 192, 192, 192 ),    /* 7 ltgray*/
    RGBDEF( 128, 128, 128 ),    /* gray*/
    RGBDEF( 0  , 0  , 255 ),    /* ltblue*/
    RGBDEF( 0  , 255, 0   ),    /* 10 ltgreen*/
    RGBDEF( 0  , 255, 255 ),    /* ltcyan*/
    RGBDEF( 255, 0  , 0   ),    /* ltred*/
    RGBDEF( 255, 0  , 255 ),    /* ltmagenta*/
    RGBDEF( 255, 255, 0   ),    /* yellow*/
    RGBDEF( 255, 255, 255 ),    /* 15 white*/
};

/* create 16 bit 5/6/5 format pixel from RGB triplet */
#define RGB2PIXEL565(r,g,b) \
    ((((r) & 0xf8) << 8) | (((g) & 0xfc) << 3) | (((b) & 0xf8) >> 3))

/* create 16 bit 5/5/5 format pixel from RGB triplet */
#define RGB2PIXEL555(r,g,b) \
    ((((r) & 0xf8) << 7) | (((g) & 0xf8) << 2) | (((b) & 0xf8) >> 3))

/* MWIMAGEBITS macros*/
#define MWIMAGE_WORDS(x)    (((x)+15)/16)
#define MWIMAGE_BYTES(x)    (MWIMAGE_WORDS(x)*sizeof(MWIMAGEBITS))
/* size of image in words*/
#define MWIMAGE_SIZE(width, height)     \
    ((height) * (((width) + MWIMAGE_BITSPERIMAGE - 1) / MWIMAGE_BITSPERIMAGE))
#define MWIMAGE_BITSPERIMAGE    (sizeof(MWIMAGEBITS) * 8)
#define MWIMAGE_BITVALUE(n) ((MWIMAGEBITS) (((MWIMAGEBITS) 1) << (n)))
#define MWIMAGE_FIRSTBIT    (MWIMAGE_BITVALUE(MWIMAGE_BITSPERIMAGE - 1))
#define MWIMAGE_NEXTBIT(m)  ((MWIMAGEBITS) ((m) >> 1))
#define MWIMAGE_TESTBIT(m)  ((m) & MWIMAGE_FIRSTBIT)  /* use with shiftbit*/
#define MWIMAGE_SHIFTBIT(m) ((MWIMAGEBITS) ((m) << 1))  /* for testbit*/

#define MIN(a,b)      ((a) < (b) ? (a) : (b))
#define MAX(a,b)      ((a) > (b) ? (a) : (b))

static int vid_minx = 32767, vid_miny = 32767;
static int vid_maxx = -1, vid_maxy = -1;

static void update_dirty_region(int x, int y, int w, int h)
{
    vid_minx = MIN(x, vid_minx);
    vid_miny = MIN(y, vid_miny);
    vid_maxx = MAX(vid_maxx, x+w-1);
    vid_maxy = MAX(vid_maxy, y+h-1);
}

static void reset_dirty_region()
{
    vid_minx = vid_miny = 32767;
    vid_maxx = vid_maxy = -1;
}

/* draw a character bitmap */
static void drawbitmap(struct console *con, int c, unsigned char attr,
    int x, int y, int drawbg)
{
    MWIMAGEBITS *imagebits = con->fontbits + con->char_height * c;
    int minx = x;
    int maxx = x + con->char_width - 1;
    int bitcount = 0;
    unsigned short bitvalue = 0;
    int height = con->char_height;
    unsigned short usval;

    /* convert EGA attribute to RGB */
    int fg = attr & 0x0F;
    int bg = (attr & 0x70) >> 4;
    unsigned char fg_red = ega_colormap[fg].r;
    unsigned char fg_green = ega_colormap[fg].g;
    unsigned char fg_blue = ega_colormap[fg].b;
    unsigned char bg_red = ega_colormap[bg].r;
    unsigned char bg_green = ega_colormap[bg].g;
    unsigned char bg_blue = ega_colormap[bg].b;

    while (height > 0) {
        unsigned char *pixels;
        if (bitcount <= 0) {
            bitcount = 16;
            bitvalue = *imagebits++;
            pixels = con->scr.pixels + y * con->scr.pitch + x * (con->scr.bpp >> 3);
        }
        switch (con->scr.pixtype) {
        case MWPF_TRUECOLORARGB:    /* byte order B G R A */
            if (MWIMAGE_TESTBIT(bitvalue)) {
                *pixels++ = fg_blue;
                *pixels++ = fg_green;
                *pixels++ = fg_red;
                *pixels++ = 0xff;
            } else if (drawbg) {
                *pixels++ = bg_blue;
                *pixels++ = bg_green;
                *pixels++ = bg_red;
                *pixels++ = 0xff;
            }
            break;
        case MWPF_TRUECOLORABGR:    /* byte order R G B A */
            if (MWIMAGE_TESTBIT(bitvalue)) {
                *pixels++ = fg_red;
                *pixels++ = fg_green;
                *pixels++ = fg_blue;
                *pixels++ = 0xff;
            } else if (drawbg) {
                *pixels++ = bg_red;
                *pixels++ = bg_green;
                *pixels++ = bg_blue;
                *pixels++ = 0xff;
            }
            break;
        case MWPF_TRUECOLORBGR:     /* byte order R G B */
            if (MWIMAGE_TESTBIT(bitvalue)) {
                *pixels++ = fg_red;
                *pixels++ = fg_green;
                *pixels++ = fg_blue;
            } else if (drawbg) {
                *pixels++ = bg_red;
                *pixels++ = bg_green;
                *pixels++ = bg_blue;
            }
            break;
        case MWPF_TRUECOLOR565:
            if (MWIMAGE_TESTBIT(bitvalue)) {
                usval = RGB2PIXEL565(fg_red, fg_green, fg_blue);
                *pixels++ = usval & 255;
                *pixels++ = usval >> 8;
            } else if (drawbg) {
                usval = RGB2PIXEL565(bg_red, bg_green, bg_blue);
                *pixels++ = usval & 255;
                *pixels++ = usval >> 8;
            }
            break;
        case MWPF_TRUECOLOR555:
            if (MWIMAGE_TESTBIT(bitvalue)) {
                usval = RGB2PIXEL555(fg_red, fg_green, fg_blue);
                *pixels++ = usval & 255;
                *pixels++ = usval >> 8;
            } else if (drawbg) {
                usval = RGB2PIXEL555(bg_red, bg_green, bg_blue);
                *pixels++ = usval & 255;
                *pixels++ = usval >> 8;
            }
            break;
        }
        bitvalue = MWIMAGE_SHIFTBIT(bitvalue);
        bitcount--;
        if (x++ == maxx) {
            x = minx;
            ++y;
            --height;
            bitcount = 0;
        }
    }
}

static void cursoron(void)
{
}

static void cursoroff(void)
{
}

/* clear line y from x1 up to and including x2 to attribute attr */
static void clear_line(struct console *con, int x1, int x2, int y, unsigned char attr)
{
    int x;

    for (x = x1; x <= x2; x++) {
        con->text_ram[page_offset() + y * con->cols + x] = ' ' | (attr << 8);
        update_dirty_region(x, y, 1, 1);
    }
}

/* scroll adapter RAM up from line y1 up to and including line y2 */
static void scrollup(struct console *con, int y1, int y2, unsigned char attr)
{
    unsigned char *vid = (unsigned char *)(con->text_ram + page_offset() +
        y1 * con->cols);
    int pitch = con->cols * 2;

    memcpy(vid, vid + pitch, (con->lines - y1) * pitch);
    clear_line(con, 0, con->cols-1, y2, attr);
    update_dirty_region(0, 0, con->cols, con->lines);
}


/* scroll adapter RAM down from line y1 up to and including line y2 */
static void scrolldn(struct console *con, int y1, int y2, unsigned char attr)
{
    unsigned char *vid = (unsigned char *)(con->text_ram + page_offset() +
        (con->lines-1) * con->cols);
    int pitch = con->cols * 2;
    int y = y2;

    while (--y >= y1) {
        memcpy(vid, vid - pitch, pitch);
        vid -= pitch;
    }
    clear_line(con, 0, con->cols-1, y1, attr);
    update_dirty_region(0, 0, con->cols, con->lines);
}

/* output character at cursor location*/
void con_textout(struct console *con, int c, int attr)
{
    cursoroff();

    switch (c) {
    case '\0':  return;
    case '\b':  if (--con->curx <= 0) con->curx = 0; goto update;
    case '\r':  con->curx = 0; goto update;
    case '\n':  goto scroll;
    }

    con->text_ram[page_offset() + con->cury * con->cols + con->curx] =
        (c & 255) | ((attr & 255) << 8);
    update_dirty_region (con->curx, con->cury, 1, 1);

    if (++con->curx >= con->cols) {
        con->curx = 0;
scroll:
        if (++con->cury >= con->lines) {
            scrollup(con, 0, con->lines - 1, ATTR_DEFAULT);
            con->cury = con->lines - 1;
        }
    }

update:
    cursoron();
}

void con_setcursor(struct console *con, int row, int col)
{
    if (con->curx != col || con->cury != row) {
        cursoroff();
        con->cury = row;
        con->curx = col;
        cursoron();
    }
}

/* draw characters from adapter RAM */
static void draw_video_ram(struct console *con, int sx, int sy, int ex, int ey)
{
    unsigned short *vidram = con->text_ram + page_offset();

    for (int y = sy; y < ey; y++) {
        int j = y * con->cols + sx;
        for (int x = sx; x < ex; x++) {
            unsigned short chattr = vidram[j];
            drawbitmap(con, chattr & 255, chattr >> 8,
                x * con->char_width, y * con->char_height, 1);
            j++;
        }
    }
}

/* Called periodically from the main loop */
void con_update(struct console *con)
{
    static int lastx = -1, lasty = -1, needscursor = 0;

    if (vid_maxx >= 0 || vid_maxy >= 0) {
        /* draw text bitmaps from adaptor RAM */
        draw_video_ram(con, vid_minx, vid_miny, vid_maxx+1, vid_maxy+1);

        /* send screen bits to backend */
        DrawBits(&con->scr, vid_minx * con->char_width, vid_miny * con->char_height,
            (vid_maxx-vid_minx+1) * con->char_width,
            (vid_maxy-vid_miny+1) * con->char_height);

        reset_dirty_region();
        needscursor = 1;
    } else {
        /* check for cursor draw */
        if (lastx != con->curx || lasty != con->cury || needscursor) {
            if (lastx >= 0) {
                /* remove last cursor */
                draw_video_ram(con, lastx, lasty, lastx+1, lasty+1);
                DrawBits(&con->scr, lastx * con->char_width, lasty * con->char_height,
                    con->char_width, con->char_height);
            }

            /* draw current cursor */
            drawbitmap(con, '_', ATTR_DEFAULT,
                con->curx * con->char_width, con->cury * con->char_height, 0);
            DrawBits(&con->scr, con->curx * con->char_width, con->cury * con->char_height,
                con->char_width, con->char_height);
            lastx = con->curx; lasty = con->cury;
            needscursor = 0;
        }
    }
}

/* wait for next event, return 1 on quit */
int waitevent(struct console *con)
{
    int c;
    SDL_Event event;

    if (WaitEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                return 1;

            case SDL_KEYDOWN:
                c = event.key.keysym.sym;
                if (c == 033)
                    return 1;
                if (c == '\n') c = '\r';
                if (c == '\r') {
                    con_textout(con, c, ATTR_DEFAULT);
                    c = '\n';
                }
                con_textout(con, c, ATTR_DEFAULT);
                break;
        }
    }

    return 0;
}


/*
 * Initialize emulated console.
 * The console size, backbuffer pixel format,
 * font size and font bits can be customized here.
 * All other calculations are made automatically.
 */
int con_init(struct console *con)
{
    extern MWIMAGEBITS rom8x16_bits[];

    /* console size */
    con->cols = 1920 / 8;
    con->lines = 1080 / 16;

    /*
     * pixel format
     *
     * For testing, uncomment one of the following lines.
     * The graphics backbuffer will be setup in the specified
     * pixel format, and the BPP, width, height, etc calculated
     * from the lines and columns specified above.
     */
    con->scr.pixtype = MWPF_TRUECOLORARGB;
    //con->scr.pixtype = MWPF_TRUECOLORABGR;
    //con->scr.pixtype = MWPF_TRUECOLORBGR;
    //con->scr.pixtype = MWPF_TRUECOLOR565;
    //con->scr.pixtype = MWPF_TRUECOLOR555;

    switch (con->scr.pixtype) {
    case MWPF_TRUECOLORARGB:
    case MWPF_TRUECOLORABGR:
        con->scr.bpp = 32;
        break;
    case MWPF_TRUECOLORBGR:
        con->scr.bpp = 24;
        break;
    case MWPF_TRUECOLOR565:
    case MWPF_TRUECOLOR555:
        con->scr.bpp = 16;
        break;
    default:
        printf("con_init: Invalid pixel format\n");
        return -1;
    }

    /* console font */
    con->char_width = 8;
    con->char_height = 16;
    con->fontbits = rom8x16_bits;

    con->text_ram = malloc(con->cols * con->lines * 2);
    memset(con->text_ram, 0, con->cols * con->lines * 2);
    con->curx = 0;
    con->cury = 0;

    /* screen pixel buffer */
    con->scr.width = con->cols * con->char_width;
    con->scr.height = con->lines * con->char_height;
    con->scr.pitch = con->scr.width * (con->scr.bpp >> 3);
    con->scr.size = con->scr.height * con->scr.pitch;
    con->scr.pixels = malloc(con->scr.size);
    memset(con->scr.pixels, 0, con->scr.size);

    if (!con->scr.pixels || !con->text_ram) {
        printf("con_init: Can't malloc text_ram/backbuffer\n");
        return -1;
    }
    struct rect r;
    r.x = r.y = 0;
    r.w = con->scr.width;
    r.h = con->scr.height;
    if (Init(&r)) return -1;

    return 0;
}

int main(int ac, char **av)
{
    struct console console;
    struct console *con = &console;

    if (con_init(con) < 0) exit(2);
    con_setcursor(con, con->lines-1, 0);
    update_dirty_region(con->curx, con->cury, 1, 1);

    for (;;) {
        con_update(con);
        Flush(0);
        if (waitevent(con))
            break;
    }

    free(con->scr.pixels);
    free(con->text_ram);
    DeInit();
}

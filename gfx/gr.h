/*
 * Host GUI Library for ChrysaLisp SDL/Framebuffer
 *
 * May 2023 Greg Haerr
 */

/* supported framebuffer pixel formats */
#define MWPF_TRUECOLORARGB 0    /* 32bpp, memory byte order B, G, R, A */
#define MWPF_TRUECOLORABGR 1    /* 32bpp, memory byte order R, G, B, A */
#define MWPF_TRUECOLORBGR  2    /* 24bpp, memory byte order R, G, B */
#define MWPF_TRUECOLOR565  3    /* 16bpp, le unsigned short 5/6/5 RGB */
#define MWPF_TRUECOLOR555  4    /* 16bpp, le unsigned short 5/5/5 RGB */

struct rect {
    int x, y;
    int w, h;
};

struct drawable {
    int pixtype;                /* pixel format */
    int bpp;                    /* bits per pixel */
    int width;                  /* width in pixels */
    int height;                 /* height in pixels */
    int pitch;                  /* stride in bytes, offset to next pixel row */
    int size;                   /* total size in bytes */
    unsigned char *pixels;      /* pixel data */
};

uint64_t Init(struct rect *r);
void DeInit(void);
uint64_t Flush(const struct rect *r);

void DrawBits(struct drawable *d, int x, int y, int width, int height); // not used by CL
uint64_t WaitEvent(void *data);

/* debug routines exit graphics mode for error message */
void unassert_handler(char *msg, char *file, int line);
#define unassert(a)   if (!(a)) unassert_handler(#a,__FILE__, __LINE__)

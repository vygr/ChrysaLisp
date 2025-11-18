/* Comprehensive GL stub header for MAME/bgfx build - NOT for runtime use */
/* This header allows bgfx to compile but the functions are not functional */
#ifndef __gl_h_
#define __gl_h_

#ifdef __cplusplus
extern "C" {
#endif

/* GL types */
typedef unsigned int GLenum;
typedef unsigned char GLboolean;
typedef unsigned int GLbitfield;
typedef signed char GLbyte;
typedef short GLshort;
typedef int GLint;
typedef int GLsizei;
typedef unsigned char GLubyte;
typedef unsigned short GLushort;
typedef unsigned int GLuint;
typedef float GLfloat;
typedef float GLclampf;
typedef double GLdouble;
typedef double GLclampd;
typedef void GLvoid;
typedef long GLintptr;
typedef long GLsizeiptr;
typedef char GLchar;

/* Boolean values */
#define GL_FALSE 0
#define GL_TRUE 1

/* Zero and One */
#define GL_ZERO 0
#define GL_ONE 1

/* Buffer bits */
#define GL_COLOR_BUFFER_BIT   0x00004000
#define GL_DEPTH_BUFFER_BIT   0x00000100
#define GL_STENCIL_BUFFER_BIT 0x00000400

/* Primitives */
#define GL_POINTS         0x0000
#define GL_LINES          0x0001
#define GL_LINE_LOOP      0x0002
#define GL_LINE_STRIP     0x0003
#define GL_TRIANGLES      0x0004
#define GL_TRIANGLE_STRIP 0x0005
#define GL_TRIANGLE_FAN   0x0006

/* Texture targets */
#define GL_TEXTURE_1D                   0x0DE0
#define GL_TEXTURE_2D                   0x0DE1
#define GL_TEXTURE_3D                   0x806F
#define GL_TEXTURE_CUBE_MAP             0x8513
#define GL_TEXTURE_2D_ARRAY             0x8C1A

/* Common GL constants */
#define GL_NEVER            0x0200
#define GL_LESS             0x0201
#define GL_EQUAL            0x0202
#define GL_LEQUAL           0x0203
#define GL_GREATER          0x0204
#define GL_NOTEQUAL         0x0205
#define GL_GEQUAL           0x0206
#define GL_ALWAYS           0x0207

/* Data types */
#define GL_BYTE             0x1400
#define GL_UNSIGNED_BYTE    0x1401
#define GL_SHORT            0x1402
#define GL_UNSIGNED_SHORT   0x1403
#define GL_INT              0x1404
#define GL_UNSIGNED_INT     0x1405
#define GL_FLOAT            0x1406

/* Pixel formats */
#define GL_RGB              0x1907
#define GL_RGBA             0x1908
#define GL_BGR              0x80E0
#define GL_BGRA             0x80E1
#define GL_RED              0x1903
#define GL_GREEN            0x1904
#define GL_BLUE             0x1905
#define GL_ALPHA            0x1906
#define GL_DEPTH_COMPONENT  0x1902

/* Internal texture formats */
#define GL_RGB8             0x8051
#define GL_RGBA4            0x8056
#define GL_RGB5_A1          0x8057
#define GL_RGBA8            0x8058
#define GL_RGB10_A2         0x8059
#define GL_RGBA12           0x805A
#define GL_RGBA16           0x805B
#define GL_DEPTH_COMPONENT16  0x81A5
#define GL_DEPTH_COMPONENT24  0x81A6
#define GL_DEPTH_COMPONENT32  0x81A7
#define GL_DEPTH_COMPONENT32F 0x8CAC
#define GL_SRGB8_ALPHA8       0x8C43

/* Packed pixel types */
#define GL_UNSIGNED_SHORT_4_4_4_4_REV 0x8365
#define GL_UNSIGNED_SHORT_1_5_5_5_REV 0x8366
#define GL_UNSIGNED_INT_8_8_8_8_REV   0x8367

/* Common enums */
#define GL_VENDOR           0x1F00
#define GL_RENDERER         0x1F01
#define GL_VERSION          0x1F02
#define GL_EXTENSIONS       0x1F03
#define GL_NONE             0
#define GL_DONT_CARE        0x1100

/* Error codes */
#define GL_INVALID_ENUM     0x0500
#define GL_INVALID_VALUE    0x0501
#define GL_INVALID_OPERATION 0x0502
#define GL_OUT_OF_MEMORY    0x0505
#ifndef GL_INVALID_INDEX
#define GL_INVALID_INDEX    0xFFFFFFFFu
#endif

/* Capability bits */
#define GL_SCISSOR_TEST     0x0C11
#define GL_STENCIL_TEST     0x0B90

/* Pixel storage modes */
#define GL_UNPACK_ALIGNMENT 0x0CF5

/* Limits */
#define GL_MAX_TEXTURE_SIZE 0x0D33

/* Face culling */
#define GL_CW               0x0900
#define GL_CCW              0x0901
#define GL_FRONT            0x0404
#define GL_BACK             0x0405
#define GL_FRONT_AND_BACK   0x0408
#define GL_CULL_FACE        0x0B44

/* Depth testing */
#define GL_DEPTH_TEST       0x0B71

/* Blending */
#define GL_BLEND                   0x0BE2
#define GL_SRC_COLOR               0x0300
#define GL_ONE_MINUS_SRC_COLOR     0x0301
#define GL_SRC_ALPHA               0x0302
#define GL_ONE_MINUS_SRC_ALPHA     0x0303
#define GL_DST_ALPHA               0x0304
#define GL_ONE_MINUS_DST_ALPHA     0x0305
#define GL_DST_COLOR               0x0306
#define GL_ONE_MINUS_DST_COLOR     0x0307
#define GL_SRC_ALPHA_SATURATE      0x0308

/* Stencil operations */
#define GL_KEEP                    0x1E00
#define GL_REPLACE                 0x1E01
#define GL_INCR                    0x1E02
#define GL_DECR                    0x1E03
#define GL_INVERT                  0x150A

/* Texture wrapping */
#define GL_REPEAT                  0x2901
#define GL_CLAMP_TO_EDGE           0x812F
#define GL_MIRRORED_REPEAT         0x8370
#define GL_TEXTURE_WRAP_S          0x2802
#define GL_TEXTURE_WRAP_T          0x2803
#define GL_TEXTURE_WRAP_R          0x8072

/* Texture filtering */
#define GL_NEAREST                 0x2600
#define GL_LINEAR                  0x2601
#define GL_NEAREST_MIPMAP_NEAREST  0x2700
#define GL_LINEAR_MIPMAP_NEAREST   0x2701
#define GL_NEAREST_MIPMAP_LINEAR   0x2702
#define GL_LINEAR_MIPMAP_LINEAR    0x2703
#define GL_TEXTURE_MAG_FILTER      0x2800
#define GL_TEXTURE_MIN_FILTER      0x2801

/* Texture parameters */
#define GL_TEXTURE_MIN_LOD         0x813A
#define GL_TEXTURE_MAX_LOD         0x813B
#define GL_TEXTURE_COMPARE_MODE    0x884C

/* Polygon mode */
#define GL_POINT            0x1B00
#define GL_LINE             0x1B01
#define GL_FILL             0x1B02

/* Stub functions - minimal set that might be referenced */
static inline void glClear(GLbitfield mask) { (void)mask; }
static inline void glClearColor(GLfloat r, GLfloat g, GLfloat b, GLfloat a) {
    (void)r; (void)g; (void)b; (void)a;
}
static inline void glEnable(GLenum cap) { (void)cap; }
static inline void glDisable(GLenum cap) { (void)cap; }
static inline void glViewport(GLint x, GLint y, GLsizei width, GLsizei height) {
    (void)x; (void)y; (void)width; (void)height;
}
static inline const GLubyte* glGetString(GLenum name) {
    (void)name;
    return (const GLubyte*)"STUB";
}
static inline void glCullFace(GLenum mode) { (void)mode; }
static inline void glFrontFace(GLenum mode) { (void)mode; }
static inline void glPolygonMode(GLenum face, GLenum mode) { (void)face; (void)mode; }
static inline void glBlendFunc(GLenum sfactor, GLenum dfactor) { (void)sfactor; (void)dfactor; }
static inline void glDepthFunc(GLenum func) { (void)func; }
static inline void glDepthMask(GLboolean flag) { (void)flag; }
static inline void glEnablei(GLenum target, GLuint index) { (void)target; (void)index; }
static inline void glDisablei(GLenum target, GLuint index) { (void)target; (void)index; }
static inline void glPixelStorei(GLenum pname, GLint param) { (void)pname; (void)param; }
static inline void glTexParameteri(GLenum target, GLenum pname, GLint param) { (void)target; (void)pname; (void)param; }
static inline void glTexParameterf(GLenum target, GLenum pname, GLfloat param) { (void)target; (void)pname; (void)param; }
static inline void glDrawBuffer(GLenum mode) { (void)mode; }
static inline void glReadBuffer(GLenum mode) { (void)mode; }
static inline void glScissor(GLint x, GLint y, GLsizei width, GLsizei height) { (void)x; (void)y; (void)width; (void)height; }
static inline GLint glGetError(void) { return 0; }
static inline void glGetIntegerv(GLenum pname, GLint *params) { (void)pname; if(params) *params = 0; }

#ifdef __cplusplus
}
#endif

#endif /* __gl_h_ */

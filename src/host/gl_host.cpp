#if defined(_HOST_GL)
#if _HOST_GL == 1

#include <GL/gl.h>
#include <GL/glu.h>
#include <stdint.h>
#include <iostream>
#include <unordered_map>
#include <vector>

// Error logging utility
void logGLError(const char* msg) {
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        std::cerr << msg << " GL error: 0x" << std::hex << err << std::endl;
    }
}

// OpenGL initialization
int64_t host_gl_init()
{
    // Note: OpenGL context should already be created by SDL window
    // This function just sets up default OpenGL state

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glFrontFace(GL_CCW);

    logGLError("host_gl_init");
    return 0;
}

int64_t host_gl_deinit()
{
    // Clean up any GL resources
    return 0;
}

// Viewport management
void host_gl_viewport(int64_t x, int64_t y, int64_t width, int64_t height)
{
    glViewport(x, y, width, height);
    logGLError("host_gl_viewport");
}

// Clear functions
void host_gl_clear(uint64_t mask)
{
    glClear(mask);
    logGLError("host_gl_clear");
}

void host_gl_clear_color(float r, float g, float b, float a)
{
    glClearColor(r, g, b, a);
    logGLError("host_gl_clear_color");
}

void host_gl_clear_depth(double depth)
{
    glClearDepth(depth);
    logGLError("host_gl_clear_depth");
}

// Enable/Disable features
void host_gl_enable(uint64_t cap)
{
    glEnable(cap);
    logGLError("host_gl_enable");
}

void host_gl_disable(uint64_t cap)
{
    glDisable(cap);
    logGLError("host_gl_disable");
}

// Matrix operations
void host_gl_matrix_mode(uint64_t mode)
{
    glMatrixMode(mode);
    logGLError("host_gl_matrix_mode");
}

void host_gl_load_identity()
{
    glLoadIdentity();
    logGLError("host_gl_load_identity");
}

void host_gl_push_matrix()
{
    glPushMatrix();
    logGLError("host_gl_push_matrix");
}

void host_gl_pop_matrix()
{
    glPopMatrix();
    logGLError("host_gl_pop_matrix");
}

void host_gl_load_matrix_f(const float *m)
{
    glLoadMatrixf(m);
    logGLError("host_gl_load_matrix_f");
}

void host_gl_mult_matrix_f(const float *m)
{
    glMultMatrixf(m);
    logGLError("host_gl_mult_matrix_f");
}

// Transformation functions
void host_gl_translate_f(float x, float y, float z)
{
    glTranslatef(x, y, z);
    logGLError("host_gl_translate_f");
}

void host_gl_rotate_f(float angle, float x, float y, float z)
{
    glRotatef(angle, x, y, z);
    logGLError("host_gl_rotate_f");
}

void host_gl_scale_f(float x, float y, float z)
{
    glScalef(x, y, z);
    logGLError("host_gl_scale_f");
}

// Perspective and orthographic projections
void host_gl_ortho(double left, double right, double bottom, double top, double near, double far)
{
    glOrtho(left, right, bottom, top, near, far);
    logGLError("host_gl_ortho");
}

void host_gl_frustum(double left, double right, double bottom, double top, double near, double far)
{
    glFrustum(left, right, bottom, top, near, far);
    logGLError("host_gl_frustum");
}

void host_gl_perspective(double fovy, double aspect, double znear, double zfar)
{
    gluPerspective(fovy, aspect, znear, zfar);
    logGLError("host_gl_perspective");
}

// Drawing primitives
void host_gl_begin(uint64_t mode)
{
    glBegin(mode);
}

void host_gl_end()
{
    glEnd();
    logGLError("host_gl_end");
}

void host_gl_vertex2f(float x, float y)
{
    glVertex2f(x, y);
}

void host_gl_vertex3f(float x, float y, float z)
{
    glVertex3f(x, y, z);
}

void host_gl_vertex4f(float x, float y, float z, float w)
{
    glVertex4f(x, y, z, w);
}

void host_gl_color3f(float r, float g, float b)
{
    glColor3f(r, g, b);
}

void host_gl_color4f(float r, float g, float b, float a)
{
    glColor4f(r, g, b, a);
}

void host_gl_normal3f(float nx, float ny, float nz)
{
    glNormal3f(nx, ny, nz);
}

void host_gl_tex_coord2f(float s, float t)
{
    glTexCoord2f(s, t);
}

// Texture management
struct GLTexture {
    GLuint id;
    int width;
    int height;
};

static std::unordered_map<uint32_t, GLTexture> textureMap;
static uint32_t nextTextureHandle = 0x2000;

uint32_t host_gl_gen_texture()
{
    GLuint texId;
    glGenTextures(1, &texId);

    uint32_t handle = nextTextureHandle++;
    textureMap[handle] = {texId, 0, 0};

    logGLError("host_gl_gen_texture");
    return handle;
}

void host_gl_delete_texture(uint32_t handle)
{
    if (textureMap.find(handle) == textureMap.end()) {
        std::cerr << "Invalid texture handle" << std::endl;
        return;
    }

    GLuint texId = textureMap[handle].id;
    glDeleteTextures(1, &texId);
    textureMap.erase(handle);

    logGLError("host_gl_delete_texture");
}

void host_gl_bind_texture(uint64_t target, uint32_t handle)
{
    if (handle == 0) {
        glBindTexture(target, 0);
        return;
    }

    if (textureMap.find(handle) == textureMap.end()) {
        std::cerr << "Invalid texture handle" << std::endl;
        return;
    }

    glBindTexture(target, textureMap[handle].id);
    logGLError("host_gl_bind_texture");
}

void host_gl_tex_image_2d(uint64_t target, int64_t level, int64_t internalformat,
                          int64_t width, int64_t height, int64_t border,
                          uint64_t format, uint64_t type, const void *pixels)
{
    glTexImage2D(target, level, internalformat, width, height, border, format, type, pixels);
    logGLError("host_gl_tex_image_2d");
}

void host_gl_tex_parameter_i(uint64_t target, uint64_t pname, int64_t param)
{
    glTexParameteri(target, pname, param);
    logGLError("host_gl_tex_parameter_i");
}

// Buffer management
struct GLBuffer {
    GLuint id;
    size_t size;
};

static std::unordered_map<uint32_t, GLBuffer> bufferMap;
static uint32_t nextBufferHandle = 0x3000;

uint32_t host_gl_gen_buffer()
{
    GLuint bufId;
    glGenBuffers(1, &bufId);

    uint32_t handle = nextBufferHandle++;
    bufferMap[handle] = {bufId, 0};

    logGLError("host_gl_gen_buffer");
    return handle;
}

void host_gl_delete_buffer(uint32_t handle)
{
    if (bufferMap.find(handle) == bufferMap.end()) {
        std::cerr << "Invalid buffer handle" << std::endl;
        return;
    }

    GLuint bufId = bufferMap[handle].id;
    glDeleteBuffers(1, &bufId);
    bufferMap.erase(handle);

    logGLError("host_gl_delete_buffer");
}

void host_gl_bind_buffer(uint64_t target, uint32_t handle)
{
    if (handle == 0) {
        glBindBuffer(target, 0);
        return;
    }

    if (bufferMap.find(handle) == bufferMap.end()) {
        std::cerr << "Invalid buffer handle" << std::endl;
        return;
    }

    glBindBuffer(target, bufferMap[handle].id);
    logGLError("host_gl_bind_buffer");
}

void host_gl_buffer_data(uint64_t target, int64_t size, const void *data, uint64_t usage)
{
    glBufferData(target, size, data, usage);
    logGLError("host_gl_buffer_data");
}

// Vertex arrays
void host_gl_vertex_pointer(int64_t size, uint64_t type, int64_t stride, const void *pointer)
{
    glVertexPointer(size, type, stride, pointer);
    logGLError("host_gl_vertex_pointer");
}

void host_gl_color_pointer(int64_t size, uint64_t type, int64_t stride, const void *pointer)
{
    glColorPointer(size, type, stride, pointer);
    logGLError("host_gl_color_pointer");
}

void host_gl_normal_pointer(uint64_t type, int64_t stride, const void *pointer)
{
    glNormalPointer(type, stride, pointer);
    logGLError("host_gl_normal_pointer");
}

void host_gl_tex_coord_pointer(int64_t size, uint64_t type, int64_t stride, const void *pointer)
{
    glTexCoordPointer(size, type, stride, pointer);
    logGLError("host_gl_tex_coord_pointer");
}

void host_gl_enable_client_state(uint64_t array)
{
    glEnableClientState(array);
    logGLError("host_gl_enable_client_state");
}

void host_gl_disable_client_state(uint64_t array)
{
    glDisableClientState(array);
    logGLError("host_gl_disable_client_state");
}

// Drawing arrays
void host_gl_draw_arrays(uint64_t mode, int64_t first, int64_t count)
{
    glDrawArrays(mode, first, count);
    logGLError("host_gl_draw_arrays");
}

void host_gl_draw_elements(uint64_t mode, int64_t count, uint64_t type, const void *indices)
{
    glDrawElements(mode, count, type, indices);
    logGLError("host_gl_draw_elements");
}

// Lighting
void host_gl_light_fv(uint64_t light, uint64_t pname, const float *params)
{
    glLightfv(light, pname, params);
    logGLError("host_gl_light_fv");
}

void host_gl_material_fv(uint64_t face, uint64_t pname, const float *params)
{
    glMaterialfv(face, pname, params);
    logGLError("host_gl_material_fv");
}

void host_gl_shade_model(uint64_t mode)
{
    glShadeModel(mode);
    logGLError("host_gl_shade_model");
}

// Blending
void host_gl_blend_func(uint64_t sfactor, uint64_t dfactor)
{
    glBlendFunc(sfactor, dfactor);
    logGLError("host_gl_blend_func");
}

// Utility functions
void host_gl_flush()
{
    glFlush();
}

void host_gl_finish()
{
    glFinish();
}

int64_t host_gl_get_error()
{
    return glGetError();
}

// Function table exported to ChrysaLisp VP
void (*host_gl_funcs[]) = {
    // Initialization
    (void*)host_gl_init,
    (void*)host_gl_deinit,

    // Viewport and clear
    (void*)host_gl_viewport,
    (void*)host_gl_clear,
    (void*)host_gl_clear_color,
    (void*)host_gl_clear_depth,

    // State management
    (void*)host_gl_enable,
    (void*)host_gl_disable,

    // Matrix operations
    (void*)host_gl_matrix_mode,
    (void*)host_gl_load_identity,
    (void*)host_gl_push_matrix,
    (void*)host_gl_pop_matrix,
    (void*)host_gl_load_matrix_f,
    (void*)host_gl_mult_matrix_f,

    // Transformations
    (void*)host_gl_translate_f,
    (void*)host_gl_rotate_f,
    (void*)host_gl_scale_f,

    // Projections
    (void*)host_gl_ortho,
    (void*)host_gl_frustum,
    (void*)host_gl_perspective,

    // Drawing primitives
    (void*)host_gl_begin,
    (void*)host_gl_end,
    (void*)host_gl_vertex2f,
    (void*)host_gl_vertex3f,
    (void*)host_gl_vertex4f,
    (void*)host_gl_color3f,
    (void*)host_gl_color4f,
    (void*)host_gl_normal3f,
    (void*)host_gl_tex_coord2f,

    // Texture management
    (void*)host_gl_gen_texture,
    (void*)host_gl_delete_texture,
    (void*)host_gl_bind_texture,
    (void*)host_gl_tex_image_2d,
    (void*)host_gl_tex_parameter_i,

    // Buffer management
    (void*)host_gl_gen_buffer,
    (void*)host_gl_delete_buffer,
    (void*)host_gl_bind_buffer,
    (void*)host_gl_buffer_data,

    // Vertex arrays
    (void*)host_gl_vertex_pointer,
    (void*)host_gl_color_pointer,
    (void*)host_gl_normal_pointer,
    (void*)host_gl_tex_coord_pointer,
    (void*)host_gl_enable_client_state,
    (void*)host_gl_disable_client_state,

    // Drawing arrays
    (void*)host_gl_draw_arrays,
    (void*)host_gl_draw_elements,

    // Lighting
    (void*)host_gl_light_fv,
    (void*)host_gl_material_fv,
    (void*)host_gl_shade_model,

    // Blending
    (void*)host_gl_blend_func,

    // Utility
    (void*)host_gl_flush,
    (void*)host_gl_finish,
    (void*)host_gl_get_error,
};

#endif
#endif

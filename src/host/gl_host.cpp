#if defined(_HOST_GL)
#if _HOST_GL == 1

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>
#ifdef __linux__
    #include <GL/glx.h>
#elif defined(_WIN32)
    #include <windows.h>
    #include <GL/wglext.h>
#endif
#include <stdint.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>

// Forward declarations
void loadGLExtensions();

// Function pointers for OpenGL extensions (modern OpenGL)
static PFNGLCREATESHADERPROC glCreateShader_ptr = nullptr;
static PFNGLDELETESHADERPROC glDeleteShader_ptr = nullptr;
static PFNGLSHADERSOURCEPROC glShaderSource_ptr = nullptr;
static PFNGLCOMPILESHADERPROC glCompileShader_ptr = nullptr;
static PFNGLGETSHADERIVPROC glGetShaderiv_ptr = nullptr;
static PFNGLGETSHADERINFOLOGPROC glGetShaderInfoLog_ptr = nullptr;
static PFNGLCREATEPROGRAMPROC glCreateProgram_ptr = nullptr;
static PFNGLDELETEPROGRAMPROC glDeleteProgram_ptr = nullptr;
static PFNGLATTACHSHADERPROC glAttachShader_ptr = nullptr;
static PFNGLLINKPROGRAMPROC glLinkProgram_ptr = nullptr;
static PFNGLGETPROGRAMIVPROC glGetProgramiv_ptr = nullptr;
static PFNGLGETPROGRAMINFOLOGPROC glGetProgramInfoLog_ptr = nullptr;
static PFNGLUSEPROGRAMPROC glUseProgram_ptr = nullptr;
static PFNGLGETUNIFORMLOCATIONPROC glGetUniformLocation_ptr = nullptr;
static PFNGLUNIFORM1FPROC glUniform1f_ptr = nullptr;
static PFNGLUNIFORM2FPROC glUniform2f_ptr = nullptr;
static PFNGLUNIFORM3FPROC glUniform3f_ptr = nullptr;
static PFNGLUNIFORM4FPROC glUniform4f_ptr = nullptr;
static PFNGLUNIFORM1IPROC glUniform1i_ptr = nullptr;
static PFNGLUNIFORMMATRIX4FVPROC glUniformMatrix4fv_ptr = nullptr;
static PFNGLGENVERTEXARRAYSPROC glGenVertexArrays_ptr = nullptr;
static PFNGLDELETEVERTEXARRAYSPROC glDeleteVertexArrays_ptr = nullptr;
static PFNGLBINDVERTEXARRAYPROC glBindVertexArray_ptr = nullptr;
static PFNGLENABLEVERTEXATTRIBARRAYPROC glEnableVertexAttribArray_ptr = nullptr;
static PFNGLDISABLEVERTEXATTRIBARRAYPROC glDisableVertexAttribArray_ptr = nullptr;
static PFNGLVERTEXATTRIBPOINTERPROC glVertexAttribPointer_ptr = nullptr;
static PFNGLGENBUFFERSPROC glGenBuffers_ptr = nullptr;
static PFNGLDELETEBUFFERSPROC glDeleteBuffers_ptr = nullptr;
static PFNGLBINDBUFFERPROC glBindBuffer_ptr = nullptr;
static PFNGLBUFFERDATAPROC glBufferData_ptr = nullptr;

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

    // Load modern OpenGL extensions
    loadGLExtensions();

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
    if (!glGenBuffers_ptr) {
        std::cerr << "glGenBuffers not available" << std::endl;
        return 0;
    }

    GLuint bufId;
    glGenBuffers_ptr(1, &bufId);

    uint32_t handle = nextBufferHandle++;
    bufferMap[handle] = {bufId, 0};

    logGLError("host_gl_gen_buffer");
    return handle;
}

void host_gl_delete_buffer(uint32_t handle)
{
    if (!glDeleteBuffers_ptr) {
        return;
    }

    if (bufferMap.find(handle) == bufferMap.end()) {
        std::cerr << "Invalid buffer handle" << std::endl;
        return;
    }

    GLuint bufId = bufferMap[handle].id;
    glDeleteBuffers_ptr(1, &bufId);
    bufferMap.erase(handle);

    logGLError("host_gl_delete_buffer");
}

void host_gl_bind_buffer(uint64_t target, uint32_t handle)
{
    if (!glBindBuffer_ptr) {
        return;
    }

    if (handle == 0) {
        glBindBuffer_ptr(target, 0);
        return;
    }

    if (bufferMap.find(handle) == bufferMap.end()) {
        std::cerr << "Invalid buffer handle" << std::endl;
        return;
    }

    glBindBuffer_ptr(target, bufferMap[handle].id);
    logGLError("host_gl_bind_buffer");
}

void host_gl_buffer_data(uint64_t target, int64_t size, const void *data, uint64_t usage)
{
    if (!glBufferData_ptr) {
        return;
    }

    glBufferData_ptr(target, size, data, usage);
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

// Modern OpenGL - Shader support
struct GLShader {
    GLuint id;
    GLenum type;
    std::string source;
};

struct GLProgram {
    GLuint id;
    std::vector<GLuint> shaders;
};

static std::unordered_map<uint32_t, GLShader> shaderMap;
static std::unordered_map<uint32_t, GLProgram> programMap;
static uint32_t nextShaderHandle = 0x4000;
static uint32_t nextProgramHandle = 0x5000;

// Load OpenGL extensions
void* getGLProcAddress(const char* name) {
    // Platform-specific extension loading
    #if defined(__linux__)
        return (void*)glXGetProcAddress((const GLubyte*)name);
    #elif defined(_WIN32)
        return (void*)wglGetProcAddress(name);
    #elif defined(__APPLE__)
        // macOS doesn't need extension loading for core functions
        return nullptr;
    #endif
}

void loadGLExtensions() {
    #ifndef __APPLE__
    glCreateShader_ptr = (PFNGLCREATESHADERPROC)getGLProcAddress("glCreateShader");
    glDeleteShader_ptr = (PFNGLDELETESHADERPROC)getGLProcAddress("glDeleteShader");
    glShaderSource_ptr = (PFNGLSHADERSOURCEPROC)getGLProcAddress("glShaderSource");
    glCompileShader_ptr = (PFNGLCOMPILESHADERPROC)getGLProcAddress("glCompileShader");
    glGetShaderiv_ptr = (PFNGLGETSHADERIVPROC)getGLProcAddress("glGetShaderiv");
    glGetShaderInfoLog_ptr = (PFNGLGETSHADERINFOLOGPROC)getGLProcAddress("glGetShaderInfoLog");
    glCreateProgram_ptr = (PFNGLCREATEPROGRAMPROC)getGLProcAddress("glCreateProgram");
    glDeleteProgram_ptr = (PFNGLDELETEPROGRAMPROC)getGLProcAddress("glDeleteProgram");
    glAttachShader_ptr = (PFNGLATTACHSHADERPROC)getGLProcAddress("glAttachShader");
    glLinkProgram_ptr = (PFNGLLINKPROGRAMPROC)getGLProcAddress("glLinkProgram");
    glGetProgramiv_ptr = (PFNGLGETPROGRAMIVPROC)getGLProcAddress("glGetProgramiv");
    glGetProgramInfoLog_ptr = (PFNGLGETPROGRAMINFOLOGPROC)getGLProcAddress("glGetProgramInfoLog");
    glUseProgram_ptr = (PFNGLUSEPROGRAMPROC)getGLProcAddress("glUseProgram");
    glGetUniformLocation_ptr = (PFNGLGETUNIFORMLOCATIONPROC)getGLProcAddress("glGetUniformLocation");
    glUniform1f_ptr = (PFNGLUNIFORM1FPROC)getGLProcAddress("glUniform1f");
    glUniform2f_ptr = (PFNGLUNIFORM2FPROC)getGLProcAddress("glUniform2f");
    glUniform3f_ptr = (PFNGLUNIFORM3FPROC)getGLProcAddress("glUniform3f");
    glUniform4f_ptr = (PFNGLUNIFORM4FPROC)getGLProcAddress("glUniform4f");
    glUniform1i_ptr = (PFNGLUNIFORM1IPROC)getGLProcAddress("glUniform1i");
    glUniformMatrix4fv_ptr = (PFNGLUNIFORMMATRIX4FVPROC)getGLProcAddress("glUniformMatrix4fv");
    glGenVertexArrays_ptr = (PFNGLGENVERTEXARRAYSPROC)getGLProcAddress("glGenVertexArrays");
    glDeleteVertexArrays_ptr = (PFNGLDELETEVERTEXARRAYSPROC)getGLProcAddress("glDeleteVertexArrays");
    glBindVertexArray_ptr = (PFNGLBINDVERTEXARRAYPROC)getGLProcAddress("glBindVertexArray");
    glEnableVertexAttribArray_ptr = (PFNGLENABLEVERTEXATTRIBARRAYPROC)getGLProcAddress("glEnableVertexAttribArray");
    glDisableVertexAttribArray_ptr = (PFNGLDISABLEVERTEXATTRIBARRAYPROC)getGLProcAddress("glDisableVertexAttribArray");
    glVertexAttribPointer_ptr = (PFNGLVERTEXATTRIBPOINTERPROC)getGLProcAddress("glVertexAttribPointer");
    glGenBuffers_ptr = (PFNGLGENBUFFERSPROC)getGLProcAddress("glGenBuffers");
    glDeleteBuffers_ptr = (PFNGLDELETEBUFFERSPROC)getGLProcAddress("glDeleteBuffers");
    glBindBuffer_ptr = (PFNGLBINDBUFFERPROC)getGLProcAddress("glBindBuffer");
    glBufferData_ptr = (PFNGLBUFFERDATAPROC)getGLProcAddress("glBufferData");
    #endif
}

// Shader functions
uint32_t host_gl_create_shader(uint64_t type)
{
    if (!glCreateShader_ptr) {
        std::cerr << "glCreateShader not available" << std::endl;
        return 0;
    }

    GLuint shaderId = glCreateShader_ptr(type);
    if (shaderId == 0) {
        logGLError("host_gl_create_shader");
        return 0;
    }

    uint32_t handle = nextShaderHandle++;
    shaderMap[handle] = {shaderId, (GLenum)type, ""};

    return handle;
}

int64_t host_gl_shader_source(uint32_t handle, const char* source)
{
    if (!glShaderSource_ptr || shaderMap.find(handle) == shaderMap.end()) {
        return -1;
    }

    GLuint shaderId = shaderMap[handle].id;
    shaderMap[handle].source = source;

    glShaderSource_ptr(shaderId, 1, &source, nullptr);
    logGLError("host_gl_shader_source");
    return 0;
}

int64_t host_gl_compile_shader(uint32_t handle)
{
    if (!glCompileShader_ptr || !glGetShaderiv_ptr || shaderMap.find(handle) == shaderMap.end()) {
        return -1;
    }

    GLuint shaderId = shaderMap[handle].id;
    glCompileShader_ptr(shaderId);

    GLint success;
    glGetShaderiv_ptr(shaderId, GL_COMPILE_STATUS, &success);

    if (!success) {
        char infoLog[512];
        if (glGetShaderInfoLog_ptr) {
            glGetShaderInfoLog_ptr(shaderId, 512, nullptr, infoLog);
            std::cerr << "Shader compilation failed: " << infoLog << std::endl;
        }
        return -1;
    }

    return 0;
}

void host_gl_delete_shader(uint32_t handle)
{
    if (!glDeleteShader_ptr || shaderMap.find(handle) == shaderMap.end()) {
        return;
    }

    glDeleteShader_ptr(shaderMap[handle].id);
    shaderMap.erase(handle);
}

// Program functions
uint32_t host_gl_create_program()
{
    if (!glCreateProgram_ptr) {
        std::cerr << "glCreateProgram not available" << std::endl;
        return 0;
    }

    GLuint programId = glCreateProgram_ptr();
    if (programId == 0) {
        logGLError("host_gl_create_program");
        return 0;
    }

    uint32_t handle = nextProgramHandle++;
    programMap[handle] = {programId, {}};

    return handle;
}

int64_t host_gl_attach_shader(uint32_t program, uint32_t shader)
{
    if (!glAttachShader_ptr ||
        programMap.find(program) == programMap.end() ||
        shaderMap.find(shader) == shaderMap.end()) {
        return -1;
    }

    glAttachShader_ptr(programMap[program].id, shaderMap[shader].id);
    programMap[program].shaders.push_back(shaderMap[shader].id);
    logGLError("host_gl_attach_shader");
    return 0;
}

int64_t host_gl_link_program(uint32_t handle)
{
    if (!glLinkProgram_ptr || !glGetProgramiv_ptr || programMap.find(handle) == programMap.end()) {
        return -1;
    }

    GLuint programId = programMap[handle].id;
    glLinkProgram_ptr(programId);

    GLint success;
    glGetProgramiv_ptr(programId, GL_LINK_STATUS, &success);

    if (!success) {
        char infoLog[512];
        if (glGetProgramInfoLog_ptr) {
            glGetProgramInfoLog_ptr(programId, 512, nullptr, infoLog);
            std::cerr << "Program linking failed: " << infoLog << std::endl;
        }
        return -1;
    }

    return 0;
}

void host_gl_use_program(uint32_t handle)
{
    if (!glUseProgram_ptr) {
        return;
    }

    if (handle == 0) {
        glUseProgram_ptr(0);
        return;
    }

    if (programMap.find(handle) == programMap.end()) {
        return;
    }

    glUseProgram_ptr(programMap[handle].id);
    logGLError("host_gl_use_program");
}

void host_gl_delete_program(uint32_t handle)
{
    if (!glDeleteProgram_ptr || programMap.find(handle) == programMap.end()) {
        return;
    }

    glDeleteProgram_ptr(programMap[handle].id);
    programMap.erase(handle);
}

int64_t host_gl_get_uniform_location(uint32_t program, const char* name)
{
    if (!glGetUniformLocation_ptr || programMap.find(program) == programMap.end()) {
        return -1;
    }

    return glGetUniformLocation_ptr(programMap[program].id, name);
}

void host_gl_uniform1f(int64_t location, float v0)
{
    if (glUniform1f_ptr && location >= 0) {
        glUniform1f_ptr(location, v0);
        logGLError("host_gl_uniform1f");
    }
}

void host_gl_uniform3f(int64_t location, float v0, float v1, float v2)
{
    if (glUniform3f_ptr && location >= 0) {
        glUniform3f_ptr(location, v0, v1, v2);
        logGLError("host_gl_uniform3f");
    }
}

void host_gl_uniform_matrix4fv(int64_t location, const float* value)
{
    if (glUniformMatrix4fv_ptr && location >= 0) {
        glUniformMatrix4fv_ptr(location, 1, GL_FALSE, value);
        logGLError("host_gl_uniform_matrix4fv");
    }
}

// VAO functions
struct GLVAO {
    GLuint id;
};

static std::unordered_map<uint32_t, GLVAO> vaoMap;
static uint32_t nextVAOHandle = 0x6000;

uint32_t host_gl_gen_vertex_array()
{
    if (!glGenVertexArrays_ptr) {
        std::cerr << "VAO not supported" << std::endl;
        return 0;
    }

    GLuint vaoId;
    glGenVertexArrays_ptr(1, &vaoId);

    uint32_t handle = nextVAOHandle++;
    vaoMap[handle] = {vaoId};

    logGLError("host_gl_gen_vertex_array");
    return handle;
}

void host_gl_delete_vertex_array(uint32_t handle)
{
    if (!glDeleteVertexArrays_ptr || vaoMap.find(handle) == vaoMap.end()) {
        return;
    }

    GLuint vaoId = vaoMap[handle].id;
    glDeleteVertexArrays_ptr(1, &vaoId);
    vaoMap.erase(handle);

    logGLError("host_gl_delete_vertex_array");
}

void host_gl_bind_vertex_array(uint32_t handle)
{
    if (!glBindVertexArray_ptr) {
        return;
    }

    if (handle == 0) {
        glBindVertexArray_ptr(0);
        return;
    }

    if (vaoMap.find(handle) == vaoMap.end()) {
        return;
    }

    glBindVertexArray_ptr(vaoMap[handle].id);
    logGLError("host_gl_bind_vertex_array");
}

void host_gl_vertex_attrib_pointer(uint64_t index, int64_t size, uint64_t type,
                                   uint64_t normalized, int64_t stride, const void* pointer)
{
    if (glVertexAttribPointer_ptr) {
        glVertexAttribPointer_ptr(index, size, type, normalized, stride, pointer);
        logGLError("host_gl_vertex_attrib_pointer");
    }
}

void host_gl_enable_vertex_attrib_array(uint64_t index)
{
    if (glEnableVertexAttribArray_ptr) {
        glEnableVertexAttribArray_ptr(index);
        logGLError("host_gl_enable_vertex_attrib_array");
    }
}

void host_gl_disable_vertex_attrib_array(uint64_t index)
{
    if (glDisableVertexAttribArray_ptr) {
        glDisableVertexAttribArray_ptr(index);
        logGLError("host_gl_disable_vertex_attrib_array");
    }
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

    // Modern OpenGL - Shaders
    (void*)host_gl_create_shader,
    (void*)host_gl_delete_shader,
    (void*)host_gl_shader_source,
    (void*)host_gl_compile_shader,

    // Modern OpenGL - Programs
    (void*)host_gl_create_program,
    (void*)host_gl_delete_program,
    (void*)host_gl_attach_shader,
    (void*)host_gl_link_program,
    (void*)host_gl_use_program,
    (void*)host_gl_get_uniform_location,
    (void*)host_gl_uniform1f,
    (void*)host_gl_uniform3f,
    (void*)host_gl_uniform_matrix4fv,

    // Modern OpenGL - VAOs
    (void*)host_gl_gen_vertex_array,
    (void*)host_gl_delete_vertex_array,
    (void*)host_gl_bind_vertex_array,
    (void*)host_gl_vertex_attrib_pointer,
    (void*)host_gl_enable_vertex_attrib_array,
    (void*)host_gl_disable_vertex_attrib_array,
};

#endif
#endif

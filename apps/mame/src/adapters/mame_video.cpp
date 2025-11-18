/*
 * MAME Video/Graphics Adapter Implementation
 *
 * Maps MAME video output to ChrysaLisp GUI functions.
 * Creates a framebuffer that MAME renders to, then blits to the ChrysaLisp GUI.
 */

#include "../../include/mame_pii_adapter.h"
#include <stdlib.h>
#include <string.h>

// Global video state
static mame_video_info_t g_video_info = {0};
static uint32_t* g_framebuffer = nullptr;
static void* g_texture_handle = nullptr;  // ChrysaLisp texture handle

// Function pointer types for GUI operations
// These match the host_gui_funcs array in ChrysaLisp
typedef int (*gui_init_func_t)(uint32_t w, uint32_t h, uint32_t flags);
typedef void (*gui_deinit_func_t)(void);
typedef void (*gui_blit_func_t)(void* texture, uint32_t x, uint32_t y,
                                 uint32_t w, uint32_t h, uint32_t dx, uint32_t dy);
typedef void (*gui_flush_func_t)(uint32_t x, uint32_t y, uint32_t w, uint32_t h);
typedef void* (*gui_create_texture_func_t)(uint32_t w, uint32_t h,
                                            const uint32_t* pixels, uint32_t mode);
typedef void (*gui_destroy_texture_func_t)(void* texture);
typedef void (*gui_begin_composite_func_t)(void);
typedef void (*gui_end_composite_func_t)(void);

/*
 * Initialize video subsystem
 */
int mame_video_init(uint32_t width, uint32_t height)
{
    mame_log(MAME_LOG_INFO, "Initializing MAME video: %dx%d", width, height);

    void** gui_funcs = mame_adapter_get_gui_funcs();
    if (!gui_funcs) {
        mame_log(MAME_LOG_ERROR, "GUI functions not available");
        return -1;
    }

    // Allocate framebuffer
    size_t buffer_size = width * height * sizeof(uint32_t);
    g_framebuffer = (uint32_t*)mame_mem_alloc(buffer_size);
    if (!g_framebuffer) {
        mame_log(MAME_LOG_ERROR, "Failed to allocate framebuffer");
        return -1;
    }

    // Clear framebuffer to black
    memset(g_framebuffer, 0, buffer_size);

    // Initialize video info structure
    g_video_info.width = width;
    g_video_info.height = height;
    g_video_info.depth = 32;  // ARGB32
    g_video_info.pitch = width * 4;
    g_video_info.framebuffer = g_framebuffer;

    // Create ChrysaLisp texture from framebuffer
    // gui_create_texture is typically at index 12 in host_gui_funcs
    gui_create_texture_func_t gui_create_texture =
        (gui_create_texture_func_t)gui_funcs[12];

    if (gui_create_texture) {
        // Mode 0 = full ARGB texture (not alpha-only)
        g_texture_handle = gui_create_texture(width, height, g_framebuffer, 0);
        if (!g_texture_handle) {
            mame_log(MAME_LOG_ERROR, "Failed to create GUI texture");
            mame_mem_free(g_framebuffer, buffer_size);
            g_framebuffer = nullptr;
            return -1;
        }
    }

    mame_log(MAME_LOG_INFO, "Video initialized successfully");
    return 0;
}

/*
 * Shutdown video subsystem
 */
void mame_video_shutdown(void)
{
    mame_log(MAME_LOG_INFO, "Shutting down video");

    void** gui_funcs = mame_adapter_get_gui_funcs();

    // Destroy texture
    if (g_texture_handle && gui_funcs) {
        gui_destroy_texture_func_t gui_destroy_texture =
            (gui_destroy_texture_func_t)gui_funcs[11];
        if (gui_destroy_texture) {
            gui_destroy_texture(g_texture_handle);
        }
        g_texture_handle = nullptr;
    }

    // Free framebuffer
    if (g_framebuffer) {
        size_t buffer_size = g_video_info.width * g_video_info.height * sizeof(uint32_t);
        mame_mem_free(g_framebuffer, buffer_size);
        g_framebuffer = nullptr;
    }

    memset(&g_video_info, 0, sizeof(g_video_info));
}

/*
 * Get video info
 */
mame_video_info_t* mame_video_get_info(void)
{
    return &g_video_info;
}

/*
 * Update display - copy framebuffer to screen
 */
void mame_video_update(void)
{
    void** gui_funcs = mame_adapter_get_gui_funcs();
    if (!gui_funcs || !g_texture_handle || !g_framebuffer) {
        return;
    }

    // First, we need to update the texture with the current framebuffer contents
    // In ChrysaLisp, textures are typically updated by recreating them or
    // using a separate update function. For now, we'll just blit the existing texture.

    // Begin composite (index 13 in host_gui_funcs)
    gui_begin_composite_func_t gui_begin_composite =
        (gui_begin_composite_func_t)gui_funcs[13];
    if (gui_begin_composite) {
        gui_begin_composite();
    }

    // Blit texture to screen (index 2 in host_gui_funcs)
    gui_blit_func_t gui_blit = (gui_blit_func_t)gui_funcs[2];
    if (gui_blit) {
        // Blit from (0,0) in texture to (0,0) on screen, full size
        gui_blit(g_texture_handle,
                 0, 0,                              // Source x, y
                 g_video_info.width, g_video_info.height,  // Source width, height
                 0, 0);                             // Dest x, y
    }

    // End composite (index 14 in host_gui_funcs)
    gui_end_composite_func_t gui_end_composite =
        (gui_end_composite_func_t)gui_funcs[14];
    if (gui_end_composite) {
        gui_end_composite();
    }

    // Flush to screen (index 15 in host_gui_funcs)
    gui_flush_func_t gui_flush = (gui_flush_func_t)gui_funcs[15];
    if (gui_flush) {
        gui_flush(0, 0, g_video_info.width, g_video_info.height);
    }
}

/*
 * Set window title
 */
void mame_video_set_title(const char* title)
{
    // ChrysaLisp doesn't have a direct "set window title" PII function
    // Window titles are typically managed at the Lisp level through the GUI widgets
    // This would need to be handled by the Lisp launcher
    mame_log(MAME_LOG_DEBUG, "Window title: %s", title);
}

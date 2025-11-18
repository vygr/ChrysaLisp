/*
 * MAME OSD Layer for ChrysaLisp - Video Implementation
 *
 * This file maps MAME's video output to the ChrysaLisp adapter layer.
 */

#include "../../../../include/mame_pii_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// When integrated with MAME:
// #include "rendutil.h"
// #include "render.h"

// Current video configuration
static uint32_t g_screen_width = 0;
static uint32_t g_screen_height = 0;
static bool g_video_initialized = false;

/*
 * Initialize video subsystem
 *
 * Called by MAME when video output is needed
 */
extern "C" int osd_video_init(uint32_t width, uint32_t height)
{
    if (g_video_initialized && g_screen_width == width && g_screen_height == height) {
        return 0;  // Already initialized with correct size
    }

    // Shutdown if previously initialized with different size
    if (g_video_initialized) {
        osd_video_shutdown();
    }

    printf("[OSD] Initializing video: %dx%d\n", width, height);

    // Initialize via adapter
    if (mame_video_init(width, height) != 0) {
        fprintf(stderr, "[OSD] ERROR: Failed to initialize video\n");
        return -1;
    }

    g_screen_width = width;
    g_screen_height = height;
    g_video_initialized = true;

    return 0;
}

/*
 * Shutdown video subsystem
 */
extern "C" void osd_video_shutdown(void)
{
    if (!g_video_initialized) {
        return;
    }

    printf("[OSD] Shutting down video\n");

    mame_video_shutdown();

    g_video_initialized = false;
    g_screen_width = 0;
    g_screen_height = 0;
}

/*
 * Update video display
 *
 * MAME calls this every frame to render the screen
 *
 * In a full MAME integration, this would receive:
 * - render_primitive_list: List of primitives to draw
 * - render_target: The target to render to
 *
 * For now, we assume MAME has already rendered to a bitmap
 * and we just need to copy it to our framebuffer.
 */
extern "C" void osd_video_update(const void* bitmap, uint32_t width, uint32_t height,
                                  uint32_t pitch)
{
    if (!g_video_initialized) {
        return;
    }

    mame_video_info_t* video_info = mame_video_get_info();
    if (!video_info || !video_info->framebuffer) {
        return;
    }

    // Ensure video is right size
    if (width != video_info->width || height != video_info->height) {
        // Reinitialize with new size
        osd_video_init(width, height);
        video_info = mame_video_get_info();
        if (!video_info) return;
    }

    // Copy bitmap to framebuffer
    // Assuming bitmap is in ARGB32 format
    const uint32_t* src = (const uint32_t*)bitmap;
    uint32_t* dst = video_info->framebuffer;

    if (pitch == width * 4) {
        // Contiguous, single memcpy
        memcpy(dst, src, height * width * 4);
    } else {
        // Copy line by line (different pitch)
        for (uint32_t y = 0; y < height; y++) {
            const uint32_t* src_line = (const uint32_t*)((const uint8_t*)src + y * pitch);
            uint32_t* dst_line = dst + y * width;
            memcpy(dst_line, src_line, width * 4);
        }
    }

    // Update display
    mame_video_update();
}

/*
 * Get current video dimensions
 */
extern "C" void osd_video_get_size(uint32_t* width, uint32_t* height)
{
    if (width) *width = g_screen_width;
    if (height) *height = g_screen_height;
}

/*
 * Set window title
 */
extern "C" void osd_video_set_title(const char* title)
{
    if (title) {
        mame_video_set_title(title);
    }
}

/*
 * Get framebuffer for direct rendering
 *
 * Some MAME drivers might want direct access to the framebuffer
 */
extern "C" void* osd_video_get_framebuffer(void)
{
    if (!g_video_initialized) {
        return nullptr;
    }

    mame_video_info_t* video_info = mame_video_get_info();
    if (!video_info) {
        return nullptr;
    }

    return video_info->framebuffer;
}

/*
 * Wait for vertical blank (VSync)
 *
 * Used for timing synchronization
 */
extern "C" void osd_video_wait_vsync(void)
{
    // Simple implementation: sleep for one frame period at 60Hz
    // In a real implementation, this would sync with the actual display refresh
    mame_time_sleep(16667);  // 1/60th of a second in microseconds
}

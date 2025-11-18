/*
 * MAME OSD Layer for ChrysaLisp - Main Entry Point
 *
 * This file provides the main entry point and initialization for MAME
 * running on ChrysaLisp.
 */

#include "../../../../include/mame_pii_adapter.h"

// MAME headers (will be available when integrated with MAME source)
// #include "emu.h"
// #include "emuopts.h"
// #include "osdepend.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cmath>

/*
 * Global state
 */
static bool g_osd_initialized = false;
static void** g_host_os_funcs = nullptr;
static void** g_host_gui_funcs = nullptr;
static void** g_host_audio_funcs = nullptr;

/*
 * OSD Initialization
 *
 * Called by MAME before anything else.
 * This is where we initialize our adapter layer.
 */
extern "C" int osd_init(void** host_os_funcs,
                        void** host_gui_funcs,
                        void** host_audio_funcs)
{
    if (g_osd_initialized) {
        return 0;  // Already initialized
    }

    printf("=== MAME for ChrysaLisp ===\n");
    printf("Initializing OSD layer...\n");

    // Store PII function pointers
    g_host_os_funcs = host_os_funcs;
    g_host_gui_funcs = host_gui_funcs;
    g_host_audio_funcs = host_audio_funcs;

    // Initialize MAME adapter layer
    if (mame_adapter_init(host_os_funcs, host_gui_funcs, host_audio_funcs) != 0) {
        fprintf(stderr, "ERROR: Failed to initialize MAME adapter\n");
        return -1;
    }

    // Initialize subsystems
    if (host_gui_funcs) {
        // Initialize video (640x480 default, will be resized by driver)
        if (mame_video_init(640, 480) != 0) {
            fprintf(stderr, "WARNING: Failed to initialize video\n");
        }
    }

    if (host_audio_funcs) {
        // Initialize audio (48kHz stereo)
        if (mame_audio_init(48000, 2) != 0) {
            fprintf(stderr, "WARNING: Failed to initialize audio\n");
        }
    }

    // Initialize input
    if (mame_input_init() != 0) {
        fprintf(stderr, "WARNING: Failed to initialize input\n");
    }

    g_osd_initialized = true;
    printf("OSD layer initialized successfully\n");

    return 0;
}

/*
 * OSD Shutdown
 *
 * Called by MAME during cleanup.
 */
extern "C" void osd_exit(void)
{
    if (!g_osd_initialized) {
        return;
    }

    printf("Shutting down OSD layer...\n");

    // Shutdown subsystems
    mame_input_shutdown();
    mame_audio_shutdown();
    mame_video_shutdown();

    // Shutdown adapter
    mame_adapter_shutdown();

    g_osd_initialized = false;
    printf("OSD layer shutdown complete\n");
}

/*
 * Main entry point for standalone MAME
 *
 * When MAME is built as a standalone binary, this is the main() function.
 * When integrated with ChrysaLisp, this is called via FFI.
 */
extern "C" int mame_chrysalisp_main(int argc, char* argv[],
                                     void** host_os_funcs,
                                     void** host_gui_funcs,
                                     void** host_audio_funcs)
{
    printf("MAME for ChrysaLisp starting...\n");

    // Initialize OSD
    if (osd_init(host_os_funcs, host_gui_funcs, host_audio_funcs) != 0) {
        fprintf(stderr, "FATAL: OSD initialization failed\n");
        return -1;
    }

    // TODO: Call into MAME's actual main code
    // This would be something like:
    //
    // emu_options options;
    // cli_frontend frontend(options);
    // int result = frontend.execute(argc, argv);

    printf("\n");
    printf("NOTE: This is a stub implementation.\n");
    printf("To complete the port:\n");
    printf("1. Clone MAME source to apps/mame/mame-src/\n");
    printf("2. Integrate this OSD layer with MAME build\n");
    printf("3. Build with: make SUBTARGET=chrysalisp NOTHREADS=1\n");
    printf("\n");

    // For now, just run a simple test
    printf("Running adapter tests...\n");

    // Test video update
    mame_video_info_t* video_info = mame_video_get_info();
    if (video_info && video_info->framebuffer) {
        printf("Video: %dx%d framebuffer ready\n",
               video_info->width, video_info->height);

        // Draw a test pattern
        for (uint32_t y = 0; y < video_info->height; y++) {
            for (uint32_t x = 0; x < video_info->width; x++) {
                uint32_t r = (x * 255) / video_info->width;
                uint32_t g = (y * 255) / video_info->height;
                uint32_t b = 128;
                video_info->framebuffer[y * video_info->width + x] =
                    0xFF000000 | (r << 16) | (g << 8) | b;
            }
        }
        mame_video_update();
        printf("Test pattern displayed\n");
    }

    // Test audio
    mame_audio_info_t* audio_info = mame_audio_get_info();
    if (audio_info) {
        printf("Audio: %d Hz, %d channels ready\n",
               audio_info->sample_rate, audio_info->channels);

        // Generate a test tone (440Hz A note)
        int16_t test_samples[4800];  // 0.1 seconds at 48kHz
        for (int i = 0; i < 4800; i++) {
            double t = (double)i / 48000.0;
            test_samples[i] = (int16_t)(sin(2.0 * 3.14159 * 440.0 * t) * 16000.0);
        }
        mame_audio_queue_samples(test_samples, 4800);
        printf("Test tone queued\n");
    }

    // Wait a bit then cleanup
    printf("\nPress Ctrl+C to exit, or waiting 5 seconds...\n");
    mame_time_sleep(5000000);  // 5 seconds

    // Shutdown
    osd_exit();

    return 0;
}

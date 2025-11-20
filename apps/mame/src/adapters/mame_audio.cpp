/*
 * MAME Audio Adapter Implementation
 *
 * Maps MAME audio output to ChrysaLisp audio service.
 */

#include "../../include/mame_pii_adapter.h"
#include <string.h>

// Global audio state
static mame_audio_info_t g_audio_info = {0};
static int16_t* g_audio_buffer = nullptr;
static uint32_t g_buffer_capacity = 0;
static void* g_audio_stream_handle = nullptr;

// Function pointer types for audio operations
typedef int (*audio_init_func_t)(void);
typedef void (*audio_deinit_func_t)(void);
typedef void* (*audio_add_sfx_func_t)(const char* path);
typedef void (*audio_play_sfx_func_t)(void* sfx_handle, int32_t channel, int32_t loops);
typedef void (*audio_change_sfx_func_t)(int32_t channel, int32_t command);

// Audio streaming function types (new PII extensions)
typedef struct {
    uint32_t sample_rate;
    uint32_t channels;
    uint32_t bits_per_sample;
    uint32_t buffer_frames;
} audio_stream_config_t;

typedef void* (*audio_stream_open_func_t)(audio_stream_config_t* config);
typedef int32_t (*audio_stream_write_func_t)(void* stream, const int16_t* samples, uint32_t num_frames);
typedef void (*audio_stream_close_func_t)(void* stream);
typedef uint32_t (*audio_stream_get_queued_func_t)(void* stream);

/*
 * Initialize audio subsystem
 */
int mame_audio_init(uint32_t sample_rate, uint32_t channels)
{
    mame_log(MAME_LOG_INFO, "Initializing MAME audio: %d Hz, %d channels",
             sample_rate, channels);

    void** audio_funcs = mame_adapter_get_audio_funcs();
    if (!audio_funcs) {
        mame_log(MAME_LOG_WARNING, "Audio functions not available, running without sound");
        return -1;
    }

    // Initialize ChrysaLisp audio system (if not already done)
    audio_init_func_t audio_init_func = (audio_init_func_t)audio_funcs[0];
    if (audio_init_func) {
        audio_init_func();  // May already be initialized, ignore errors
    }

    // Set up audio info
    g_audio_info.sample_rate = sample_rate;
    g_audio_info.channels = channels;
    g_audio_info.bits = 16;
    g_audio_info.buffer_size = sample_rate / 60;  // ~1 frame at 60fps

    // Try to open audio stream using extended PII (index 6)
    audio_stream_open_func_t audio_stream_open =
        (audio_stream_open_func_t)audio_funcs[6];

    if (audio_stream_open) {
        // Use new streaming API
        audio_stream_config_t config;
        config.sample_rate = sample_rate;
        config.channels = channels;
        config.bits_per_sample = 16;
        config.buffer_frames = sample_rate / 60;  // 1 frame buffer

        g_audio_stream_handle = audio_stream_open(&config);
        if (g_audio_stream_handle) {
            mame_log(MAME_LOG_INFO, "Audio streaming initialized successfully");
            return 0;
        } else {
            mame_log(MAME_LOG_WARNING, "Failed to open audio stream");
        }
    } else {
        mame_log(MAME_LOG_WARNING, "Audio streaming not available in PII");
    }

    // Fallback: Allocate local buffer for queuing
    g_buffer_capacity = g_audio_info.buffer_size * channels * 2;
    g_audio_buffer = (int16_t*)mame_mem_alloc(g_buffer_capacity * sizeof(int16_t));
    if (!g_audio_buffer) {
        mame_log(MAME_LOG_ERROR, "Failed to allocate audio buffer");
        return -1;
    }

    memset(g_audio_buffer, 0, g_buffer_capacity * sizeof(int16_t));

    mame_log(MAME_LOG_INFO, "Audio initialized (buffered mode, no streaming)");
    return 0;
}

/*
 * Shutdown audio subsystem
 */
void mame_audio_shutdown(void)
{
    mame_log(MAME_LOG_INFO, "Shutting down audio");

    void** audio_funcs = mame_adapter_get_audio_funcs();

    // Close audio stream if open
    if (g_audio_stream_handle && audio_funcs) {
        audio_stream_close_func_t audio_stream_close =
            (audio_stream_close_func_t)audio_funcs[8];
        if (audio_stream_close) {
            audio_stream_close(g_audio_stream_handle);
        }
        g_audio_stream_handle = nullptr;
    }

    // Shutdown audio system
    if (audio_funcs) {
        audio_deinit_func_t audio_deinit_func = (audio_deinit_func_t)audio_funcs[1];
        if (audio_deinit_func) {
            audio_deinit_func();
        }
    }

    if (g_audio_buffer) {
        mame_mem_free(g_audio_buffer, g_buffer_capacity * sizeof(int16_t));
        g_audio_buffer = nullptr;
    }

    memset(&g_audio_info, 0, sizeof(g_audio_info));
    g_buffer_capacity = 0;
}

/*
 * Get audio info
 */
mame_audio_info_t* mame_audio_get_info(void)
{
    return &g_audio_info;
}

/*
 * Queue audio samples for playback
 */
void mame_audio_queue_samples(const int16_t* samples, uint32_t count)
{
    if (!samples || count == 0) {
        return;
    }

    // If we have a stream open, use it
    if (g_audio_stream_handle) {
        void** audio_funcs = mame_adapter_get_audio_funcs();
        if (!audio_funcs) {
            return;
        }

        audio_stream_write_func_t audio_stream_write =
            (audio_stream_write_func_t)audio_funcs[7];

        if (audio_stream_write) {
            // count is in samples, convert to frames
            uint32_t num_frames = count / g_audio_info.channels;

            int32_t written = audio_stream_write(g_audio_stream_handle, samples, num_frames);
            if (written < 0) {
                mame_log(MAME_LOG_WARNING, "Audio stream write failed");
            } else if ((uint32_t)written < num_frames) {
                mame_log(MAME_LOG_DEBUG, "Audio buffer full, dropped %d frames",
                         num_frames - written);
            }
        }
        return;
    }

    // Fallback: Just buffer locally (no playback)
    if (g_audio_buffer) {
        uint32_t copy_count = (count < g_buffer_capacity) ? count : g_buffer_capacity;
        memcpy(g_audio_buffer, samples, copy_count * sizeof(int16_t));
    }
}

/*
 * MAME OSD Layer for ChrysaLisp - Audio Implementation
 *
 * This file maps MAME's audio output to the ChrysaLisp adapter layer.
 */

#include "../../../../include/mame_pii_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Current audio configuration
static bool g_audio_initialized = false;
static uint32_t g_sample_rate = 48000;
static uint32_t g_channels = 2;

/*
 * Initialize audio subsystem
 *
 * Called by MAME when audio output is needed
 */
extern "C" int osd_audio_init(uint32_t sample_rate, uint32_t channels)
{
    if (g_audio_initialized) {
        // Already initialized, check if parameters match
        if (g_sample_rate == sample_rate && g_channels == channels) {
            return 0;  // Already initialized correctly
        }
        // Different parameters, reinitialize
        osd_audio_shutdown();
    }

    printf("[OSD] Initializing audio: %d Hz, %d channels\n", sample_rate, channels);

    // Initialize via adapter
    if (mame_audio_init(sample_rate, channels) != 0) {
        fprintf(stderr, "[OSD] WARNING: Failed to initialize audio\n");
        // Don't fail completely - continue without audio
        return 0;
    }

    g_sample_rate = sample_rate;
    g_channels = channels;
    g_audio_initialized = true;

    return 0;
}

/*
 * Shutdown audio subsystem
 */
extern "C" void osd_audio_shutdown(void)
{
    if (!g_audio_initialized) {
        return;
    }

    printf("[OSD] Shutting down audio\n");

    mame_audio_shutdown();

    g_audio_initialized = false;
}

/*
 * Queue audio samples
 *
 * MAME calls this to send audio data for playback
 *
 * samples: Interleaved sample data (LRLRLR for stereo)
 * num_samples: Total number of samples (not frames!)
 */
extern "C" void osd_audio_update(const int16_t* samples, uint32_t num_samples)
{
    if (!g_audio_initialized || !samples || num_samples == 0) {
        return;
    }

    // Queue samples via adapter
    mame_audio_queue_samples(samples, num_samples);
}

/*
 * Get audio buffer info
 *
 * MAME uses this to determine how many samples it should generate
 */
extern "C" uint32_t osd_audio_get_buffer_size(void)
{
    if (!g_audio_initialized) {
        return 0;
    }

    mame_audio_info_t* audio_info = mame_audio_get_info();
    if (!audio_info) {
        return 0;
    }

    return audio_info->buffer_size;
}

/*
 * Get current sample rate
 */
extern "C" uint32_t osd_audio_get_sample_rate(void)
{
    return g_sample_rate;
}

/*
 * Get number of channels
 */
extern "C" uint32_t osd_audio_get_channels(void)
{
    return g_channels;
}

/*
 * Set audio volume
 *
 * volume: 0.0 (mute) to 1.0 (full)
 */
extern "C" void osd_audio_set_volume(float volume)
{
    // TODO: Implement volume control
    // This would require extending the audio adapter
    // For now, just ignore
    (void)volume;
}

/*
 * Check if audio is available
 */
extern "C" bool osd_audio_is_available(void)
{
    return g_audio_initialized;
}

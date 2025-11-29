/*
 * ChrysaLisp Audio Streaming Extension
 *
 * Extends the audio PII to support continuous audio streaming,
 * required for emulators like MAME that generate audio samples in real-time.
 *
 * This extends the existing host_audio_funcs with streaming capabilities.
 */

#ifndef AUDIO_STREAMING_H
#define AUDIO_STREAMING_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Audio stream handle (opaque)
 */
typedef void* audio_stream_handle_t;

/*
 * Audio stream configuration
 */
typedef struct {
    uint32_t sample_rate;    // Samples per second (e.g., 48000)
    uint32_t channels;       // 1 = mono, 2 = stereo
    uint32_t bits_per_sample;// Typically 16
    uint32_t buffer_frames;  // Size of audio buffer in frames
} audio_stream_config_t;

/*
 * Audio stream functions
 *
 * These should be added to the host_audio_funcs table after the existing functions:
 * [0-5]: Existing SFX functions (init, deinit, add_sfx, play_sfx, change_sfx, remove_sfx)
 * [6]: pii_audio_stream_open
 * [7]: pii_audio_stream_write
 * [8]: pii_audio_stream_close
 * [9]: pii_audio_stream_get_queued
 */

/*
 * Open an audio stream for continuous playback
 *
 * Returns: stream handle on success, NULL on failure
 */
audio_stream_handle_t pii_audio_stream_open(audio_stream_config_t* config);

/*
 * Write audio samples to the stream
 *
 * samples: Interleaved sample data (e.g., LRLRLR for stereo)
 * num_frames: Number of frames (not samples! stereo frame = 2 samples)
 *
 * Returns: Number of frames actually written, or -1 on error
 */
int32_t pii_audio_stream_write(audio_stream_handle_t stream,
                                 const int16_t* samples,
                                 uint32_t num_frames);

/*
 * Close an audio stream
 */
void pii_audio_stream_close(audio_stream_handle_t stream);

/*
 * Get number of frames currently queued in the stream buffer
 *
 * Useful for maintaining proper buffer fill level
 * Returns: Number of frames queued
 */
uint32_t pii_audio_stream_get_queued(audio_stream_handle_t stream);

#ifdef __cplusplus
}
#endif

#endif // AUDIO_STREAMING_H

/*
 * SDL-based Audio Streaming Implementation
 *
 * Implements continuous audio streaming using SDL2's audio API.
 * This complements the existing audio_sdl.cpp SFX system.
 */

#include "audio_streaming.h"
#include <SDL2/SDL.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * Internal stream structure
 */
typedef struct {
    SDL_AudioDeviceID device_id;
    audio_stream_config_t config;
    int16_t* buffer;
    uint32_t buffer_size_frames;
    uint32_t write_pos;
    uint32_t read_pos;
    uint32_t queued_frames;
    SDL_mutex* mutex;
    bool active;
} audio_stream_internal_t;

/*
 * SDL audio callback
 *
 * Called by SDL when it needs more audio data
 */
static void audio_stream_callback(void* userdata, uint8_t* stream_out, int len)
{
    audio_stream_internal_t* s = (audio_stream_internal_t*)userdata;
    int16_t* output = (int16_t*)stream_out;
    uint32_t frames_needed = len / (sizeof(int16_t) * s->config.channels);

    SDL_LockMutex(s->mutex);

    uint32_t frames_to_copy = frames_needed;
    if (frames_to_copy > s->queued_frames) {
        frames_to_copy = s->queued_frames;
    }

    // Copy samples from ring buffer to output
    for (uint32_t i = 0; i < frames_to_copy; i++) {
        for (uint32_t ch = 0; ch < s->config.channels; ch++) {
            uint32_t buffer_idx = (s->read_pos * s->config.channels) + ch;
            output[i * s->config.channels + ch] = s->buffer[buffer_idx];
        }
        s->read_pos = (s->read_pos + 1) % s->buffer_size_frames;
    }

    s->queued_frames -= frames_to_copy;

    // Fill remaining with silence if we ran out of data
    if (frames_to_copy < frames_needed) {
        uint32_t silence_samples = (frames_needed - frames_to_copy) * s->config.channels;
        memset(output + (frames_to_copy * s->config.channels), 0,
               silence_samples * sizeof(int16_t));
    }

    SDL_UnlockMutex(s->mutex);
}

/*
 * Open audio stream
 */
audio_stream_handle_t pii_audio_stream_open(audio_stream_config_t* config)
{
    if (!config) {
        fprintf(stderr, "[AUDIO_STREAM] NULL config\n");
        return nullptr;
    }

    // Allocate stream structure
    audio_stream_internal_t* stream = (audio_stream_internal_t*)
        malloc(sizeof(audio_stream_internal_t));
    if (!stream) {
        fprintf(stderr, "[AUDIO_STREAM] Failed to allocate stream structure\n");
        return nullptr;
    }

    memset(stream, 0, sizeof(audio_stream_internal_t));
    stream->config = *config;

    // Allocate ring buffer (double the requested buffer size for safety)
    stream->buffer_size_frames = config->buffer_frames * 2;
    stream->buffer = (int16_t*)malloc(
        stream->buffer_size_frames * config->channels * sizeof(int16_t));

    if (!stream->buffer) {
        fprintf(stderr, "[AUDIO_STREAM] Failed to allocate audio buffer\n");
        free(stream);
        return nullptr;
    }

    memset(stream->buffer, 0,
           stream->buffer_size_frames * config->channels * sizeof(int16_t));

    // Create mutex for thread safety
    stream->mutex = SDL_CreateMutex();
    if (!stream->mutex) {
        fprintf(stderr, "[AUDIO_STREAM] Failed to create mutex\n");
        free(stream->buffer);
        free(stream);
        return nullptr;
    }

    // Set up SDL audio spec
    SDL_AudioSpec desired, obtained;
    SDL_zero(desired);
    desired.freq = config->sample_rate;
    desired.format = AUDIO_S16SYS;  // Signed 16-bit, system byte order
    desired.channels = config->channels;
    desired.samples = config->buffer_frames;
    desired.callback = audio_stream_callback;
    desired.userdata = stream;

    // Open SDL audio device
    stream->device_id = SDL_OpenAudioDevice(
        nullptr,  // Device name (NULL = default)
        0,        // iscapture (0 = playback)
        &desired,
        &obtained,
        0         // allowed_changes
    );

    if (stream->device_id == 0) {
        fprintf(stderr, "[AUDIO_STREAM] Failed to open audio device: %s\n",
                SDL_GetError());
        SDL_DestroyMutex(stream->mutex);
        free(stream->buffer);
        free(stream);
        return nullptr;
    }

    // Start playback
    SDL_PauseAudioDevice(stream->device_id, 0);
    stream->active = true;

    fprintf(stderr, "[AUDIO_STREAM] Opened: %d Hz, %d ch, %d frames buffer\n",
            config->sample_rate, config->channels, config->buffer_frames);

    return (audio_stream_handle_t)stream;
}

/*
 * Write samples to stream
 */
int32_t pii_audio_stream_write(audio_stream_handle_t handle,
                                const int16_t* samples,
                                uint32_t num_frames)
{
    if (!handle || !samples || num_frames == 0) {
        return -1;
    }

    audio_stream_internal_t* stream = (audio_stream_internal_t*)handle;

    SDL_LockMutex(stream->mutex);

    // Calculate available space
    uint32_t available = stream->buffer_size_frames - stream->queued_frames;
    uint32_t frames_to_write = num_frames;

    if (frames_to_write > available) {
        // Buffer full, drop oldest frames or limit write
        frames_to_write = available;
        fprintf(stderr, "[AUDIO_STREAM] WARNING: Buffer overrun, dropping frames\n");
    }

    // Copy samples to ring buffer
    for (uint32_t i = 0; i < frames_to_write; i++) {
        for (uint32_t ch = 0; ch < stream->config.channels; ch++) {
            uint32_t buffer_idx = (stream->write_pos * stream->config.channels) + ch;
            stream->buffer[buffer_idx] = samples[i * stream->config.channels + ch];
        }
        stream->write_pos = (stream->write_pos + 1) % stream->buffer_size_frames;
    }

    stream->queued_frames += frames_to_write;

    SDL_UnlockMutex(stream->mutex);

    return frames_to_write;
}

/*
 * Close audio stream
 */
void pii_audio_stream_close(audio_stream_handle_t handle)
{
    if (!handle) {
        return;
    }

    audio_stream_internal_t* stream = (audio_stream_internal_t*)handle;

    if (stream->active) {
        SDL_PauseAudioDevice(stream->device_id, 1);
        SDL_CloseAudioDevice(stream->device_id);
        stream->active = false;
    }

    if (stream->mutex) {
        SDL_DestroyMutex(stream->mutex);
    }

    if (stream->buffer) {
        free(stream->buffer);
    }

    free(stream);

    fprintf(stderr, "[AUDIO_STREAM] Closed\n");
}

/*
 * Get queued frame count
 */
uint32_t pii_audio_stream_get_queued(audio_stream_handle_t handle)
{
    if (!handle) {
        return 0;
    }

    audio_stream_internal_t* stream = (audio_stream_internal_t*)handle;

    SDL_LockMutex(stream->mutex);
    uint32_t queued = stream->queued_frames;
    SDL_UnlockMutex(stream->mutex);

    return queued;
}

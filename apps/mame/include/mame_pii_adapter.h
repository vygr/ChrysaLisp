/*
 * MAME PII Adapter Header
 *
 * This header provides the interface layer between MAME's OSD (Operating System Dependent)
 * layer and ChrysaLisp's Platform Interface Implementation (PII).
 *
 * It maps MAME's expected OS functionality to ChrysaLisp's host functions.
 */

#ifndef MAME_PII_ADAPTER_H
#define MAME_PII_ADAPTER_H

#include <stdint.h>
#include <stddef.h>
#include "../../../src/host/pii.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ============================================================================
 * File I/O Adapter
 * ============================================================================
 */

typedef struct mame_file_handle {
    int64_t fd;          // ChrysaLisp file descriptor
    uint64_t size;       // File size in bytes
    uint64_t position;   // Current file position
    int flags;           // Open flags
} mame_file_handle_t;

// File operations that map to ChrysaLisp PII
mame_file_handle_t* mame_file_open(const char* path, int mode);
int64_t mame_file_read(mame_file_handle_t* handle, void* buffer, uint64_t length);
int64_t mame_file_write(mame_file_handle_t* handle, const void* buffer, uint64_t length);
int64_t mame_file_seek(mame_file_handle_t* handle, int64_t offset, int whence);
int64_t mame_file_tell(mame_file_handle_t* handle);
void mame_file_close(mame_file_handle_t* handle);
uint64_t mame_file_size(mame_file_handle_t* handle);

// Directory enumeration
typedef void (*mame_dir_callback_t)(const char* filename, void* user_data);
int mame_dir_enum(const char* path, const char* pattern, mame_dir_callback_t callback, void* user_data);

/*
 * ============================================================================
 * Memory Management Adapter
 * ============================================================================
 */

// Memory allocation using ChrysaLisp PII mmap
void* mame_mem_alloc(size_t size);
void mame_mem_free(void* ptr, size_t size);
void* mame_mem_realloc(void* ptr, size_t old_size, size_t new_size);

// Memory-mapped file support (for ROM loading)
void* mame_mem_map_file(const char* path, size_t* out_size);
void mame_mem_unmap_file(void* addr, size_t size);

/*
 * ============================================================================
 * Timing Adapter
 * ============================================================================
 */

// Time functions using ChrysaLisp pii_gettime
uint64_t mame_time_usec(void);           // Get current time in microseconds
void mame_time_sleep(uint64_t usec);     // Sleep for specified microseconds

/*
 * ============================================================================
 * Video/Graphics Adapter
 * ============================================================================
 */

typedef struct mame_video_info {
    uint32_t width;          // Screen width in pixels
    uint32_t height;         // Screen height in pixels
    uint32_t depth;          // Bits per pixel (typically 32)
    uint32_t pitch;          // Bytes per scanline
    uint32_t* framebuffer;   // Pointer to pixel data (ARGB32)
} mame_video_info_t;

// Video initialization and control
int mame_video_init(uint32_t width, uint32_t height);
void mame_video_shutdown(void);
mame_video_info_t* mame_video_get_info(void);
void mame_video_update(void);            // Copy framebuffer to screen
void mame_video_set_title(const char* title);

/*
 * ============================================================================
 * Audio Adapter
 * ============================================================================
 */

typedef struct mame_audio_info {
    uint32_t sample_rate;    // Samples per second (e.g., 48000)
    uint32_t channels;       // Number of channels (1=mono, 2=stereo)
    uint32_t bits;           // Bits per sample (typically 16)
    uint32_t buffer_size;    // Size of audio buffer in samples
} mame_audio_info_t;

// Audio initialization and control
int mame_audio_init(uint32_t sample_rate, uint32_t channels);
void mame_audio_shutdown(void);
mame_audio_info_t* mame_audio_get_info(void);
void mame_audio_queue_samples(const int16_t* samples, uint32_t count);

/*
 * ============================================================================
 * Input Adapter
 * ============================================================================
 */

// Input types
typedef enum {
    MAME_INPUT_KEYBOARD,
    MAME_INPUT_JOYSTICK,
    MAME_INPUT_MOUSE,
    MAME_INPUT_COIN,
    MAME_INPUT_START
} mame_input_type_t;

// Input event
typedef struct mame_input_event {
    mame_input_type_t type;
    uint32_t code;           // Scan code or button number
    uint32_t pressed;        // 1 if pressed, 0 if released
} mame_input_event_t;

// Input functions
int mame_input_init(void);
void mame_input_shutdown(void);
int mame_input_poll(mame_input_event_t* event);  // Returns 1 if event available, 0 if not
void mame_input_process_chrysalisp_event(void* chrysalisp_msg); // Process ChrysaLisp GUI event

/*
 * ============================================================================
 * Threading Adapter
 * ============================================================================
 */

// Thread handle (maps to ChrysaLisp task)
typedef void* mame_thread_t;
typedef void* mame_mutex_t;
typedef void (*mame_thread_func_t)(void* param);

// Thread operations
mame_thread_t mame_thread_create(mame_thread_func_t func, void* param);
void mame_thread_join(mame_thread_t thread);
void mame_thread_yield(void);

// Mutex operations
mame_mutex_t mame_mutex_create(void);
void mame_mutex_destroy(mame_mutex_t mutex);
void mame_mutex_lock(mame_mutex_t mutex);
void mame_mutex_unlock(mame_mutex_t mutex);

/*
 * ============================================================================
 * Logging/Debug Adapter
 * ============================================================================
 */

typedef enum {
    MAME_LOG_ERROR,
    MAME_LOG_WARNING,
    MAME_LOG_INFO,
    MAME_LOG_DEBUG
} mame_log_level_t;

void mame_log(mame_log_level_t level, const char* format, ...);
void mame_log_set_level(mame_log_level_t level);

/*
 * ============================================================================
 * Main Adapter Interface
 * ============================================================================
 */

// Adapter initialization/shutdown
int mame_adapter_init(void* chrysalisp_host_os_funcs,
                      void* chrysalisp_host_gui_funcs,
                      void* chrysalisp_host_audio_funcs);
void mame_adapter_shutdown(void);

// Get function pointers to ChrysaLisp PII
void** mame_adapter_get_os_funcs(void);
void** mame_adapter_get_gui_funcs(void);
void** mame_adapter_get_audio_funcs(void);

#ifdef __cplusplus
}
#endif

#endif // MAME_PII_ADAPTER_H

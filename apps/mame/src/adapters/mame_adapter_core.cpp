/*
 * MAME PII Adapter Core Implementation
 *
 * This file implements the core adapter functionality that bridges MAME
 * to ChrysaLisp's Platform Interface Implementation (PII).
 */

#include "../../include/mame_pii_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

// Global pointers to ChrysaLisp PII function tables
static void** g_chrysalisp_os_funcs = nullptr;
static void** g_chrysalisp_gui_funcs = nullptr;
static void** g_chrysalisp_audio_funcs = nullptr;

// Current log level
static mame_log_level_t g_log_level = MAME_LOG_INFO;

/*
 * ============================================================================
 * Main Adapter Interface Implementation
 * ============================================================================
 */

int mame_adapter_init(void* chrysalisp_host_os_funcs,
                      void* chrysalisp_host_gui_funcs,
                      void* chrysalisp_host_audio_funcs)
{
    mame_log(MAME_LOG_INFO, "Initializing MAME ChrysaLisp adapter...");

    g_chrysalisp_os_funcs = (void**)chrysalisp_host_os_funcs;
    g_chrysalisp_gui_funcs = (void**)chrysalisp_host_gui_funcs;
    g_chrysalisp_audio_funcs = (void**)chrysalisp_host_audio_funcs;

    if (!g_chrysalisp_os_funcs) {
        mame_log(MAME_LOG_ERROR, "OS functions not provided!");
        return -1;
    }

    mame_log(MAME_LOG_INFO, "MAME adapter initialized successfully");
    return 0;
}

void mame_adapter_shutdown(void)
{
    mame_log(MAME_LOG_INFO, "Shutting down MAME adapter...");

    g_chrysalisp_os_funcs = nullptr;
    g_chrysalisp_gui_funcs = nullptr;
    g_chrysalisp_audio_funcs = nullptr;
}

void** mame_adapter_get_os_funcs(void)
{
    return g_chrysalisp_os_funcs;
}

void** mame_adapter_get_gui_funcs(void)
{
    return g_chrysalisp_gui_funcs;
}

void** mame_adapter_get_audio_funcs(void)
{
    return g_chrysalisp_audio_funcs;
}

/*
 * ============================================================================
 * Logging Implementation
 * ============================================================================
 */

void mame_log(mame_log_level_t level, const char* format, ...)
{
    if (level > g_log_level) {
        return;
    }

    const char* level_str;
    switch (level) {
        case MAME_LOG_ERROR:   level_str = "ERROR"; break;
        case MAME_LOG_WARNING: level_str = "WARN "; break;
        case MAME_LOG_INFO:    level_str = "INFO "; break;
        case MAME_LOG_DEBUG:   level_str = "DEBUG"; break;
        default:               level_str = "?????"; break;
    }

    fprintf(stderr, "[MAME:%s] ", level_str);

    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);

    fprintf(stderr, "\n");
}

void mame_log_set_level(mame_log_level_t level)
{
    g_log_level = level;
    mame_log(MAME_LOG_INFO, "Log level set to %d", level);
}

/*
 * ============================================================================
 * Timing Implementation
 * ============================================================================
 */

// Function pointer type for pii_gettime
typedef uint64_t (*pii_gettime_func_t)(void);
typedef void (*pii_sleep_func_t)(uint64_t usec);

uint64_t mame_time_usec(void)
{
    if (!g_chrysalisp_os_funcs) {
        return 0;
    }

    // pii_gettime is typically at index 7 in host_os_funcs
    // (see src/host/pii_*.cpp for the exact layout)
    pii_gettime_func_t pii_gettime = (pii_gettime_func_t)g_chrysalisp_os_funcs[7];
    if (pii_gettime) {
        return pii_gettime();
    }

    return 0;
}

void mame_time_sleep(uint64_t usec)
{
    if (!g_chrysalisp_os_funcs) {
        return;
    }

    // pii_sleep is typically at index 14 in host_os_funcs
    pii_sleep_func_t pii_sleep = (pii_sleep_func_t)g_chrysalisp_os_funcs[14];
    if (pii_sleep) {
        pii_sleep(usec);
    }
}

/*
 * ============================================================================
 * Memory Management Implementation
 * ============================================================================
 */

// Function pointer types for memory operations
typedef void* (*pii_mmap_func_t)(size_t len, int64_t fd, uint64_t mode);
typedef int64_t (*pii_munmap_func_t)(void* addr, size_t len, uint64_t mode);

void* mame_mem_alloc(size_t size)
{
    if (!g_chrysalisp_os_funcs) {
        return nullptr;
    }

    // Use pii_mmap for allocation (index 8 in host_os_funcs)
    pii_mmap_func_t pii_mmap = (pii_mmap_func_t)g_chrysalisp_os_funcs[8];
    if (pii_mmap) {
        return pii_mmap(size, -1, mmap_data);
    }

    return nullptr;
}

void mame_mem_free(void* ptr, size_t size)
{
    if (!g_chrysalisp_os_funcs || !ptr) {
        return;
    }

    // Use pii_munmap for deallocation (index 9 in host_os_funcs)
    pii_munmap_func_t pii_munmap = (pii_munmap_func_t)g_chrysalisp_os_funcs[9];
    if (pii_munmap) {
        pii_munmap(ptr, size, mmap_data);
    }
}

void* mame_mem_realloc(void* ptr, size_t old_size, size_t new_size)
{
    if (new_size == 0) {
        mame_mem_free(ptr, old_size);
        return nullptr;
    }

    void* new_ptr = mame_mem_alloc(new_size);
    if (!new_ptr) {
        return nullptr;
    }

    if (ptr && old_size > 0) {
        size_t copy_size = (old_size < new_size) ? old_size : new_size;
        memcpy(new_ptr, ptr, copy_size);
        mame_mem_free(ptr, old_size);
    }

    return new_ptr;
}

void* mame_mem_map_file(const char* path, size_t* out_size)
{
    // TODO: Implement file mapping using pii_open + pii_mmap
    // This is used for memory-mapping ROM files
    mame_log(MAME_LOG_WARNING, "mame_mem_map_file not yet implemented for: %s", path);
    return nullptr;
}

void mame_mem_unmap_file(void* addr, size_t size)
{
    if (addr) {
        mame_mem_free(addr, size);
    }
}

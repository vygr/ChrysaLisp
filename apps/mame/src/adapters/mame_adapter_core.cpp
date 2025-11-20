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
    if (!g_chrysalisp_os_funcs || !path) {
        return nullptr;
    }

    // Function pointer types
    typedef int64_t (*pii_open_func_t)(const char*, uint64_t);
    typedef int64_t (*pii_fstat_func_t)(const char*, void*);
    typedef void* (*pii_mmap_func_t)(size_t, int64_t, uint64_t);
    typedef uint64_t (*pii_close_func_t)(uint64_t);

    // Get file size first using pii_fstat (index 1)
    pii_fstat_func_t pii_fstat = (pii_fstat_func_t)g_chrysalisp_os_funcs[1];
    if (!pii_fstat) {
        mame_log(MAME_LOG_ERROR, "pii_fstat not available");
        return nullptr;
    }

    // ChrysaLisp stat structure
    struct {
        uint64_t mtime;
        uint64_t size;
        uint64_t mode;
    } stat_buf;

    if (pii_fstat(path, &stat_buf) != 0) {
        mame_log(MAME_LOG_ERROR, "Failed to stat file: %s", path);
        return nullptr;
    }

    size_t file_size = stat_buf.size;
    if (file_size == 0) {
        mame_log(MAME_LOG_WARNING, "File has zero size: %s", path);
        return nullptr;
    }

    // Open the file (index 2)
    pii_open_func_t pii_open = (pii_open_func_t)g_chrysalisp_os_funcs[2];
    if (!pii_open) {
        mame_log(MAME_LOG_ERROR, "pii_open not available");
        return nullptr;
    }

    int64_t fd = pii_open(path, file_open_read);
    if (fd == -1) {
        mame_log(MAME_LOG_ERROR, "Failed to open file: %s", path);
        return nullptr;
    }

    // Memory-map the file (index 8)
    pii_mmap_func_t pii_mmap = (pii_mmap_func_t)g_chrysalisp_os_funcs[8];
    if (!pii_mmap) {
        pii_close_func_t pii_close = (pii_close_func_t)g_chrysalisp_os_funcs[3];
        if (pii_close) pii_close(fd);
        mame_log(MAME_LOG_ERROR, "pii_mmap not available");
        return nullptr;
    }

    // Map the file into memory
    // Note: ChrysaLisp's pii_mmap can take a file descriptor as second parameter
    // for file-backed mapping
    void* mapped = pii_mmap(file_size, fd, mmap_data);

    // Close the file descriptor (we don't need it after mmap)
    pii_close_func_t pii_close = (pii_close_func_t)g_chrysalisp_os_funcs[3];
    if (pii_close) {
        pii_close(fd);
    }

    if (!mapped || mapped == (void*)-1) {
        mame_log(MAME_LOG_ERROR, "Failed to mmap file: %s", path);
        return nullptr;
    }

    if (out_size) {
        *out_size = file_size;
    }

    mame_log(MAME_LOG_DEBUG, "Memory-mapped file: %s (%zu bytes)", path, file_size);
    return mapped;
}

void mame_mem_unmap_file(void* addr, size_t size)
{
    if (addr) {
        mame_mem_free(addr, size);
    }
}

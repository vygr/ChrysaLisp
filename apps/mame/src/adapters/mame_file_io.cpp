/*
 * MAME File I/O Adapter Implementation
 *
 * Maps MAME file operations to ChrysaLisp PII file functions.
 */

#include "../../include/mame_pii_adapter.h"
#include <stdlib.h>
#include <string.h>

// Function pointer types for file operations
typedef int64_t (*pii_open_func_t)(const char* path, uint64_t mode);
typedef uint64_t (*pii_close_func_t)(uint64_t fd);
typedef int64_t (*pii_read_func_t)(int64_t fd, void* buffer, uint64_t length);
typedef int64_t (*pii_write_func_t)(int64_t fd, const void* buffer, uint64_t length);
typedef int64_t (*pii_seek_func_t)(int64_t fd, int64_t offset, uint64_t whence);
typedef int64_t (*pii_fstat_func_t)(const char* path, void* stat_buffer);
typedef void* (*pii_dirlist_func_t)(const char* path);

// Whence values for seek (matching ChrysaLisp/POSIX)
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/*
 * File open
 */
mame_file_handle_t* mame_file_open(const char* path, int mode)
{
    void** os_funcs = mame_adapter_get_os_funcs();
    if (!os_funcs || !path) {
        mame_log(MAME_LOG_ERROR, "Cannot open file: invalid parameters");
        return nullptr;
    }

    // Get pii_open function (index 2 in host_os_funcs)
    pii_open_func_t pii_open = (pii_open_func_t)os_funcs[2];
    if (!pii_open) {
        mame_log(MAME_LOG_ERROR, "pii_open not available");
        return nullptr;
    }

    // Convert mode to ChrysaLisp file_open flags
    uint64_t open_mode;
    if (mode & 0x02) {  // Write mode
        open_mode = file_open_write;
    } else {            // Read mode (default)
        open_mode = file_open_read;
    }

    // Open the file
    int64_t fd = pii_open(path, open_mode);
    if (fd == -1) {
        mame_log(MAME_LOG_WARNING, "Failed to open file: %s", path);
        return nullptr;
    }

    // Allocate handle structure
    mame_file_handle_t* handle = (mame_file_handle_t*)malloc(sizeof(mame_file_handle_t));
    if (!handle) {
        pii_close_func_t pii_close = (pii_close_func_t)os_funcs[3];
        if (pii_close) {
            pii_close(fd);
        }
        return nullptr;
    }

    handle->fd = fd;
    handle->position = 0;
    handle->flags = mode;

    // Get file size using pii_fstat (index 1 in host_os_funcs)
    pii_fstat_func_t pii_fstat = (pii_fstat_func_t)os_funcs[1];
    if (pii_fstat) {
        // ChrysaLisp stat structure (simplified)
        struct {
            uint64_t mtime;
            uint64_t size;
            uint64_t mode;
        } stat_buf;

        if (pii_fstat(path, &stat_buf) == 0) {
            handle->size = stat_buf.size;
        } else {
            handle->size = 0;
        }
    } else {
        handle->size = 0;
    }

    mame_log(MAME_LOG_DEBUG, "Opened file: %s (fd=%lld, size=%llu)",
             path, (long long)fd, (unsigned long long)handle->size);

    return handle;
}

/*
 * File read
 */
int64_t mame_file_read(mame_file_handle_t* handle, void* buffer, uint64_t length)
{
    if (!handle || !buffer) {
        return -1;
    }

    void** os_funcs = mame_adapter_get_os_funcs();
    if (!os_funcs) {
        return -1;
    }

    // Get pii_read function (index 5 in host_os_funcs)
    pii_read_func_t pii_read = (pii_read_func_t)os_funcs[5];
    if (!pii_read) {
        return -1;
    }

    int64_t bytes_read = pii_read(handle->fd, buffer, length);
    if (bytes_read > 0) {
        handle->position += bytes_read;
    }

    return bytes_read;
}

/*
 * File write
 */
int64_t mame_file_write(mame_file_handle_t* handle, const void* buffer, uint64_t length)
{
    if (!handle || !buffer) {
        return -1;
    }

    void** os_funcs = mame_adapter_get_os_funcs();
    if (!os_funcs) {
        return -1;
    }

    // Get pii_write function (index 6 in host_os_funcs)
    pii_write_func_t pii_write = (pii_write_func_t)os_funcs[6];
    if (!pii_write) {
        return -1;
    }

    int64_t bytes_written = pii_write(handle->fd, buffer, length);
    if (bytes_written > 0) {
        handle->position += bytes_written;
        if (handle->position > handle->size) {
            handle->size = handle->position;
        }
    }

    return bytes_written;
}

/*
 * File seek
 */
int64_t mame_file_seek(mame_file_handle_t* handle, int64_t offset, int whence)
{
    if (!handle) {
        return -1;
    }

    void** os_funcs = mame_adapter_get_os_funcs();
    if (!os_funcs) {
        return -1;
    }

    // Get pii_seek function (index 13 in host_os_funcs)
    pii_seek_func_t pii_seek = (pii_seek_func_t)os_funcs[13];
    if (!pii_seek) {
        return -1;
    }

    int64_t new_pos = pii_seek(handle->fd, offset, whence);
    if (new_pos >= 0) {
        handle->position = new_pos;
    }

    return new_pos;
}

/*
 * File tell - get current position
 */
int64_t mame_file_tell(mame_file_handle_t* handle)
{
    if (!handle) {
        return -1;
    }

    return handle->position;
}

/*
 * File close
 */
void mame_file_close(mame_file_handle_t* handle)
{
    if (!handle) {
        return;
    }

    void** os_funcs = mame_adapter_get_os_funcs();
    if (os_funcs) {
        // Get pii_close function (index 3 in host_os_funcs)
        pii_close_func_t pii_close = (pii_close_func_t)os_funcs[3];
        if (pii_close) {
            pii_close(handle->fd);
        }
    }

    mame_log(MAME_LOG_DEBUG, "Closed file (fd=%lld)", (long long)handle->fd);
    free(handle);
}

/*
 * Get file size
 */
uint64_t mame_file_size(mame_file_handle_t* handle)
{
    if (!handle) {
        return 0;
    }

    return handle->size;
}

/*
 * Directory enumeration
 *
 * ChrysaLisp's pii_dirlist returns a string containing directory entries
 * separated by newlines. Each entry has format: "filename\tsize\tmode\tmtime"
 */
int mame_dir_enum(const char* path, const char* pattern,
                  mame_dir_callback_t callback, void* user_data)
{
    void** os_funcs = mame_adapter_get_os_funcs();
    if (!os_funcs || !path || !callback) {
        return -1;
    }

    // Get pii_dirlist function (index 11 in host_os_funcs)
    pii_dirlist_func_t pii_dirlist = (pii_dirlist_func_t)os_funcs[11];
    if (!pii_dirlist) {
        mame_log(MAME_LOG_ERROR, "pii_dirlist not available");
        return -1;
    }

    // Call pii_dirlist to get directory contents
    // Returns a string object (str class in ChrysaLisp)
    void* dirlist_str = pii_dirlist(path);
    if (!dirlist_str) {
        mame_log(MAME_LOG_WARNING, "Failed to list directory: %s", path);
        return -1;
    }

    // ChrysaLisp string objects have this layout:
    // [obj_header][length:long][data:bytes...]
    // The string data starts at offset 16 (assuming 8-byte obj header + 8-byte length)
    char* dirlist_data = (char*)dirlist_str + 16;

    // Parse the directory listing
    // Format: "filename\tsize\tmode\tmtime\n..."
    char* line = dirlist_data;
    int count = 0;

    while (*line) {
        // Find end of line
        char* line_end = line;
        while (*line_end && *line_end != '\n') {
            line_end++;
        }

        // Extract filename (everything before first tab)
        char filename[512] = {0};
        char* tab = line;
        while (tab < line_end && *tab != '\t') {
            tab++;
        }

        size_t filename_len = tab - line;
        if (filename_len >= sizeof(filename)) {
            filename_len = sizeof(filename) - 1;
        }
        memcpy(filename, line, filename_len);
        filename[filename_len] = '\0';

        // Check if filename matches pattern (simple extension matching)
        bool matches = true;
        if (pattern) {
            // Simple pattern matching: check if filename ends with pattern
            // Pattern should be like ".zip" or "*.zip"
            const char* pattern_str = pattern;
            if (*pattern_str == '*') {
                pattern_str++; // Skip '*'
            }

            size_t pattern_len = strlen(pattern_str);
            if (filename_len >= pattern_len) {
                const char* filename_ext = filename + filename_len - pattern_len;
                if (strcmp(filename_ext, pattern_str) != 0) {
                    matches = false;
                }
            } else {
                matches = false;
            }
        }

        // Call callback if pattern matches
        if (matches && filename[0] != '\0') {
            callback(filename, user_data);
            count++;
        }

        // Move to next line
        if (*line_end == '\n') {
            line = line_end + 1;
        } else {
            break;
        }
    }

    // Note: We're not freeing dirlist_str here because ChrysaLisp uses
    // reference counting and the object is managed by the PII layer

    mame_log(MAME_LOG_DEBUG, "Enumerated directory '%s': found %d matching entries",
             path, count);

    return count;
}

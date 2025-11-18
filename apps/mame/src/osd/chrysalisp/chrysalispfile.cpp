/*
 * MAME OSD Layer for ChrysaLisp - File I/O Implementation
 *
 * This file maps MAME's file I/O operations to the ChrysaLisp adapter layer.
 */

#include "../../../../include/mame_pii_adapter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// When integrated with MAME, include MAME headers:
// #include "osdfile.h"
// #include "osdcore.h"

/*
 * OSD File Handle
 *
 * MAME's OSD uses an opaque handle for file operations.
 * We wrap our adapter's file handle.
 */
typedef struct osd_file {
    mame_file_handle_t* adapter_handle;
    char* path;
    bool is_open;
} osd_file;

/*
 * Open a file
 *
 * MAME signature:
 * osd_file_error osd_open(const char *path, uint32_t openflags,
 *                         osd_file **file, uint64_t *filesize)
 */
extern "C" int osd_file_open(const char* path, uint32_t openflags,
                              osd_file** file, uint64_t* filesize)
{
    if (!path || !file) {
        return -1;  // FILERR_INVALID_PARAMETER
    }

    // Allocate OSD file structure
    osd_file* f = (osd_file*)malloc(sizeof(osd_file));
    if (!f) {
        return -2;  // FILERR_OUT_OF_MEMORY
    }

    memset(f, 0, sizeof(osd_file));

    // Convert MAME open flags to our mode
    // MAME flags: OPEN_FLAG_READ (1), OPEN_FLAG_WRITE (2), OPEN_FLAG_CREATE (4)
    int mode = 0;
    if (openflags & 0x01) mode |= 0x00;  // Read
    if (openflags & 0x02) mode |= 0x02;  // Write

    // Open via adapter
    f->adapter_handle = mame_file_open(path, mode);
    if (!f->adapter_handle) {
        free(f);
        return -3;  // FILERR_NOT_FOUND or FILERR_ACCESS_DENIED
    }

    // Store path
    f->path = strdup(path);
    f->is_open = true;

    // Get file size if requested
    if (filesize) {
        *filesize = mame_file_size(f->adapter_handle);
    }

    *file = f;
    return 0;  // FILERR_NONE
}

/*
 * Close a file
 */
extern "C" int osd_file_close(osd_file* file)
{
    if (!file) {
        return -1;
    }

    if (file->adapter_handle) {
        mame_file_close(file->adapter_handle);
        file->adapter_handle = nullptr;
    }

    if (file->path) {
        free(file->path);
        file->path = nullptr;
    }

    file->is_open = false;
    free(file);

    return 0;
}

/*
 * Read from a file
 */
extern "C" int osd_file_read(osd_file* file, void* buffer,
                             uint64_t offset, uint32_t length,
                             uint32_t* actual)
{
    if (!file || !file->adapter_handle || !buffer) {
        return -1;
    }

    // Seek to offset
    if (mame_file_seek(file->adapter_handle, offset, 0) < 0) {
        return -4;  // FILERR_SEEK_ERROR
    }

    // Read data
    int64_t bytes_read = mame_file_read(file->adapter_handle, buffer, length);
    if (bytes_read < 0) {
        return -5;  // FILERR_READ_ERROR
    }

    if (actual) {
        *actual = (uint32_t)bytes_read;
    }

    return 0;
}

/*
 * Write to a file
 */
extern "C" int osd_file_write(osd_file* file, const void* buffer,
                               uint64_t offset, uint32_t length,
                               uint32_t* actual)
{
    if (!file || !file->adapter_handle || !buffer) {
        return -1;
    }

    // Seek to offset
    if (mame_file_seek(file->adapter_handle, offset, 0) < 0) {
        return -4;  // FILERR_SEEK_ERROR
    }

    // Write data
    int64_t bytes_written = mame_file_write(file->adapter_handle, buffer, length);
    if (bytes_written < 0) {
        return -6;  // FILERR_WRITE_ERROR
    }

    if (actual) {
        *actual = (uint32_t)bytes_written;
    }

    return 0;
}

/*
 * Get file size
 */
extern "C" uint64_t osd_file_size(osd_file* file)
{
    if (!file || !file->adapter_handle) {
        return 0;
    }

    return mame_file_size(file->adapter_handle);
}

/*
 * Directory enumeration
 *
 * MAME calls this to find ROM files
 */

// Callback context for directory enumeration
typedef struct {
    void* user_callback;
    void* user_data;
    int count;
} dir_enum_context;

// Adapter to MAME callback
static void dir_enum_adapter(const char* filename, void* user_data)
{
    dir_enum_context* ctx = (dir_enum_context*)user_data;

    // Call MAME's callback
    // MAME callback signature: int (*callback)(const char *name, void *param)
    typedef int (*mame_dir_callback)(const char*, void*);
    mame_dir_callback cb = (mame_dir_callback)ctx->user_callback;

    if (cb) {
        cb(filename, ctx->user_data);
        ctx->count++;
    }
}

/*
 * Enumerate directory
 */
extern "C" int osd_enumerate_directory(const char* path, const char* pattern,
                                        void* callback, void* user_data)
{
    if (!path) {
        return -1;
    }

    dir_enum_context ctx;
    ctx.user_callback = callback;
    ctx.user_data = user_data;
    ctx.count = 0;

    int result = mame_dir_enum(path, pattern, dir_enum_adapter, &ctx);

    return (result >= 0) ? ctx.count : -1;
}

/*
 * Check if file exists
 */
extern "C" bool osd_file_exists(const char* path)
{
    if (!path) {
        return false;
    }

    // Try to open the file
    mame_file_handle_t* handle = mame_file_open(path, 0);
    if (handle) {
        mame_file_close(handle);
        return true;
    }

    return false;
}

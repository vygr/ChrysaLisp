/*
 * MAME Adapter Test Program
 *
 * This is a minimal test program that demonstrates the MAME adapter layer
 * working with ChrysaLisp's PII. It doesn't run actual MAME emulation yet,
 * but validates that all the adapter functions work correctly.
 */

#include "../include/mame_pii_adapter.h"
#include <stdio.h>
#include <string.h>

// Test helper macros
#define TEST_START(name) printf("Testing %s... ", name); fflush(stdout);
#define TEST_PASS() printf("PASS\n");
#define TEST_FAIL(msg) { printf("FAIL: %s\n", msg); return -1; }

/*
 * Test file I/O operations
 */
int test_file_io(void)
{
    TEST_START("file I/O");

    // Create a test file
    mame_file_handle_t* handle = mame_file_open("/tmp/mame_test.txt", 0x02);
    if (!handle) {
        TEST_FAIL("Failed to create test file");
    }

    // Write some data
    const char* test_data = "Hello from MAME adapter!";
    int64_t written = mame_file_write(handle, test_data, strlen(test_data));
    if (written != (int64_t)strlen(test_data)) {
        mame_file_close(handle);
        TEST_FAIL("Failed to write data");
    }

    mame_file_close(handle);

    // Read it back
    handle = mame_file_open("/tmp/mame_test.txt", 0x00);
    if (!handle) {
        TEST_FAIL("Failed to open test file for reading");
    }

    char read_buffer[256] = {0};
    int64_t bytes_read = mame_file_read(handle, read_buffer, sizeof(read_buffer));
    if (bytes_read <= 0 || strcmp(read_buffer, test_data) != 0) {
        mame_file_close(handle);
        TEST_FAIL("Read data doesn't match written data");
    }

    mame_file_close(handle);
    TEST_PASS();
    return 0;
}

/*
 * Test memory operations
 */
int test_memory(void)
{
    TEST_START("memory operations");

    // Allocate memory
    size_t size = 1024 * 1024;  // 1MB
    void* mem = mame_mem_alloc(size);
    if (!mem) {
        TEST_FAIL("Failed to allocate memory");
    }

    // Write to it
    memset(mem, 0xAB, size);

    // Verify
    uint8_t* bytes = (uint8_t*)mem;
    if (bytes[0] != 0xAB || bytes[size-1] != 0xAB) {
        mame_mem_free(mem, size);
        TEST_FAIL("Memory verification failed");
    }

    // Free it
    mame_mem_free(mem, size);

    TEST_PASS();
    return 0;
}

/*
 * Test timing functions
 */
int test_timing(void)
{
    TEST_START("timing functions");

    uint64_t start = mame_time_usec();
    if (start == 0) {
        TEST_FAIL("Failed to get time");
    }

    // Sleep for 100ms
    mame_time_sleep(100000);

    uint64_t end = mame_time_usec();
    uint64_t elapsed = end - start;

    // Should be approximately 100ms (allow ±50ms tolerance)
    if (elapsed < 50000 || elapsed > 150000) {
        char msg[128];
        snprintf(msg, sizeof(msg), "Sleep inaccurate: %llu us",
                 (unsigned long long)elapsed);
        TEST_FAIL(msg);
    }

    TEST_PASS();
    return 0;
}

/*
 * Test video operations
 */
int test_video(void)
{
    TEST_START("video operations");

    if (mame_video_init(320, 240) != 0) {
        TEST_FAIL("Failed to initialize video");
    }

    mame_video_info_t* info = mame_video_get_info();
    if (!info || info->width != 320 || info->height != 240) {
        mame_video_shutdown();
        TEST_FAIL("Video info incorrect");
    }

    if (!info->framebuffer) {
        mame_video_shutdown();
        TEST_FAIL("Framebuffer not allocated");
    }

    // Draw a test pattern
    for (uint32_t y = 0; y < info->height; y++) {
        for (uint32_t x = 0; x < info->width; x++) {
            uint32_t r = (x * 255) / info->width;
            uint32_t g = (y * 255) / info->height;
            uint32_t b = 128;
            info->framebuffer[y * info->width + x] =
                0xFF000000 | (r << 16) | (g << 8) | b;
        }
    }

    // Update display (this would show the test pattern)
    mame_video_update();

    mame_video_shutdown();
    TEST_PASS();
    return 0;
}

/*
 * Test input operations
 */
int test_input(void)
{
    TEST_START("input operations");

    if (mame_input_init() != 0) {
        TEST_FAIL("Failed to initialize input");
    }

    // Create a fake keyboard event
    uint64_t fake_event[5] = {
        0x01,  // EV_TYPE_KEY_DOWN
        0,     // target_id
        0,     // key code
        0x52,  // scan code (up arrow)
        0      // modifiers
    };

    mame_input_process_chrysalisp_event(fake_event);

    // Poll for the event
    mame_input_event_t input_event;
    if (!mame_input_poll(&input_event)) {
        mame_input_shutdown();
        TEST_FAIL("Failed to poll input event");
    }

    if (input_event.type != MAME_INPUT_JOYSTICK ||
        input_event.code != 0x52 ||
        input_event.pressed != 1) {
        mame_input_shutdown();
        TEST_FAIL("Input event data incorrect");
    }

    mame_input_shutdown();
    TEST_PASS();
    return 0;
}

/*
 * Main test entry point
 *
 * This would be called from ChrysaLisp with the PII function pointers
 */
extern "C" int mame_test_main(void** host_os_funcs,
                               void** host_gui_funcs,
                               void** host_audio_funcs)
{
    printf("=== MAME Adapter Test Suite ===\n");

    // Initialize adapter
    if (mame_adapter_init(host_os_funcs, host_gui_funcs, host_audio_funcs) != 0) {
        printf("FATAL: Failed to initialize adapter\n");
        return -1;
    }

    // Run tests
    int result = 0;

    if (test_memory() != 0) result = -1;
    if (test_timing() != 0) result = -1;
    if (test_file_io() != 0) result = -1;

    // Only test video/input if GUI functions are available
    if (host_gui_funcs) {
        if (test_video() != 0) result = -1;
        if (test_input() != 0) result = -1;
    } else {
        printf("Skipping video/input tests (no GUI functions)\n");
    }

    // Cleanup
    mame_adapter_shutdown();

    if (result == 0) {
        printf("\n=== ALL TESTS PASSED ===\n");
    } else {
        printf("\n=== SOME TESTS FAILED ===\n");
    }

    return result;
}

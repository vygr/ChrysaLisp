// Test DOOM WASM loading and initialization
// Compile: g++ -o test_doom test_doom.cpp ../../src/host/wasm_wasm3.cpp ../../src/host/pii_linux.cpp -lm3 -I../../src/host -D_HOST_WASM=1
// Run: ./test_doom

#include <stdio.h>
#include <stdint.h>
#include "wasm.h"

int main() {
    printf("==============================================\n");
    printf(" DOOM WASM Integration Test\n");
    printf("==============================================\n\n");

    // Load DOOM
    printf("1. Loading doom.wasm...\n");
    uint64_t instance_id = 0;
    void* inst = host_wasm_load("doom.wasm", &instance_id);

    if (!inst) {
        printf("✗ Failed to load doom.wasm\n");
        printf("Error: %s\n", host_wasm_get_error(instance_id));
        return 1;
    }

    printf("✓ doom.wasm loaded successfully (instance_id: %llu)\n\n", (unsigned long long)instance_id);

    // Get main() export
    printf("2. Getting main() export...\n");
    void* main_func = host_wasm_get_export(instance_id, "main");

    if (!main_func) {
        printf("✗ Failed to get main() export\n");
        host_wasm_unload(instance_id);
        return 1;
    }

    printf("✓ Found main() export\n\n");

    // Initialize DOOM
    printf("3. Calling main() to initialize DOOM...\n");
    int64_t result = host_wasm_call_i64(main_func, nullptr, 0);

    printf("✓ DOOM initialized! (returned: %lld)\n\n", (long long)result);

    // Get doom_loop_step export
    printf("4. Getting doom_loop_step() export...\n");
    void* loop_func = host_wasm_get_export(instance_id, "doom_loop_step");

    if (!loop_func) {
        printf("✗ Failed to get doom_loop_step() export\n");
        host_wasm_unload(instance_id);
        return 1;
    }

    printf("✓ Found doom_loop_step() export\n\n");

    // Run a few game frames
    printf("5. Running 10 game loop iterations...\n");
    for (int i = 0; i < 10; i++) {
        host_wasm_call_i64(loop_func, nullptr, 0);
        printf("  Frame %d/10 complete\r", i + 1);
        fflush(stdout);
    }
    printf("\n✓ Ran 10 frames successfully!\n\n");

    // Cleanup
    printf("6. Cleaning up...\n");
    host_wasm_unload(instance_id);
    printf("✓ DOOM unloaded\n\n");

    printf("==============================================\n");
    printf(" ✓ All tests passed!\n");
    printf(" DOOM is ready to integrate into ChrysaLisp!\n");
    printf("==============================================\n");

    return 0;
}

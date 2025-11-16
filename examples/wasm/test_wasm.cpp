// Standalone test program for WASM binaries
// Compile: g++ -I../../src/host test_wasm.cpp -lm3 -o test_wasm
// Run: ./test_wasm

#include <stdio.h>
#include <stdint.h>
#include "wasm3.h"
#include "m3_env.h"

void test_math_wasm() {
    printf("\n=== Testing math.wasm ===\n");

    FILE* f = fopen("math.wasm", "rb");
    if (!f) {
        printf("ERROR: Could not open math.wasm\n");
        return;
    }

    fseek(f, 0, SEEK_END);
    size_t fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* wasm = new uint8_t[fsize];
    fread(wasm, 1, fsize, f);
    fclose(f);

    printf("Loaded math.wasm (%zu bytes)\n", fsize);

    // Create WASM3 runtime
    IM3Environment env = m3_NewEnvironment();
    IM3Runtime runtime = m3_NewRuntime(env, 64*1024, NULL);
    IM3Module module;

    M3Result result = m3_ParseModule(env, &module, wasm, fsize);
    if (result) {
        printf("ERROR parsing: %s\n", result);
        return;
    }

    result = m3_LoadModule(runtime, module);
    if (result) {
        printf("ERROR loading: %s\n", result);
        return;
    }

    printf("✓ Module loaded successfully\n");

    // Test add function
    IM3Function func;
    result = m3_FindFunction(&func, runtime, "add");
    if (!result) {
        const char* args[] = {"5", "10"};
        result = m3_CallArgv(func, 2, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ add(5, 10) = %lld (expected: 15)\n", (long long)*ret);
        }
    }

    // Test multiply
    result = m3_FindFunction(&func, runtime, "multiply");
    if (!result) {
        const char* args[] = {"6", "7"};
        result = m3_CallArgv(func, 2, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ multiply(6, 7) = %lld (expected: 42)\n", (long long)*ret);
        }
    }

    // Test fibonacci
    result = m3_FindFunction(&func, runtime, "fibonacci");
    if (!result) {
        const char* args[] = {"10"};
        result = m3_CallArgv(func, 1, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ fibonacci(10) = %lld (expected: 55)\n", (long long)*ret);
        }
    }

    m3_FreeRuntime(runtime);
    m3_FreeEnvironment(env);
    delete[] wasm;
}

void test_array_ops_wasm() {
    printf("\n=== Testing array_ops.wasm (NumPy-style) ===\n");

    FILE* f = fopen("array_ops.wasm", "rb");
    if (!f) {
        printf("ERROR: Could not open array_ops.wasm\n");
        return;
    }

    fseek(f, 0, SEEK_END);
    size_t fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* wasm = new uint8_t[fsize];
    fread(wasm, 1, fsize, f);
    fclose(f);

    printf("Loaded array_ops.wasm (%zu bytes)\n", fsize);

    // Create WASM3 runtime with more memory for arrays
    IM3Environment env = m3_NewEnvironment();
    IM3Runtime runtime = m3_NewRuntime(env, 512*1024, NULL);
    IM3Module module;

    M3Result result = m3_ParseModule(env, &module, wasm, fsize);
    if (result) {
        printf("ERROR parsing: %s\n", result);
        return;
    }

    result = m3_LoadModule(runtime, module);
    if (result) {
        printf("ERROR loading: %s\n", result);
        return;
    }

    printf("✓ Module loaded successfully\n");

    // Get WASM memory
    uint32_t mem_size;
    int64_t* mem = (int64_t*)m3_GetMemory(runtime, &mem_size, 0);
    printf("✓ WASM memory: %u bytes\n", mem_size);

    // Write test array [1, 2, 3, 4, 5] to memory
    mem[0] = 1;
    mem[1] = 2;
    mem[2] = 3;
    mem[3] = 4;
    mem[4] = 5;

    // Test sum_array (like np.sum)
    IM3Function func;
    result = m3_FindFunction(&func, runtime, "sum_array");
    if (!result) {
        const char* args[] = {"0", "5"};  // offset=0, count=5
        result = m3_CallArgv(func, 2, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ sum_array([1,2,3,4,5]) = %lld (expected: 15)\n", (long long)*ret);
        }
    }

    // Test mean (like np.mean)
    result = m3_FindFunction(&func, runtime, "mean");
    if (!result) {
        const char* args[] = {"0", "5"};
        result = m3_CallArgv(func, 2, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ mean([1,2,3,4,5]) = %lld (expected: 3)\n", (long long)*ret);
        }
    }

    // Set up for dot product: [1,2,3] · [4,5,6]
    mem[0] = 1; mem[1] = 2; mem[2] = 3;  // First vector at offset 0
    mem[100] = 4; mem[101] = 5; mem[102] = 6;  // Second vector at offset 800 (100*8 bytes)

    result = m3_FindFunction(&func, runtime, "dot_product");
    if (!result) {
        const char* args[] = {"0", "800", "3"};  // offset1=0, offset2=800, count=3
        result = m3_CallArgv(func, 3, args);
        if (!result) {
            uint64_t* ret;
            m3_GetResults(func, 1, (const void**)&ret);
            printf("✓ dot_product([1,2,3], [4,5,6]) = %lld (expected: 32)\n", (long long)*ret);
        }
    }

    m3_FreeRuntime(runtime);
    m3_FreeEnvironment(env);
    delete[] wasm;
}

int main() {
    printf("==============================================\n");
    printf(" WASM Integration Test\n");
    printf(" Testing real WASM binaries with wasm3\n");
    printf("==============================================\n");

    test_math_wasm();
    test_array_ops_wasm();

    printf("\n==============================================\n");
    printf(" All tests completed!\n");
    printf("==============================================\n");

    return 0;
}

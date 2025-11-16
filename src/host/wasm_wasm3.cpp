#include "wasm.h"
#include "pii.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Check if wasm3 should be enabled (controlled by build system)
#ifndef _HOST_WASM
#define _HOST_WASM 0
#endif

#if _HOST_WASM

#include "wasm3.h"
#include "m3_env.h"
#include <map>
#include <string>

// WASM instance structure
struct wasm_instance {
    IM3Environment env;
    IM3Runtime runtime;
    IM3Module module;
    std::string error;
    std::string filepath;
};

// Instance management - using a map for handle-based access
static std::map<uint64_t, wasm_instance*> instances;
static uint64_t next_instance_id = 1;

//
// Instance lifecycle
//

void* host_wasm_load(const char* wasm_file, uint64_t* instance_id) {
    if (!wasm_file || !instance_id) {
        return nullptr;
    }

    // Read WASM file using PII functions
    int64_t fd = pii_open(wasm_file, file_open_read);
    if (fd < 0) {
        return nullptr;
    }

    // Get file size
    uint64_t fsize = 0;
    pii_stat(wasm_file, &fsize, nullptr);

    // Allocate buffer and read file
    uint8_t* wasm_bytes = new uint8_t[fsize];
    int64_t bytes_read = pii_read(fd, wasm_bytes, fsize);
    pii_close(fd);

    if (bytes_read != (int64_t)fsize) {
        delete[] wasm_bytes;
        return nullptr;
    }

    // Create WASM3 environment
    wasm_instance* inst = new wasm_instance();
    inst->filepath = wasm_file;
    inst->env = m3_NewEnvironment();
    if (!inst->env) {
        inst->error = "Failed to create WASM3 environment";
        delete[] wasm_bytes;
        delete inst;
        return nullptr;
    }

    // Create runtime with 512KB stack
    inst->runtime = m3_NewRuntime(inst->env, 512*1024, nullptr);
    if (!inst->runtime) {
        inst->error = "Failed to create WASM3 runtime";
        m3_FreeEnvironment(inst->env);
        delete[] wasm_bytes;
        delete inst;
        return nullptr;
    }

    // Parse module
    M3Result result = m3_ParseModule(inst->env, &inst->module, wasm_bytes, fsize);
    delete[] wasm_bytes; // Don't need original bytes anymore

    if (result) {
        inst->error = std::string("Failed to parse WASM module: ") + result;
        m3_FreeRuntime(inst->runtime);
        m3_FreeEnvironment(inst->env);
        delete inst;
        return nullptr;
    }

    // Load module into runtime
    result = m3_LoadModule(inst->runtime, inst->module);
    if (result) {
        inst->error = std::string("Failed to load WASM module: ") + result;
        m3_FreeModule(inst->module);
        m3_FreeRuntime(inst->runtime);
        m3_FreeEnvironment(inst->env);
        delete inst;
        return nullptr;
    }

    // Assign instance ID and store
    *instance_id = next_instance_id++;
    instances[*instance_id] = inst;

    return inst;
}

void host_wasm_unload(uint64_t instance_id) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) {
        return;
    }

    wasm_instance* inst = it->second;

    // Clean up WASM3 resources
    // Note: m3_FreeRuntime also frees the module
    m3_FreeRuntime(inst->runtime);
    m3_FreeEnvironment(inst->env);

    delete inst;
    instances.erase(it);
}

//
// Function exports
//

void* host_wasm_get_export(uint64_t instance_id, const char* export_name) {
    auto it = instances.find(instance_id);
    if (it == instances.end() || !export_name) {
        return nullptr;
    }

    wasm_instance* inst = it->second;

    IM3Function func;
    M3Result result = m3_FindFunction(&func, inst->runtime, export_name);
    if (result) {
        inst->error = std::string("Export '") + export_name + "' not found: " + result;
        return nullptr;
    }

    return func;
}

int64_t host_wasm_call_i64(void* func_ptr, const int64_t* args, uint64_t arg_count) {
    if (!func_ptr) {
        return 0;
    }

    IM3Function func = (IM3Function)func_ptr;

    // Build argument list for wasm3
    // wasm3 expects arguments as strings for CallArgv
    const char* argv[16];
    char arg_buffers[16][32];

    if (arg_count > 16) {
        arg_count = 16; // Safety limit
    }

    for (uint64_t i = 0; i < arg_count; i++) {
        snprintf(arg_buffers[i], sizeof(arg_buffers[i]), "%lld", (long long)args[i]);
        argv[i] = arg_buffers[i];
    }

    // Call the function
    M3Result result = m3_CallArgv(func, arg_count, argv);
    if (result) {
        return 0; // Error - return 0
    }

    // Get return value (if any)
    uint64_t* return_val = nullptr;
    result = m3_GetResults(func, 1, (const void**)&return_val);
    if (result || !return_val) {
        return 0; // No return value or error
    }

    return (int64_t)(*return_val);
}

double host_wasm_call_f64(void* func_ptr, const double* args, uint64_t arg_count) {
    if (!func_ptr) {
        return 0.0;
    }

    IM3Function func = (IM3Function)func_ptr;

    // Build argument list for wasm3
    const char* argv[16];
    char arg_buffers[16][32];

    if (arg_count > 16) {
        arg_count = 16; // Safety limit
    }

    for (uint64_t i = 0; i < arg_count; i++) {
        snprintf(arg_buffers[i], sizeof(arg_buffers[i]), "%f", args[i]);
        argv[i] = arg_buffers[i];
    }

    // Call the function
    M3Result result = m3_CallArgv(func, arg_count, argv);
    if (result) {
        return 0.0; // Error - return 0
    }

    // Get return value (if any)
    double* return_val = nullptr;
    result = m3_GetResults(func, 1, (const void**)&return_val);
    if (result || !return_val) {
        return 0.0; // No return value or error
    }

    return *return_val;
}

//
// Memory access
//

void* host_wasm_get_memory(uint64_t instance_id, uint64_t* size) {
    auto it = instances.find(instance_id);
    if (it == instances.end() || !size) {
        if (size) *size = 0;
        return nullptr;
    }

    wasm_instance* inst = it->second;

    uint32_t mem_size;
    uint8_t* mem = m3_GetMemory(inst->runtime, &mem_size, 0);

    *size = (uint64_t)mem_size;
    return mem;
}

void host_wasm_write_memory(uint64_t instance_id, uint64_t offset, const void* data, uint64_t len) {
    if (!data || len == 0) {
        return;
    }

    uint64_t mem_size;
    uint8_t* mem = (uint8_t*)host_wasm_get_memory(instance_id, &mem_size);

    if (!mem || offset + len > mem_size) {
        return; // Out of bounds
    }

    memcpy(mem + offset, data, len);
}

void host_wasm_read_memory(uint64_t instance_id, uint64_t offset, void* buffer, uint64_t len) {
    if (!buffer || len == 0) {
        return;
    }

    uint64_t mem_size;
    uint8_t* mem = (uint8_t*)host_wasm_get_memory(instance_id, &mem_size);

    if (!mem || offset + len > mem_size) {
        return; // Out of bounds
    }

    memcpy(buffer, mem + offset, len);
}

//
// Error handling
//

const char* host_wasm_get_error(uint64_t instance_id) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) {
        return "Invalid instance ID";
    }

    return it->second->error.c_str();
}

//
// Host function vtable
//

void (*host_wasm_funcs[])() = {
    (void(*)())host_wasm_load,        // 0: Load WASM module
    (void(*)())host_wasm_unload,      // 1: Unload instance
    (void(*)())host_wasm_get_export,  // 2: Get exported function
    (void(*)())host_wasm_call_i64,    // 3: Call function (i64 return)
    (void(*)())host_wasm_call_f64,    // 4: Call function (f64 return)
    (void(*)())host_wasm_get_memory,  // 5: Get memory pointer
    (void(*)())host_wasm_write_memory,// 6: Write to memory
    (void(*)())host_wasm_read_memory, // 7: Read from memory
    (void(*)())host_wasm_get_error,   // 8: Get error message
};

#else // _HOST_WASM not enabled

//
// Stub implementation when WASM is disabled
//

void* host_wasm_load(const char* wasm_file, uint64_t* instance_id) {
    return nullptr;
}

void host_wasm_unload(uint64_t instance_id) {
}

void* host_wasm_get_export(uint64_t instance_id, const char* export_name) {
    return nullptr;
}

int64_t host_wasm_call_i64(void* func_ptr, const int64_t* args, uint64_t arg_count) {
    return 0;
}

double host_wasm_call_f64(void* func_ptr, const double* args, uint64_t arg_count) {
    return 0.0;
}

void* host_wasm_get_memory(uint64_t instance_id, uint64_t* size) {
    if (size) *size = 0;
    return nullptr;
}

void host_wasm_write_memory(uint64_t instance_id, uint64_t offset, const void* data, uint64_t len) {
}

void host_wasm_read_memory(uint64_t instance_id, uint64_t offset, void* buffer, uint64_t len) {
}

const char* host_wasm_get_error(uint64_t instance_id) {
    return "WASM support not enabled";
}

void (*host_wasm_funcs[])() = {
    (void(*)())host_wasm_load,
    (void(*)())host_wasm_unload,
    (void(*)())host_wasm_get_export,
    (void(*)())host_wasm_call_i64,
    (void(*)())host_wasm_call_f64,
    (void(*)())host_wasm_get_memory,
    (void(*)())host_wasm_write_memory,
    (void(*)())host_wasm_read_memory,
    (void(*)())host_wasm_get_error,
};

#endif // _HOST_WASM

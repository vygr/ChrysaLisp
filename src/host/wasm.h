#ifndef WASM_H
#define WASM_H

#include <stdint.h>
#include <stddef.h>

// WASM instance management
void* host_wasm_load(const char* wasm_file, uint64_t* instance_id);
void host_wasm_unload(uint64_t instance_id);

// Function exports
void* host_wasm_get_export(uint64_t instance_id, const char* export_name);
int64_t host_wasm_call_i64(void* func_ptr, const int64_t* args, uint64_t arg_count);
double host_wasm_call_f64(void* func_ptr, const double* args, uint64_t arg_count);

// Memory access
void* host_wasm_get_memory(uint64_t instance_id, uint64_t* size);
void host_wasm_write_memory(uint64_t instance_id, uint64_t offset, const void* data, uint64_t len);
void host_wasm_read_memory(uint64_t instance_id, uint64_t offset, void* buffer, uint64_t len);

// Error handling
const char* host_wasm_get_error(uint64_t instance_id);

// Vtable for host functions
extern void (*host_wasm_funcs[])();

#endif // WASM_H

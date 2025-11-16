# WASM Mounting Design for ChrysaLisp

## Executive Summary

ChrysaLisp can be extended to mount and execute arbitrary WebAssembly (WASM) modules in two modes:
1. **Command-line mode**: Execute WASM modules as pipeline commands
2. **GUI mode**: Mount WASM modules as interactive rectangles within ChrysaLisp windows

The existing architecture—particularly the host interface vtable system and the View hierarchy—provides an ideal foundation for WASM integration.

## Background

### Current State
- ChrysaLisp has WASM as a **planned feature** (README.md:69, docs/todo.md:76)
- Current mentions focus on compiling ChrysaLisp **TO** WASM, not loading WASM modules
- No existing WASM runtime integration

### Why WASM Mounting?
1. **Sandboxed execution** - Run untrusted code safely
2. **Performance** - Near-native speed for compute-intensive tasks
3. **Language interop** - Use Rust/C/C++/Go modules from ChrysaLisp
4. **Ecosystem access** - Leverage existing WASM tools and libraries
5. **Distributed compute** - WASM modules could run on any VP node

## Architecture Overview

### Three-Layer Design

```
┌─────────────────────────────────────────────────────────┐
│ Lisp Layer (class/wasm/lisp.inc, gui/wasm/lisp.inc)   │
│  - Wasm class (load/call/memory access)                │
│  - WasmView class (GUI integration)                     │
│  - High-level API for users                            │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│ VP Layer (sys/wasm/class.vp, sys/wasm/class.inc)      │
│  - FFI bindings to host functions                      │
│  - Method definitions (:load, :call, :get_memory)      │
│  - Type marshaling                                      │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│ Host Layer (src/host/wasm_*.cpp, src/host/wasm.h)     │
│  - WASM runtime integration (wasm3/wasmtime/WAMR)      │
│  - host_wasm_funcs vtable                              │
│  - Memory management and instance lifecycle            │
└─────────────────────────────────────────────────────────┘
```

## Host Layer Implementation

### New Vtable: `host_wasm_funcs`

Following the pattern of `host_os_funcs`, `host_gui_funcs`, and `host_audio_funcs`, add a fourth vtable:

**src/host/wasm.h:**
```cpp
#ifndef WASM_H
#define WASM_H

#include <stdint.h>
#include <stddef.h>

// WASM instance handle (opaque)
typedef struct wasm_instance wasm_instance_t;

// Function exports
extern void* host_wasm_load(const char* wasm_file, size_t* instance_id);
extern void host_wasm_unload(size_t instance_id);
extern void* host_wasm_get_export(size_t instance_id, const char* export_name);
extern int64_t host_wasm_call_i64(void* func_ptr, const int64_t* args, size_t arg_count);
extern double host_wasm_call_f64(void* func_ptr, const double* args, size_t arg_count);

// Memory access
extern void* host_wasm_get_memory(size_t instance_id, size_t* size);
extern void host_wasm_write_memory(size_t instance_id, size_t offset, const void* data, size_t len);
extern void host_wasm_read_memory(size_t instance_id, size_t offset, void* buffer, size_t len);

// Canvas for GUI
extern void* host_wasm_create_canvas(size_t width, size_t height);
extern void host_wasm_free_canvas(void* canvas_ptr);
extern void host_wasm_canvas_get_pixels(void* canvas_ptr, void** pixels, size_t* size);

// Error handling
extern const char* host_wasm_get_error(size_t instance_id);

#endif // WASM_H
```

**src/host/wasm_wasm3.cpp:**
```cpp
#include "wasm.h"
#include "wasm3.h"
#include "m3_env.h"
#include <map>
#include <vector>
#include <string>

// Instance management
static std::map<size_t, wasm_instance_t*> instances;
static size_t next_instance_id = 1;

struct wasm_instance {
    IM3Environment env;
    IM3Runtime runtime;
    IM3Module module;
    std::string error;
    void* canvas_buffer;
    size_t canvas_size;
};

void* host_wasm_load(const char* wasm_file, size_t* instance_id) {
    // Read WASM file
    FILE* f = fopen(wasm_file, "rb");
    if (!f) return nullptr;

    fseek(f, 0, SEEK_END);
    size_t fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* wasm_bytes = new uint8_t[fsize];
    fread(wasm_bytes, 1, fsize, f);
    fclose(f);

    // Create WASM3 environment
    wasm_instance_t* inst = new wasm_instance_t();
    inst->env = m3_NewEnvironment();
    inst->runtime = m3_NewRuntime(inst->env, 64*1024, nullptr); // 64KB stack

    M3Result result = m3_ParseModule(inst->env, &inst->module, wasm_bytes, fsize);
    delete[] wasm_bytes;

    if (result) {
        inst->error = result;
        delete inst;
        return nullptr;
    }

    result = m3_LoadModule(inst->runtime, inst->module);
    if (result) {
        inst->error = result;
        m3_FreeModule(inst->module);
        delete inst;
        return nullptr;
    }

    // Assign instance ID
    *instance_id = next_instance_id++;
    instances[*instance_id] = inst;

    return inst;
}

void host_wasm_unload(size_t instance_id) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) return;

    wasm_instance_t* inst = it->second;
    m3_FreeRuntime(inst->runtime);
    m3_FreeEnvironment(inst->env);
    if (inst->canvas_buffer) free(inst->canvas_buffer);
    delete inst;

    instances.erase(it);
}

void* host_wasm_get_export(size_t instance_id, const char* export_name) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) return nullptr;

    IM3Function func;
    M3Result result = m3_FindFunction(&func, it->second->runtime, export_name);
    if (result) {
        it->second->error = result;
        return nullptr;
    }

    return func;
}

int64_t host_wasm_call_i64(void* func_ptr, const int64_t* args, size_t arg_count) {
    IM3Function func = (IM3Function)func_ptr;

    // Call with arguments
    M3Result result = m3_CallArgv(func, arg_count, (const char**)args);
    if (result) return -1;

    // Get return value
    int64_t ret = 0;
    m3_GetResultsV(func, &ret);
    return ret;
}

void* host_wasm_get_memory(size_t instance_id, size_t* size) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) return nullptr;

    uint32_t mem_size;
    uint8_t* mem = m3_GetMemory(it->second->runtime, &mem_size, 0);
    *size = mem_size;
    return mem;
}

void host_wasm_write_memory(size_t instance_id, size_t offset, const void* data, size_t len) {
    size_t mem_size;
    uint8_t* mem = (uint8_t*)host_wasm_get_memory(instance_id, &mem_size);
    if (!mem || offset + len > mem_size) return;

    memcpy(mem + offset, data, len);
}

void host_wasm_read_memory(size_t instance_id, size_t offset, void* buffer, size_t len) {
    size_t mem_size;
    uint8_t* mem = (uint8_t*)host_wasm_get_memory(instance_id, &mem_size);
    if (!mem || offset + len > mem_size) return;

    memcpy(buffer, mem + offset, len);
}

void* host_wasm_create_canvas(size_t width, size_t height) {
    size_t size = width * height * 4; // RGBA
    return calloc(1, size);
}

void host_wasm_free_canvas(void* canvas_ptr) {
    free(canvas_ptr);
}

void host_wasm_canvas_get_pixels(void* canvas_ptr, void** pixels, size_t* size) {
    *pixels = canvas_ptr;
    // Size needs to be tracked separately
}

const char* host_wasm_get_error(size_t instance_id) {
    auto it = instances.find(instance_id);
    if (it == instances.end()) return "Invalid instance ID";
    return it->second->error.c_str();
}
```

### Runtime Choice: wasm3

**Rationale:**
- **Lightweight**: ~8KB runtime, perfect for embedded use
- **Simple C API**: Easy to integrate
- **No JIT required**: Works on all platforms (including ARM)
- **Good performance**: Interpreter is fast enough for most use cases
- **License**: MIT (compatible with ChrysaLisp)

**Alternatives considered:**
- **wasmtime**: Heavier but faster with JIT
- **WAMR**: Good middle ground, Bytecode Alliance project

## VP Layer Implementation

### Structure Definitions

**sys/wasm/class.inc:**
```lisp
(include "sys/class.inc")

(def-class 'sys_wasm 'sys_obj)
(dec-method 'vtable :load)
(dec-method 'vtable :unload)
(dec-method 'vtable :call)
(dec-method 'vtable :get_export)
(dec-method 'vtable :get_memory)
(dec-method 'vtable :write_memory)
(dec-method 'vtable :read_memory)
(dec-method 'vtable :get_error)
(def-method-end)

(def-struct 'wasm +obj_size)
    (ulong instance_id)
    (ptr filepath)
(def-struct-end)
```

**sys/wasm/class.vp:**
```lisp
(include "sys/wasm/class.inc")
(include "sys/func.inc")

(def-method 'sys_wasm :load)
    ;; inputs
    ;; r0 = filepath (C string)
    ;; outputs
    ;; r0 = instance_id (or 0 on error)
    ;; r1 = instance pointer
    ;; trashes
    ;; r1-r14

    (ptr-call host_wasm_load)
    (vp-ret)

(def-func-end)

(def-method 'sys_wasm :unload)
    ;; inputs
    ;; r0 = instance_id
    ;; outputs
    ;; trashes
    ;; r1-r14

    (ptr-call host_wasm_unload)
    (vp-ret)

(def-func-end)

(def-method 'sys_wasm :call)
    ;; inputs
    ;; r0 = function pointer (from get_export)
    ;; r1 = args array pointer
    ;; r2 = arg count
    ;; outputs
    ;; r0 = return value
    ;; trashes
    ;; r1-r14

    (ptr-call host_wasm_call_i64)
    (vp-ret)

(def-func-end)

(def-method 'sys_wasm :get_export)
    ;; inputs
    ;; r0 = instance_id
    ;; r1 = export_name (C string)
    ;; outputs
    ;; r0 = function pointer (or 0 if not found)
    ;; trashes
    ;; r1-r14

    (ptr-call host_wasm_get_export)
    (vp-ret)

(def-func-end)

;; Additional methods for memory access...
```

## Lisp Layer Implementation

### Core Wasm Class

**class/wasm/lisp.inc:**
```lisp
(include "sys/wasm/class.inc")

(defclass Wasm (filepath) nil
    ;; Load WASM module
    ;; inputs
    ;;   filepath: path to .wasm file
    ;; outputs
    ;;   instance: Wasm object or nil on error

    (defq instance_id 0 error_msg nil)

    ;; Allocate WASM object
    (defq this (alloc Wasm))

    ;; Store filepath
    (setf this +wasm_filepath (str-alloc filepath))

    ;; Load WASM module via FFI
    (defq result (sys-wasm-load (getf this +wasm_filepath)))
    (setq instance_id (elem 0 result))

    (if (= instance_id 0)
        ;; Load failed
        (progn
            (setq error_msg (sys-wasm-get-error 0))
            (throw "WASM load failed" error_msg))
        ;; Success
        (setf this +wasm_instance_id instance_id))

    this)

(defmethod :call (export_name . args)
    ;; Call a WASM export function
    ;; inputs
    ;;   export_name: string name of export
    ;;   args: variadic arguments (converted to i64)
    ;; outputs
    ;;   result: return value from WASM function

    (defq instance_id (getf this +wasm_instance_id))

    ;; Get export function pointer
    (defq func_ptr (sys-wasm-get-export instance_id export_name))
    (if (= func_ptr 0)
        (throw "Export not found" export_name))

    ;; Convert args to i64 array
    (defq arg_array (nums-new (length args)))
    (each! 0 args (# (nums-set arg_array %0 %1)))

    ;; Call the function
    (sys-wasm-call func_ptr arg_array (length args)))

(defmethod :get_memory ((offset 0) (size nil))
    ;; Read from WASM linear memory
    ;; inputs
    ;;   offset: byte offset (default 0)
    ;;   size: bytes to read (default all)
    ;; outputs
    ;;   data: str containing memory contents

    (defq instance_id (getf this +wasm_instance_id))

    ;; Get memory pointer and size
    (defq mem_info (sys-wasm-get-memory instance_id))
    (defq mem_ptr (elem 0 mem_info))
    (defq mem_size (elem 1 mem_info))

    ;; Read requested range
    (setd size (- mem_size offset))
    (defq buffer (str-alloc size))
    (sys-wasm-read-memory instance_id offset buffer size)
    buffer)

(defmethod :write_memory (offset data)
    ;; Write to WASM linear memory
    ;; inputs
    ;;   offset: byte offset
    ;;   data: str to write

    (defq instance_id (getf this +wasm_instance_id))
    (sys-wasm-write-memory instance_id offset data (length data)))

(defmethod :close ()
    ;; Unload WASM module

    (defq instance_id (getf this +wasm_instance_id))
    (if (!= instance_id 0)
        (progn
            (sys-wasm-unload instance_id)
            (setf this +wasm_instance_id 0)))

    ;; Free filepath
    (free (getf this +wasm_filepath))
    (setf this +wasm_filepath 0))
```

### GUI WasmView Class

**gui/wasm/lisp.inc:**
```lisp
(include "gui/view/class.inc")
(include "class/wasm/class.inc")

(defclass WasmView (filepath width height) (View)
    ;; Interactive WASM-powered view
    ;; inputs
    ;;   filepath: path to .wasm file
    ;;   width: view width
    ;;   height: view height
    ;; outputs
    ;;   view: WasmView instance
    ;;
    ;; WASM module should export:
    ;;   render(width, height) -> void
    ;;     Renders to linear memory at offset 0
    ;;     Format: ARGB32, row-major
    ;;   on_mouse(x, y, button) -> void (optional)
    ;;   on_key(keycode) -> void (optional)

    ;; Call parent constructor
    (defq this (. (View) :init width height 1))

    ;; Load WASM module
    (setf this +wasm_view_module (Wasm filepath))

    ;; Allocate pixel buffer in WASM memory
    (defq pixel_count (* width height))
    (defq pixel_bytes (* pixel_count 4))  ; ARGB32

    ;; Check WASM memory is large enough
    (defq mem_size (elem 1 (sys-wasm-get-memory
        (getf (getf this +wasm_view_module) +wasm_instance_id))))
    (if (< mem_size pixel_bytes)
        (throw "WASM memory too small for canvas"
               (cat "Need " pixel_bytes " bytes, have " mem_size)))

    ;; Store dimensions
    (setf this +wasm_view_width width)
    (setf this +wasm_view_height height)
    (setf this +wasm_view_pixel_bytes pixel_bytes)

    ;; Get render function
    (setf this +wasm_view_render_fn
        (sys-wasm-get-export
            (getf (getf this +wasm_view_module) +wasm_instance_id)
            "render"))

    this)

(defmethod :draw ()
    ;; Render WASM content and blit to view

    (defq wasm (getf this +wasm_view_module))
    (defq width (getf this +wasm_view_width))
    (defq height (getf this +wasm_view_height))

    ;; Call WASM render function
    (. wasm :call "render" width height)

    ;; Read pixel data from WASM memory
    (defq pixels (. wasm :get_memory 0 (getf this +wasm_view_pixel_bytes)))

    ;; Create texture from pixels
    ;; (Assumes host_gui_create_texture exists - may need to add)
    (defq texture (host-gui-create-texture pixels width height))

    ;; Blit to view
    (. this :ctx_blit texture 0 0 width height)

    ;; Free texture
    (host-gui-free-texture texture))

(defmethod :event (msg)
    ;; Forward events to WASM handlers

    (defq wasm (getf this +wasm_view_module))

    (cond
        ;; Mouse events
        ((= (getf msg +ev_msg_type) +ev_type_mouse)
            (defq x (getf msg +ev_msg_mouse_x))
            (defq y (getf msg +ev_msg_mouse_y))
            (defq buttons (getf msg +ev_msg_mouse_buttons))

            ;; Try to call on_mouse if it exists
            (catch
                (. wasm :call "on_mouse" x y buttons)
                (progn)))  ; Ignore if function doesn't exist

        ;; Keyboard events
        ((= (getf msg +ev_msg_type) +ev_type_key)
            (defq key (getf msg +ev_msg_key_keycode))

            (catch
                (. wasm :call "on_key" key)
                (progn))))

    ;; Mark dirty for redraw
    (. this :dirty))

(defmethod :close ()
    ;; Clean up WASM resources

    (. (getf this +wasm_view_module) :close)
    (. this :super :close))
```

## Command-Line Integration

### WASM Command Runner

**cmd/wasm.lisp:**
```lisp
(import "lib/argparse.inc")
(import "class/wasm/class.inc")

(defun main ()
    ;; Parse arguments
    ;; Usage: wasm <module.wasm> <export> [args...]

    (defq args (penv))

    (if (< (length args) 2)
        (progn
            (print "Usage: wasm <module.wasm> <export> [args...]")
            (print "")
            (print "Examples:")
            (print "  wasm math.wasm add 5 10")
            (print "  cat data.txt | wasm filter.wasm process")
            (exit 1)))

    (defq module_path (elem 0 args))
    (defq export_name (elem 1 args))
    (defq wasm_args (slice 2 -1 args))

    ;; Load WASM module
    (defq wasm (Wasm module_path))

    ;; If stdin is connected, read it into WASM memory
    (when (defq stdin (penv 'stdin))
        (defq input_data (str-read stdin))
        (. wasm :write_memory 0 input_data))

    ;; Convert string args to numbers
    (defq numeric_args (map (# (num %0)) wasm_args))

    ;; Call export
    (defq result (. wasm :call export_name . numeric_args))

    ;; Print result
    (when (defq stdout (penv 'stdout))
        (print result stdout))

    ;; Clean up
    (. wasm :close))
```

**Usage examples:**
```bash
# Simple calculation
wasm calculator.wasm add 42 58

# Filter pipeline
cat input.txt | wasm text_filter.wasm process | sort | unique

# Distributed execution (WASM runs on remote node!)
wasm heavy_compute.wasm crunch_data 1000000
```

## Example WASM Modules

### Example 1: Simple Math (Rust)

**math.rs:**
```rust
#[no_mangle]
pub extern "C" fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[no_mangle]
pub extern "C" fn multiply(a: i64, b: i64) -> i64 {
    a * b
}

#[no_mangle]
pub extern "C" fn fibonacci(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
```

Compile:
```bash
rustc --target wasm32-unknown-unknown -O --crate-type=cdylib math.rs -o math.wasm
```

Use from ChrysaLisp:
```lisp
(defq wasm (Wasm "math.wasm"))
(print (. wasm :call "add" 5 10))          ;; => 15
(print (. wasm :call "fibonacci" 20))      ;; => 6765
(. wasm :close)
```

### Example 2: Mandelbrot Renderer (C)

**mandelbrot.c:**
```c
#include <stdint.h>

// Canvas buffer in linear memory
uint32_t canvas[800 * 600];

uint32_t mandelbrot_pixel(double x0, double y0) {
    double x = 0.0, y = 0.0;
    int iteration = 0;
    int max_iteration = 256;

    while (x*x + y*y <= 4.0 && iteration < max_iteration) {
        double xtemp = x*x - y*y + x0;
        y = 2*x*y + y0;
        x = xtemp;
        iteration++;
    }

    // Color based on iteration
    uint8_t color = (iteration * 255) / max_iteration;
    return 0xFF000000 | (color << 16) | (color << 8) | color;
}

void render(int width, int height) {
    for (int py = 0; py < height; py++) {
        for (int px = 0; px < width; px++) {
            double x = (px - width/2.0) * 4.0 / width;
            double y = (py - height/2.0) * 4.0 / height;
            canvas[py * width + px] = mandelbrot_pixel(x, y);
        }
    }
}

void on_mouse(int x, int y, int buttons) {
    // Zoom on click
    // (Would update view bounds and re-render)
}
```

Compile:
```bash
clang --target=wasm32 -O3 -nostdlib -Wl,--no-entry \
      -Wl,--export=render -Wl,--export=on_mouse \
      mandelbrot.c -o mandelbrot.wasm
```

Use from ChrysaLisp:
```lisp
(import "gui/wasm/class.inc")

(defun main ()
    (defq window (create-window "Mandelbrot" 800 600))
    (defq mandel_view (WasmView "mandelbrot.wasm" 800 600))
    (. window :add_child mandel_view)
    (view-service))
```

### Example 3: Game of Life (AssemblyScript)

**life.as:**
```typescript
const width: i32 = 640;
const height: i32 = 480;

let current: Uint8Array = new Uint8Array(width * height);
let next: Uint8Array = new Uint8Array(width * height);
let pixels: Uint32Array = new Uint32Array(width * height);

function count_neighbors(x: i32, y: i32): i32 {
    let count: i32 = 0;
    for (let dy: i32 = -1; dy <= 1; dy++) {
        for (let dx: i32 = -1; dx <= 1; dx++) {
            if (dx == 0 && dy == 0) continue;

            let nx = (x + dx + width) % width;
            let ny = (y + dy + height) % height;

            if (current[ny * width + nx]) count++;
        }
    }
    return count;
}

export function step(): void {
    for (let y: i32 = 0; y < height; y++) {
        for (let x: i32 = 0; x < width; x++) {
            let idx = y * width + x;
            let neighbors = count_neighbors(x, y);

            if (current[idx]) {
                next[idx] = (neighbors == 2 || neighbors == 3) ? 1 : 0;
            } else {
                next[idx] = (neighbors == 3) ? 1 : 0;
            }
        }
    }

    // Swap buffers
    let temp = current;
    current = next;
    next = temp;
}

export function render(w: i32, h: i32): void {
    for (let i: i32 = 0; i < width * height; i++) {
        pixels[i] = current[i] ? 0xFFFFFFFF : 0xFF000000;
    }
}

export function on_mouse(x: i32, y: i32, buttons: i32): void {
    if (buttons) {
        current[y * width + x] = 1;
    }
}
```

Use with animation:
```lisp
(defq life_view (WasmView "life.wasm" 640 480))
(defq life_wasm (getf life_view +wasm_view_module))

;; Animate
(defun animate ()
    (. life_wasm :call "step")
    (. life_view :dirty)
    (mail-timeout 16667 (task-mailbox))  ; ~60 FPS
    (animate))

(animate)
```

## Implementation Plan

### Phase 1: Foundation (Week 1)
- [ ] Add wasm3 to build system (makefile updates)
- [ ] Implement `src/host/wasm_wasm3.cpp` basic functions
- [ ] Create `host_wasm_funcs` vtable
- [ ] Test basic load/call from C++ host

### Phase 2: VP Layer (Week 2)
- [ ] Create `sys/wasm/class.inc` structures
- [ ] Implement `sys/wasm/class.vp` methods
- [ ] Add FFI bindings
- [ ] Test VP-level WASM execution

### Phase 3: Lisp Layer (Week 2-3)
- [ ] Implement `class/wasm/lisp.inc` Wasm class
- [ ] Add memory marshaling utilities
- [ ] Create test suite for basic WASM modules
- [ ] Test with Rust/C compiled WASM

### Phase 4: Command Integration (Week 3)
- [ ] Create `cmd/wasm.lisp`
- [ ] Test in pipe pipelines
- [ ] Add stdin/stdout WASM integration
- [ ] Document command-line usage

### Phase 5: GUI Integration (Week 3-4)
- [ ] Implement `gui/wasm/lisp.inc` WasmView
- [ ] Add texture creation/blitting helpers
- [ ] Implement event forwarding
- [ ] Test with interactive WASM modules

### Phase 6: Advanced Features (Week 4)
- [ ] WASM → ChrysaLisp imports (callbacks)
- [ ] Better error handling and debugging
- [ ] Performance optimization
- [ ] Documentation and examples

## Integration Points

### Build System Changes

**Makefile additions:**
```makefile
# WASM3 library
WASM3_DIR = lib/wasm3
WASM3_LIB = $(WASM3_DIR)/libwasm3.a

$(WASM3_LIB):
	cd $(WASM3_DIR) && make

# Add to host compilation
$(HOST): $(WASM3_LIB)
	$(CXX) ... -I$(WASM3_DIR)/source $(WASM3_LIB) ...
```

### Boot Image Updates

Add WASM classes to boot image:
```lisp
;; In sys/boot.inc
(import "class/wasm/class.inc")
(import "gui/wasm/class.inc")
```

## Performance Considerations

### Memory Copying
**Challenge**: Copying pixel buffers every frame is expensive.

**Solutions:**
1. **Shared memory**: Map WASM linear memory directly to texture
2. **Dirty rectangles**: Only copy changed regions
3. **DMA**: Use host GPU memory mapping (platform-specific)

### WASM Call Overhead
**Challenge**: FFI overhead for frequent calls.

**Solutions:**
1. **Batching**: Group multiple operations in single WASM call
2. **Caching**: Cache function pointers
3. **JIT upgrade**: Switch to wasmtime for JIT compilation

### Distribution
**Opportunity**: WASM modules can run on **any VP node**!

```lisp
;; Run WASM on remote node
(defq remote_wasm (. (Task "wasm crunch.wasm process 1000000") :run))
```

## Security Considerations

### Sandboxing
WASM provides good sandboxing by default:
- No direct file system access
- No network access
- Limited memory access (own linear memory only)

### Import Restrictions
**Only expose safe imports:**
```cpp
// Safe: read-only data
m3_LinkRawFunction(module, "env", "get_time", "i()", &wasm_get_time);

// UNSAFE: Don't expose!
// m3_LinkRawFunction(module, "env", "system", ...);
```

### Resource Limits
Implement limits:
- Maximum memory size
- Execution timeout
- Call stack depth

## Future Enhancements

### WASI Support
Support WASI (WebAssembly System Interface):
- File system access (sandboxed)
- Command-line args
- Environment variables
- Standard I/O

### Component Model
Support WASM Component Model:
- Interface Types
- Module composition
- Better language interop

### Debugging
Add debugging support:
- Breakpoints
- Stack traces
- Memory inspection

### Compilation Target
Flip the direction: compile ChrysaLisp **TO** WASM:
- VP → WASM backend
- Run ChrysaLisp in browsers
- Distributed web nodes

## Conclusion

ChrysaLisp's architecture is **exceptionally well-suited** for WASM integration:

1. **Clean host interface**: vtable system makes adding WASM runtime straightforward
2. **FFI already exists**: Pattern for native code integration is proven
3. **View hierarchy**: Perfect for embedding WASM-rendered content
4. **Distributed nature**: WASM modules could run across the network
5. **No GC conflicts**: Reference counting doesn't interfere with WASM

The implementation is **moderate complexity** (~4 weeks) and would unlock:
- Safe execution of untrusted code
- Access to Rust/C/C++/Go ecosystem
- High-performance compute kernels
- Interactive multimedia applications
- Language-agnostic command-line tools

This positions ChrysaLisp as a **unique distributed WASM runtime** with seamless GUI integration—a capability no other Lisp system currently offers.

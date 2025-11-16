# DOOM WASM Status

## Current Status: 📦 Downloaded, 🔧 Implementation Needed

### ✅ What We Have

1. **doom.wasm** - 6.6MB WebAssembly binary
   - Source: https://github.com/diekmann/wasm-fizzbuzz/tree/main/doom
   - Verified: Valid WASM binary (file type confirmed)
   - Size: 6,683,648 bytes
   - Original: id Software DOOM 1993 (1997 Linux port)

2. **Complete Analysis**
   - JavaScript implementation reverse-engineered
   - All import functions documented
   - Memory requirements understood
   - Input handling mapped
   - Frame buffer format specified

### ⚠️ What's Missing (The Implementation Gap)

Our current WASM integration supports **exporting functions from WASM**, but DOOM needs **importing functions into WASM**.

**What DOOM needs**:
```
WASM → calls → JavaScript functions
```

**What we currently support**:
```
ChrysaLisp → calls → WASM functions
```

### Required Implementation

To run DOOM, we need to add **WASM import function support** to `src/host/wasm_wasm3.cpp`.

#### The Missing Piece: m3_LinkRawFunction

wasm3 provides `m3_LinkRawFunction()` to link C/C++ functions that WASM can call:

```cpp
// Example: What DOOM needs
m3_LinkRawFunction(module, "js", "js_draw_screen", "v(i)", &doom_js_draw_screen);
//                         ^^^^   ^^^^^^^^^^^^^^^   ^^^^    ^^^^^^^^^^^^^^^^^^^
//                       namespace  function name  signature   C function pointer
```

**Signatures**:
- `v(ii)` = void function(int, int)
- `F()` = double function()
- `v(i)` = void function(int)

#### Functions DOOM Needs to Import

1. **js_console_log(offset, length)** - signature: `v(ii)`
   - Read string from WASM memory, print to console

2. **js_stdout(offset, length)** - signature: `v(ii)`
   - Same as console_log

3. **js_stderr(offset, length)** - signature: `v(ii)`
   - Same as console_log

4. **js_milliseconds_since_start()** - signature: `F()`
   - Returns current time in milliseconds

5. **js_draw_screen(ptr)** - signature: `v(i)`
   - Read 640×400 RGBA frame buffer from WASM memory
   - Create texture and render to screen

### Implementation Plan

#### Phase 1: Basic Import Support (2-3 hours)

**File**: `src/host/wasm_wasm3.cpp`

Add import linking infrastructure:

```cpp
// Global registry of import functions
struct ImportFunction {
    const char* module;
    const char* name;
    const char* signature;
    M3RawCall function;
};

static ImportFunction doom_imports[] = {
    {"js", "js_console_log", "v(ii)", doom_js_console_log},
    {"js", "js_stdout", "v(ii)", doom_js_stdout},
    {"js", "js_stderr", "v(ii)", doom_js_stderr},
    {"js", "js_milliseconds_since_start", "F()", doom_js_get_time},
    {"js", "js_draw_screen", "v(i)", doom_js_draw_screen},
    {NULL, NULL, NULL, NULL}
};

// Link imports after loading module
M3Result link_imports(IM3Runtime runtime, IM3Module module, const char* wasm_file) {
    // Check if this is doom.wasm
    if (strstr(wasm_file, "doom.wasm")) {
        for (int i = 0; doom_imports[i].module; i++) {
            M3Result res = m3_LinkRawFunction(
                module,
                doom_imports[i].module,
                doom_imports[i].name,
                doom_imports[i].signature,
                doom_imports[i].function
            );
            if (res) return res;
        }
    }
    return m3Err_none;
}
```

Implement the import functions:

```cpp
m3ApiRawFunction(doom_js_console_log) {
    m3ApiReturnType(void)
    m3ApiGetArg(int32_t, offset)
    m3ApiGetArg(int32_t, length)

    // Get WASM memory
    uint8_t* mem = m3_GetMemory(runtime, NULL, 0);

    // Read string
    char* str = (char*)(mem + offset);

    // Print (using PII or printf)
    printf("[DOOM] %.*s\n", length, str);

    m3ApiSuccess();
}

m3ApiRawFunction(doom_js_get_time) {
    m3ApiReturnType(double)

    // Get milliseconds since start
    static auto start = std::chrono::steady_clock::now();
    auto now = std::chrono::steady_clock::now();
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now - start).count();

    m3ApiReturn((double)ms);
}

m3ApiRawFunction(doom_js_draw_screen) {
    m3ApiReturnType(void)
    m3ApiGetArg(int32_t, ptr)

    // Get WASM memory
    uint8_t* mem = m3_GetMemory(runtime, NULL, 0);

    // Frame buffer is 640x400 RGBA
    uint8_t* framebuffer = mem + ptr;

    // TODO: Copy to texture and render
    // For now, just acknowledge
    // (Need to integrate with ChrysaLisp GUI system)

    m3ApiSuccess();
}
```

#### Phase 2: Frame Buffer Rendering (2-3 hours)

**Problem**: Need to get frame buffer from WASM to ChrysaLisp screen

**Solution Options**:

1. **Direct SDL** (Quick but bypasses ChrysaLisp):
   ```cpp
   SDL_UpdateTexture(doom_texture, NULL, framebuffer, 640 * 4);
   SDL_RenderCopy(renderer, doom_texture, NULL, NULL);
   ```

2. **Via ChrysaLisp GUI** (Proper integration):
   - Store frame buffer pointer globally
   - Create `host_wasm_get_framebuffer()` function
   - DoomView reads it via `:get_memory()`
   - Create texture and blit in `:draw()`

#### Phase 3: Input Handling (1-2 hours)

Map ChrysaLisp keycodes to DOOM keycodes in DoomView:

```lisp
(defun convert-to-doom-key (chrysalisp_key)
    (cond
        ((= chrysalisp_key +key_left) 0xAC)
        ((= chrysalisp_key +key_up) 0xAD)
        ((= chrysalisp_key +key_right) 0xAE)
        ((= chrysalisp_key +key_down) 0xAF)
        ; ... more mappings
        (:t chrysalisp_key)))
```

#### Phase 4: Game Loop Integration (1 hour)

Call `doom_loop_step()` at 35 FPS:

```lisp
(defmethod :event (msg)
    (when (= (getf msg +ev_msg_type) +ev_type_timer)
        (. doom_wasm :call "doom_loop_step")
        (. this :dirty)
        (mail-timeout 28 (task-mailbox))))  ; ~35 FPS
```

### Estimated Total Implementation Time

- Phase 1: 2-3 hours (import function infrastructure)
- Phase 2: 2-3 hours (frame buffer rendering)
- Phase 3: 1-2 hours (input handling)
- Phase 4: 1 hour (game loop)

**Total: 6-9 hours** for a developer familiar with the codebase

### Alternative: Quick Proof-of-Concept

For a faster demonstration, we could:

1. Hardcode doom.wasm imports in wasm_wasm3.cpp
2. Render directly to SDL (bypass ChrysaLisp GUI)
3. Create standalone doom_test.cpp

This would prove DOOM runs but wouldn't integrate with ChrysaLisp GUI system.

### Why This Wasn't Done Yet

Our initial WASM integration focused on:
- ✅ Loading WASM modules
- ✅ Calling WASM exports
- ✅ Memory access

DOOM is the **first module we've encountered that needs imports**. This is a natural extension of the existing work.

### Key Insight

Once we add import function support:
- ✅ DOOM will work
- ✅ Many other WASM applications will work
- ✅ We can bridge WASM ↔ ChrysaLisp (bidirectional calling)

This is a **significant capability upgrade** beyond what we have now.

## How to Test (Once Implemented)

```lisp
(import "class/wasm/lisp.inc")

; Load DOOM
(defq doom (Wasm "examples/wasm/doom.wasm"))

; Initialize game
(. doom :call "main")
(print "DOOM initialized!")

; Run one frame
(. doom :call "doom_loop_step")
(print "First frame rendered!")

; Cleanup
(. doom :close)
```

## Files

- ✅ `examples/wasm/doom.wasm` - The binary (downloaded)
- ✅ `docs/DOOM_WASM_INTEGRATION.md` - Full technical spec
- ✅ `examples/wasm/DOOM_STATUS.md` - This file
- ⏳ `src/host/wasm_wasm3.cpp` - Needs import function support
- ⏳ `gui/doom/lisp.inc` - DoomView class (future)
- ⏳ `apps/doom/app.lisp` - DOOM app (future)

## Conclusion

We have **everything needed to run DOOM** except the import function infrastructure in our WASM integration. This is a well-defined, achievable enhancement that would unlock not just DOOM, but many other WASM applications.

**Bottom line**: DOOM is **ready to run** as soon as we implement wasm3 import function support.

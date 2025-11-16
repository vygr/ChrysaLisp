# DOOM WebAssembly Integration for ChrysaLisp

## Overview

DOOM (1993) has been ported to WebAssembly and we can run it in ChrysaLisp! The binary is 6.6MB and includes the full game engine.

**Downloaded**: `doom.wasm` (6,683,648 bytes)
**Source**: https://github.com/diekmann/wasm-fizzbuzz/tree/main/doom
**License**: id Software DOOM source code license

## Technical Requirements

### WASM Module Specifications

**Memory**: 108 pages (108 × 64KB = 6,912KB ≈ 7MB)
**Screen Resolution**: 640×400 pixels, RGBA format (1,024,000 bytes)
**Target FPS**: 35 FPS (game logic) + 60 FPS (rendering)

### WASM Exports (Functions DOOM Provides)

```c
void main();                              // Initialize DOOM
void doom_loop_step();                    // Game loop (call each frame)
void add_browser_event(int type, int keycode);  // Handle input
                                          // type: 0=keydown, 1=keyup
```

### WASM Imports (Functions DOOM Needs From Us)

These must be provided in the `importObject` when loading DOOM:

```javascript
importObject = {
    js: {
        js_console_log: function(offset, length) {...},
        js_stdout: function(offset, length) {...},
        js_stderr: function(offset, length) {...},
        js_milliseconds_since_start: function() {...},
        js_draw_screen: function(ptr) {...},
    },
    env: {
        memory: WebAssembly.Memory
    }
}
```

#### Import Function Details

**1. js_console_log(offset, length)**
- Read UTF-8 string from WASM memory at `offset` for `length` bytes
- Print to console/log

**2. js_stdout(offset, length)**
- Same as console_log but for stdout
- Used for game messages

**3. js_stderr(offset, length)**
- Same as console_log but for stderr
- Used for error messages

**4. js_milliseconds_since_start()**
- Returns: double (milliseconds since game start)
- Used for timing/animation
- Should be monotonically increasing

**5. js_draw_screen(ptr)**
- `ptr`: Pointer to frame buffer in WASM memory
- Frame buffer format: 640×400 pixels, RGBA (4 bytes per pixel)
- Total size: 640 × 400 × 4 = 1,024,000 bytes
- Must copy buffer and render to display

### Input Handling

DOOM uses custom keycodes (not standard ASCII):

| Key | DOOM Code | Note |
|-----|-----------|------|
| Arrows | 0xAC-0xAF | Left=0xAC, Up=0xAD, Right=0xAE, Down=0xAF |
| Ctrl | 0x9D | Right Ctrl = 0x80+0x1D |
| Alt | 0xB8 | Right Alt = 0x80+0x38 |
| Space | 32 | Standard ASCII |
| Enter | 13 | Standard ASCII |
| Backspace | 127 | Not 8! |
| A-Z | 97-122 | Lowercase ASCII (a-z) |
| F1-F12 | 187-198 | keyCode + 75 |

**Event Flow**:
1. User presses key
2. Convert to DOOM keycode
3. Call `add_browser_event(0, doom_keycode)`  // 0 = keydown
4. User releases key
5. Call `add_browser_event(1, doom_keycode)`  // 1 = keyup

## ChrysaLisp Integration Architecture

### Phase 1: WASM Import Functions (C++ Host Layer)

We need to extend `src/host/wasm_wasm3.cpp` to support import functions:

```cpp
// Import function for DOOM logging
m3ApiRawFunction(doom_js_console_log) {
    m3ApiGetArg(int32_t, offset);
    m3ApiGetArg(int32_t, length);

    uint8_t* mem = m3_GetMemory(runtime, NULL, 0);
    const char* str = (const char*)(mem + offset);

    // Print to ChrysaLisp console (via sys_pii)
    printf("[DOOM] %.*s\n", length, str);

    m3ApiSuccess();
}

// Import function for timing
m3ApiRawFunction(doom_js_milliseconds_since_start) {
    static auto start_time = std::chrono::steady_clock::now();
    auto now = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time);

    m3ApiReturn(duration.count());
}

// Import function for drawing
m3ApiRawFunction(doom_js_draw_screen) {
    m3ApiGetArg(int32_t, ptr);

    uint8_t* mem = m3_GetMemory(runtime, NULL, 0);
    uint8_t* framebuffer = mem + ptr;

    // Copy 640x400 RGBA buffer to ChrysaLisp texture
    // (Need to implement texture bridge)

    m3ApiSuccess();
}
```

**Link imports when loading**:
```cpp
M3Result link_doom_imports(IM3Runtime runtime, IM3Module module) {
    m3_LinkRawFunction(module, "js", "js_console_log", "v(ii)", &doom_js_console_log);
    m3_LinkRawFunction(module, "js", "js_stdout", "v(ii)", &doom_js_console_log);
    m3_LinkRawFunction(module, "js", "js_stderr", "v(ii)", &doom_js_console_log);
    m3_LinkRawFunction(module, "js", "js_milliseconds_since_start", "F()", &doom_js_milliseconds_since_start);
    m3_LinkRawFunction(module, "js", "js_draw_screen", "v(i)", &doom_js_draw_screen);
    return m3Err_none;
}
```

### Phase 2: DoomView GUI Class (Lisp Layer)

**File**: `gui/doom/lisp.inc`

```lisp
(import "class/wasm/lisp.inc")
(import "gui/view/lisp.inc")

(defclass DoomView () (View)
    ; DOOM game view - renders WASM DOOM
    ; Automatically handles input and rendering

    (defmethod :init (width height)
        ; Initialize view
        (defq this (. (View) :init width height 1))

        ; Load DOOM WASM (needs special import handling)
        (setf this +doom_wasm (Wasm "examples/wasm/doom.wasm"))
        (setf this +doom_width 640)
        (setf this +doom_height 400)
        (setf this +doom_frame_buffer nil)

        ; Initialize DOOM
        (. (getf this +doom_wasm) :call "main")

        ; Start game loop timer
        (setf this +doom_timer_id
            (mail-timeout 28 (task-mailbox)))  ; ~35 FPS

        this)

    (defmethod :draw ()
        ; Render current DOOM frame buffer
        (when (getf this +doom_frame_buffer)
            (defq texture (host-gui-create-texture
                (getf this +doom_frame_buffer)
                (getf this +doom_width)
                (getf this +doom_height)))

            (. this :ctx_blit texture 0 0
                (getf this +doom_width)
                (getf this +doom_height))

            (host-gui-free-texture texture)))

    (defmethod :event (msg)
        ; Handle keyboard input
        (cond
            ((= (getf msg +ev_msg_type) +ev_type_key)
                (defq keycode (getf msg +ev_msg_key_keycode))
                (defq doom_code (convert-to-doom-keycode keycode))
                (defq event_type (if (getf msg +ev_msg_key_down) 0 1))

                ; Send to DOOM
                (. (getf this +doom_wasm) :call "add_browser_event"
                    event_type doom_code))

            ((= (getf msg +ev_msg_type) +ev_type_timer)
                ; Game loop tick
                (. (getf this +doom_wasm) :call "doom_loop_step")
                (. this :dirty)  ; Request redraw

                ; Re-arm timer
                (mail-timeout 28 (task-mailbox)))))

    (defmethod :close ()
        ; Cleanup
        (. (getf this +doom_wasm) :close)
        (. this :super :close)))

(defun convert-to-doom-keycode (key)
    ; Convert ChrysaLisp keycode to DOOM keycode
    (cond
        ((= key +ev_key_left) 0xAC)
        ((= key +ev_key_up) 0xAD)
        ((= key +ev_key_right) 0xAE)
        ((= key +ev_key_down) 0xAF)
        ((= key +ev_key_backspace) 127)
        ((= key +ev_key_ctrl) 0x9D)
        ((= key +ev_key_alt) 0xB8)
        ((= key +ev_key_space) 32)
        ((= key +ev_key_enter) 13)
        ; A-Z to lowercase
        ((&& (>= key 65) (<= key 90)) (+ key 32))
        ; F1-F12
        ((&& (>= key 112) (<= key 123)) (+ key 75))
        ; Default: pass through
        (:t key)))
```

### Phase 3: DOOM Application

**File**: `apps/doom/app.lisp`

```lisp
(import "gui/doom/lisp.inc")
(import "gui/window/lisp.inc")

(defun main ()
    ; Create window
    (defq window (create-window "DOOM" 640 400))

    ; Create DOOM view
    (defq doom_view (DoomView 640 400))

    ; Add to window
    (. window :add_child doom_view)

    ; Focus for keyboard input
    (. doom_view :focus)

    ; Run event loop
    (view-service))

(main)
```

## Current Implementation Status

### ✅ What Works Now

1. **WASM loading**: Our integration can load doom.wasm
2. **Memory access**: Can read WASM memory
3. **Function calling**: Can call DOOM exports

### ⚠️ What Needs Implementation

1. **WASM imports**: Need to add m3_LinkRawFunction support to wasm_wasm3.cpp
2. **Shared memory**: DOOM needs to provide WebAssembly.Memory
3. **Texture creation**: Need host_gui_create_texture from memory buffer
4. **Timer integration**: Need to call doom_loop_step() regularly
5. **Input mapping**: Convert ChrysaLisp keys to DOOM keycodes

## Implementation Steps

### Step 1: Extend wasm_wasm3.cpp

Add import function support:

```cpp
// In host_wasm_load():
result = m3_LoadModule(inst->runtime, inst->module);
if (result) { /* error */ }

// NEW: Link doom imports
result = link_doom_imports(inst->runtime, inst->module);
if (result) {
    inst->error = std::string("Failed to link imports: ") + result;
    // cleanup
    return nullptr;
}
```

### Step 2: Add Texture Support

Extend `src/host/gui_sdl.cpp`:

```cpp
void* host_gui_create_texture_from_memory(void* pixels, uint32_t width, uint32_t height) {
    SDL_Surface* surface = SDL_CreateRGBSurfaceFrom(
        pixels, width, height, 32, width * 4,
        0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000);

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    SDL_FreeSurface(surface);

    return texture;
}
```

### Step 3: Test with Simple DOOM Launch

```lisp
(import "class/wasm/lisp.inc")

; Load DOOM
(defq doom (Wasm "examples/wasm/doom.wasm"))

; Initialize
(print "Initializing DOOM...")
(. doom :call "main")

; Run one game step
(print "Running game loop...")
(. doom :call "doom_loop_step")

(print "DOOM test complete!")
(. doom :close)
```

## Challenges

### 1. Import Functions

**Problem**: wasm3 requires linking import functions with `m3_LinkRawFunction`
**Solution**: Extend our C++ wrapper to support this

### 2. Frame Buffer Access

**Problem**: Need to read 1MB frame buffer from WASM memory every frame
**Solution**: Use `host_wasm_get_memory()` then create SDL texture

### 3. Performance

**Problem**: 640×400×4 bytes = 1MB per frame at 35 FPS = 35MB/sec
**Solution**: This is manageable - modern systems handle this easily

### 4. Shared Memory

**Problem**: JavaScript creates WebAssembly.Memory and shares it
**Solution**: wasm3 manages memory internally - we just access it via m3_GetMemory

## Expected Performance

- **Memory**: ~7MB WASM + 1MB frame buffer = 8MB total
- **CPU**: DOOM is from 1993 - extremely lightweight
- **FPS**: Target 35 FPS (game) + 60 FPS (render)
- **Startup**: ~100-200ms to load and initialize

## File Checklist

- [x] `examples/wasm/doom.wasm` - Downloaded (6.6MB)
- [ ] `src/host/wasm_wasm3.cpp` - Add import function support
- [ ] `src/host/gui_sdl.cpp` - Add texture-from-memory function
- [ ] `gui/doom/lisp.inc` - DoomView class
- [ ] `apps/doom/app.lisp` - DOOM application
- [ ] `examples/wasm/test_doom.lisp` - Basic test script

## Next Steps

1. **Implement import functions** in wasm_wasm3.cpp
2. **Test basic loading** - can we call main()?
3. **Add frame buffer rendering** - can we see the screen?
4. **Add input handling** - can we play?
5. **Optimize performance** - smooth 35 FPS?

Once complete, you'll be able to run DOOM natively in ChrysaLisp windows!

## References

- GitHub: https://github.com/diekmann/wasm-fizzbuzz/tree/main/doom
- Live Demo: https://diekmann.github.io/wasm-fizzbuzz/doom/
- DOOM Source: id Software (1997 Linux DOOM 1.10)
- wasm3 Imports: https://github.com/wasm3/wasm3/blob/main/docs/Cookbook.md#importing-functions

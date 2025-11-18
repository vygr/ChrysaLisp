# MAME Port to ChrysaLisp - Architecture Document

## Overview

This document describes the architecture for porting MAME (Multiple Arcade Machine Emulator) to run as a native ChrysaLisp application.

## Goals

1. Run MAME's emulation cores within the ChrysaLisp environment
2. Use ChrysaLisp's Platform Interface Implementation (PII) for all host OS interactions
3. Integrate with ChrysaLisp's GUI service for graphics output
4. Utilize ChrysaLisp's audio service for sound
5. Handle input through ChrysaLisp's event system

## Architecture Layers

```
┌─────────────────────────────────────────────┐
│     MAME Application (Lisp launcher)        │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│         MAME Core Emulation Engine          │
│         (C++ - original MAME code)          │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│      ChrysaLisp PII Adapter Layer           │
│  (Maps MAME APIs → ChrysaLisp PII/Services) │
└─────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────┐
│        ChrysaLisp Host Interface            │
│   (host_os_funcs, host_gui_funcs, etc.)     │
└─────────────────────────────────────────────┘
```

## Key Subsystems

### 1. File I/O Adapter (`mame_file_io.h/cpp`)

Maps MAME's file operations to ChrysaLisp PII:
- `osd_open()` → `pii_open()`
- `osd_read()` → `pii_read()`
- `osd_write()` → `pii_write()`
- `osd_close()` → `pii_close()`
- `osd_seek()` → `pii_seek()`

### 2. Graphics/Video Adapter (`mame_video.h/cpp`)

Maps MAME's video output to ChrysaLisp GUI:
- MAME renders to a pixel buffer
- Adapter creates ChrysaLisp texture from buffer
- Uses `host_gui_blit()` to display
- Handles screen scaling and aspect ratios

### 3. Audio Adapter (`mame_audio.h/cpp`)

Maps MAME's audio output to ChrysaLisp audio service:
- MAME generates audio samples
- Adapter converts to ChrysaLisp audio format
- Uses `host_audio_play_sfx()` or streaming

### 4. Input Adapter (`mame_input.h/cpp`)

Maps ChrysaLisp GUI events to MAME input:
- Polls ChrysaLisp event system
- Translates keyboard/joystick events
- Maps to arcade controls (coins, start, joystick, buttons)

### 5. Threading/Timing Adapter (`mame_thread.h/cpp`)

Maps MAME's threading to ChrysaLisp tasks:
- MAME threads → ChrysaLisp tasks
- MAME mutexes → ChrysaLisp synchronization primitives
- Timing via `pii_gettime()`

### 6. Memory Management Adapter (`mame_memory.h/cpp`)

Maps MAME's memory allocation:
- Uses `pii_mmap()` for large allocations
- Standard malloc/free for small allocations
- Memory-mapped ROM loading

## Build Strategy

### Phase 1: Minimal Core (Current Focus)
- Create adapter layer interfaces
- Build minimal MAME core with one simple driver
- Prove the concept with a basic game (e.g., Pac-Man)

### Phase 2: Expand Drivers
- Add more arcade game drivers
- Improve video/audio quality
- Optimize performance

### Phase 3: Full Integration
- Complete GUI with game selection
- Save state functionality
- Configuration management

## Technical Challenges

### Challenge 1: SDL Dependency
**MAME uses SDL2 extensively. ChrysaLisp also uses SDL2 (default GUI backend).**

**Solution:** Since both use SDL2, we can:
- Use ChrysaLisp's existing SDL initialization
- Share the SDL context between ChrysaLisp GUI and MAME
- Alternative: Create abstraction layer that maps SDL calls to ChrysaLisp equivalents

### Challenge 2: Threading Model
**MAME uses pthread/std::thread. ChrysaLisp uses cooperative multitasking.**

**Solution:**
- Map MAME threads to ChrysaLisp tasks
- Implement mutex/semaphore primitives using ChrysaLisp mailbox system
- May need to modify MAME for cooperative yielding

### Challenge 3: Build System
**MAME uses custom GENie/Makefile build system.**

**Solution:**
- Create custom Makefile that integrates with ChrysaLisp build
- Compile MAME as a static library
- Link with ChrysaLisp adapter layer
- Create final boot_image with embedded MAME

### Challenge 4: Size
**MAME is huge (100+ MB compiled). ChrysaLisp boot images are ~170KB.**

**Solution:**
- Don't embed MAME in boot_image
- Load MAME as external binary via JIT
- Or use dynamic linking approach
- Focus on minimal driver set to reduce size

## Directory Structure

```
apps/mame/
├── docs/
│   ├── ARCHITECTURE.md (this file)
│   ├── BUILD.md
│   └── PORTING_NOTES.md
├── src/
│   ├── adapters/
│   │   ├── mame_file_io.cpp
│   │   ├── mame_video.cpp
│   │   ├── mame_audio.cpp
│   │   ├── mame_input.cpp
│   │   ├── mame_thread.cpp
│   │   └── mame_memory.cpp
│   ├── osd/              (OSD = OS Dependent layer)
│   │   └── chrysalisp/   (ChrysaLisp-specific OSD)
│   └── main.cpp          (MAME entry point)
├── include/
│   └── mame_pii_adapter.h
├── app.lisp              (Lisp launcher)
├── widgets.inc           (UI components)
└── Makefile
```

## Integration with ChrysaLisp Build System

The MAME port will:
1. Compile MAME C++ code to native object files
2. Create MAME library linked against ChrysaLisp PII
3. JIT load MAME when app starts (similar to Chess app using native code)
4. Lisp launcher handles UI and ROM selection

## Memory Layout

```
┌─────────────────────────┐
│ ChrysaLisp Boot Image   │
│ (~170KB)                │
├─────────────────────────┤
│ MAME Launcher App       │
│ (Lisp code)             │
├─────────────────────────┤
│ MAME Core Library       │
│ (JIT loaded, ~20-50MB)  │
├─────────────────────────┤
│ ROM Images              │
│ (loaded as needed)      │
├─────────────────────────┤
│ Emulated Machine RAM    │
│ (varies by machine)     │
└─────────────────────────┘
```

## Performance Considerations

1. **Native Code:** MAME core runs as native compiled code, not VP
2. **Zero-Copy Graphics:** Direct pixel buffer access where possible
3. **Audio Buffering:** Double-buffer audio to prevent dropouts
4. **Frame Skipping:** Allow frame skip to maintain speed on slower systems

## Next Steps

1. ✅ Create directory structure
2. ✅ Write architecture documentation
3. 🔄 Implement minimal PII adapter layer
4. ⏳ Create proof-of-concept with simple MAME driver
5. ⏳ Build complete launcher UI
6. ⏳ Optimize and expand

## References

- MAME: https://github.com/mamedev/mame
- ChrysaLisp Porting Guide: `docs/ai_digest/porting.md`
- ChrysaLisp Host Interface: `docs/ai_digest/host_interface.md`

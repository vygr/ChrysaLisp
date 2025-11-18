# MAME Porting Notes for Developers

This document contains technical notes for developers working on the MAME port.

## Adapter Layer Design Decisions

### Why Not Use SDL Directly?

While MAME typically uses SDL2 and ChrysaLisp also uses SDL2 (by default), we've created an adapter layer instead of sharing SDL directly. Reasons:

1. **Portability** - ChrysaLisp can use different GUI backends (SDL, framebuffer, raw)
2. **Abstraction** - Keeps MAME isolated from ChrysaLisp implementation details
3. **Control** - Allows ChrysaLisp to manage window/event loop lifecycle
4. **Future-proofing** - Easier to support non-SDL backends

### PII Function Table Layout

The adapter assumes this layout for `host_os_funcs` (from `src/host/pii_*.cpp`):

```c
Index | Function
------|----------
0     | pii_exit
1     | pii_fstat
2     | pii_open
3     | pii_close
4     | pii_unlink
5     | pii_read
6     | pii_write
7     | pii_gettime
8     | pii_mmap
9     | pii_munmap
10    | pii_mprotect
11    | pii_dirlist
12    | pii_remove
13    | pii_seek
14    | pii_sleep
15    | pii_random
16    | pii_open_shared
17    | pii_close_shared
18    | pii_flush_icache
```

**IMPORTANT:** These indices must match the actual PII implementation! Verify against your ChrysaLisp version.

### Memory Management Strategy

The adapter uses ChrysaLisp's `pii_mmap` for all allocations because:

1. Consistent with ChrysaLisp memory model
2. Allows large ROM files to be memory-mapped efficiently
3. Better integration with ChrysaLisp's memory tracking

For small allocations, we still use `mmap` rather than `malloc` to avoid mixing allocation strategies.

## Integration with MAME OSD Layer

MAME's OSD (Operating System Dependent) layer is the interface point. To integrate:

### 1. Create ChrysaLisp OSD Module

In MAME source, create `src/osd/chrysalisp/`:

```
src/osd/chrysalisp/
├── chrysalispmain.cpp    - Entry point
├── chrysalispvideo.cpp   - Video using our adapter
├── chrysalispaudio.cpp   - Audio using our adapter
├── chrysalispinput.cpp   - Input using our adapter
├── chrysalispfile.cpp    - File I/O using our adapter
├── chrysalispmisc.cpp    - Misc functions
└── chrysalisp.mak        - Build configuration
```

### 2. Replace OSD Calls

Map MAME OSD calls to adapter functions:

```cpp
// In chrysalispfile.cpp
osd_file::error osd_file::open(std::string const &path,
                                uint32_t openflags,
                                ptr &file,
                                uint64_t &filesize)
{
    mame_file_handle_t* handle = mame_file_open(path.c_str(), openflags);
    if (!handle)
        return error::ACCESS_DENIED;

    // Wrap in OSD file object
    // ...
}
```

### 3. Video Frame Delivery

MAME renders to a bitmap. Our adapter needs the pixels:

```cpp
void chrysalisp_video_update(bitmap_rgb32 &bitmap)
{
    mame_video_info_t* info = mame_video_get_info();

    // Copy MAME's bitmap to our framebuffer
    for (int y = 0; y < bitmap.height(); y++) {
        uint32_t* src = &bitmap.pix(y);
        uint32_t* dst = info->framebuffer + y * info->width;
        memcpy(dst, src, bitmap.width() * 4);
    }

    // Display
    mame_video_update();
}
```

### 4. Input Polling

MAME polls input every frame:

```cpp
void chrysalisp_input_poll()
{
    mame_input_event_t event;
    while (mame_input_poll(&event)) {
        // Map to MAME input system
        switch (event.type) {
            case MAME_INPUT_KEYBOARD:
                input_update_keyboard(event.code, event.pressed);
                break;
            // ...
        }
    }
}
```

## Threading Challenges

### Problem

MAME uses threads heavily:
- Separate thread for audio
- Parallel device emulation
- Background asset loading

ChrysaLisp uses cooperative multitasking (tasks, not preemptive threads).

### Solution Options

**Option 1: Run MAME single-threaded**
- Set `MAME_BUILD_THREADS=0` in build
- Simplest but may reduce performance
- Good for initial port

**Option 2: Map threads to ChrysaLisp tasks**
- Implement pthread API in terms of ChrysaLisp tasks
- Need to add yield points in MAME code
- More work but better performance

**Option 3: Hybrid approach**
- Use real threads for MAME core
- Communicate with ChrysaLisp via mailboxes
- Requires careful synchronization

Current adapter implements stub thread functions. Need to choose an option for full port.

## Audio Streaming Challenge

### Problem

ChrysaLisp's current audio system (`host_audio_funcs`) is designed for sound effects:
- Load discrete sound files
- Play/stop individual sounds
- Not designed for continuous streaming

MAME generates audio samples continuously at 48kHz (or other rates).

### Current Solution

The adapter buffers samples but doesn't actually play them yet.

### Proposed Solutions

**Option 1: Extend ChrysaLisp audio PII**

Add new functions to `host_audio_funcs`:
```c
// New functions needed:
pii_audio_stream_open(sample_rate, channels)
pii_audio_stream_write(samples, count)
pii_audio_stream_close()
```

Implement in `src/host/audio_sdl.cpp` using SDL_mixer's music API or raw audio callback.

**Option 2: Write to temp WAV file**

- Write samples to temporary WAV file
- Play using existing audio system
- Update file continuously
- Inefficient but works with current PII

**Option 3: Direct SDL access**

- Access SDL audio directly (bypassing PII)
- Breaks abstraction but enables streaming
- Only works with SDL backend

**Recommendation:** Option 1 is best long-term solution.

## ROM File Loading

### MAME ROM Format

MAME ROMs are typically:
- ZIP files containing multiple ROM images
- Individual binary files in specific directory structure
- Sometimes require CHD (Compressed Hunks of Data) files

### Loading Strategy

```cpp
// Use mame_file_open to read ROM
mame_file_handle_t* rom = mame_file_open("roms/pacman.zip", 0);

// For ZIP files, need ZIP parsing
// Could use libzip or implement minimal ZIP reader

// For uncompressed ROMs, use mmap for efficiency
void* rom_data = mame_mem_map_file("roms/pacman/rom1.bin", &size);
```

## Performance Optimization

### Critical Paths

1. **Video blitting** - Called 60+ times per second
   - Use zero-copy where possible
   - Consider SIMD for format conversion
   - Bypass SDL entirely and write to framebuffer directly

2. **Audio mixing** - Called ~800 times per second (at 48kHz)
   - Pre-allocate buffers
   - Minimize memory copies

3. **Input polling** - Called every frame
   - Keep event queue small
   - Process events in batch

### Profiling Points

Add timing around:
```cpp
uint64_t start = mame_time_usec();
// ... critical section ...
uint64_t elapsed = mame_time_usec() - start;
if (elapsed > 16666) // 16.6ms = 60fps
    mame_log(MAME_LOG_WARNING, "Slow frame: %llu us", elapsed);
```

## Testing Strategy

### Unit Tests

Each adapter module should have tests:
- `test_file_io()` - Verify file operations
- `test_memory()` - Verify allocation/deallocation
- `test_video()` - Verify framebuffer operations
- etc.

### Integration Tests

1. **Minimal ROM** - Create a fake ROM that just fills screen
2. **Simple Game** - Test with Pac-Man or similar
3. **Complex Game** - Test with 3D or vector game
4. **Stress Test** - Run for hours, check for leaks

### Debugging

```cpp
// Enable detailed logging
mame_log_set_level(MAME_LOG_DEBUG);

// Add assertions
assert(handle != nullptr);

// Use ChrysaLisp debug builds
// Set *debug_mode* to 1 or 2 in ChrysaLisp
```

## Known Issues and TODOs

### High Priority

- [ ] Implement audio streaming (see Audio Streaming Challenge above)
- [ ] Complete directory enumeration (`mame_dir_enum`)
- [ ] Implement threading support (see Threading Challenges above)
- [ ] Test on all platforms (ARM64, RISC-V, etc.)

### Medium Priority

- [ ] Optimize video blitting (zero-copy)
- [ ] Add save state support
- [ ] Implement file memory-mapping (`mame_mem_map_file`)
- [ ] Add configuration file support
- [ ] Create ROM verification system

### Low Priority

- [ ] Add network play (use ChrysaLisp distributed system!)
- [ ] Create debug UI (show CPU state, memory, etc.)
- [ ] Add cheat system
- [ ] Support for artwork/bezels
- [ ] Recording/playback of gameplay

## Building Minimal MAME

To reduce initial complexity, build MAME with minimal drivers:

```bash
# Just Pac-Man
make SUBTARGET=tiny SOURCES=src/mame/drivers/pacman.cpp

# Popular classics
make SUBTARGET=classic SOURCES=src/mame/drivers/pacman.cpp,\
src/mame/drivers/galaxian.cpp,\
src/mame/drivers/asteroid.cpp,\
src/mame/drivers/donkeyko.cpp
```

This creates much smaller binary and faster compilation.

## References

### MAME Documentation

- OSD Layer: `docs/osd.txt` in MAME source
- Build System: `docs/build.md`
- Source Structure: `docs/source.md`

### ChrysaLisp References

- Porting Guide: `docs/ai_digest/porting.md`
- Host Interface: `docs/ai_digest/host_interface.md`
- PII Implementation: `src/host/pii_*.cpp`

### External Resources

- MAME Development: https://wiki.mamedev.org/
- SDL2 Audio: https://wiki.libsdl.org/CategoryAudio
- ZIP File Format: https://en.wikipedia.org/wiki/ZIP_(file_format)

## Getting Help

- MAME Development: MAMEWorld Forums, #mamedev on IRC
- ChrysaLisp: GitHub issues, #ChrysaLisp-OS on Matrix
- This Port: File issue in ChrysaLisp repository with `[MAME]` tag

## Summary

The foundation is in place. Next steps:

1. Choose threading model
2. Implement audio streaming
3. Create MAME OSD module
4. Build minimal MAME with one driver
5. Test end-to-end
6. Iterate and expand

Good luck and happy porting! 🎮

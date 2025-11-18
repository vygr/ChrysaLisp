# MAME Port - TODO List

This document tracks remaining work for the MAME port to ChrysaLisp.

## Legend
- 🔴 **High Priority** - Blocking issues that prevent basic functionality
- 🟡 **Medium Priority** - Important features for usability
- 🟢 **Low Priority** - Nice-to-have enhancements
- ✅ **Done** - Completed items

---

## Phase 1: Core Integration (High Priority)

### 🔴 1.1 Audio Streaming Implementation
**Status:** In Progress
**Blockers:** ChrysaLisp PII needs audio streaming support

**Tasks:**
- [ ] Extend ChrysaLisp `host_audio_funcs` with streaming API
  - [ ] Add `pii_audio_stream_open(sample_rate, channels)`
  - [ ] Add `pii_audio_stream_write(samples, count)`
  - [ ] Add `pii_audio_stream_close()`
- [ ] Implement in `src/host/audio_sdl.cpp` using SDL audio callback
- [ ] Update `mame_audio.cpp` to use streaming API
- [ ] Test with continuous audio generation

**Files to modify:**
- `src/host/audio_sdl.cpp`
- `src/host/pii.h`
- `apps/mame/src/adapters/mame_audio.cpp`

**Estimated effort:** 8-16 hours

---

### 🔴 1.2 Directory Enumeration
**Status:** Not started
**Blockers:** None

**Tasks:**
- [ ] Implement `mame_dir_enum()` in `mame_file_io.cpp`
- [ ] Parse ChrysaLisp `pii_dirlist` output format
- [ ] Handle recursive directory scanning
- [ ] Add filtering by extension (e.g., *.zip)

**Files to modify:**
- `apps/mame/src/adapters/mame_file_io.cpp`

**Estimated effort:** 2-4 hours

---

### 🔴 1.3 File Memory Mapping
**Status:** Not started
**Blockers:** None

**Tasks:**
- [ ] Implement `mame_mem_map_file()` for ROM loading
- [ ] Use `pii_open` + `pii_fstat` + `pii_mmap`
- [ ] Handle file-backed mmap for large ROM files
- [ ] Implement `mame_mem_unmap_file()`

**Files to modify:**
- `apps/mame/src/adapters/mame_adapter_core.cpp`

**Estimated effort:** 2-4 hours

---

### 🔴 1.4 MAME Core Integration
**Status:** Not started
**Blockers:** Need to choose threading model

**Tasks:**
- [ ] Clone MAME source code
- [ ] Create ChrysaLisp OSD module in MAME source
  - [ ] Create `src/osd/chrysalisp/` directory
  - [ ] Implement `chrysalispmain.cpp` (entry point)
  - [ ] Implement `chrysalispfile.cpp` (file I/O)
  - [ ] Implement `chrysalispvideo.cpp` (video output)
  - [ ] Implement `chrysalispaudio.cpp` (audio output)
  - [ ] Implement `chrysalispinput.cpp` (input handling)
  - [ ] Create `chrysalisp.mak` (build config)
- [ ] Decide on threading approach (see Threading section below)
- [ ] Build minimal MAME with single driver (e.g., Pac-Man)
- [ ] Link with adapter library
- [ ] Test end-to-end

**Files to create:**
- `apps/mame/mame-src/src/osd/chrysalisp/*.cpp`
- `apps/mame/mame-src/scripts/target/chrysalisp/chrysalisp.lua`

**Estimated effort:** 40-80 hours

---

### 🔴 1.5 Threading Model Decision
**Status:** Not started
**Priority:** Must decide before MAME integration

**Options:**

**Option A: Single-threaded MAME**
- Pros: Simplest, no threading complexity
- Cons: May reduce performance
- Implementation: Set `MAME_BUILD_THREADS=0`
- Effort: 0 hours (just a build flag)

**Option B: Map threads to ChrysaLisp tasks**
- Pros: Better performance, uses ChrysaLisp model
- Cons: Need to add yield points in MAME
- Implementation: Implement pthread API wrapper
- Effort: 20-40 hours

**Option C: Hybrid (real threads + mailboxes)**
- Pros: Best performance, minimal MAME changes
- Cons: Complex synchronization
- Implementation: Real threads, communicate via mailboxes
- Effort: 30-50 hours

**Recommendation:** Start with Option A, migrate to B or C if needed

**Tasks:**
- [ ] Benchmark single-threaded vs multi-threaded MAME
- [ ] Choose threading model
- [ ] Document decision
- [ ] Implement chosen approach

---

## Phase 2: Testing and Validation (High/Medium Priority)

### 🔴 2.1 ROM Loading and Verification
**Status:** Not started

**Tasks:**
- [ ] Implement ZIP file parsing (for ROM archives)
  - Option 1: Use libzip library
  - Option 2: Implement minimal ZIP reader
- [ ] ROM verification (CRC checks)
- [ ] Handle missing ROMs gracefully
- [ ] Support ROM search paths

**Estimated effort:** 8-16 hours

---

### 🟡 2.2 Platform Testing
**Status:** Not started

**Tasks:**
- [ ] Test on Linux x86_64
- [ ] Test on Linux ARM64 (Raspberry Pi)
- [ ] Test on macOS x86_64
- [ ] Test on macOS ARM64 (Apple Silicon)
- [ ] Test on Windows x86_64
- [ ] Test on RISC-V 64
- [ ] Document platform-specific issues

**Estimated effort:** 16-24 hours

---

### 🟡 2.3 Integration Tests
**Status:** Not started

**Tasks:**
- [ ] Test with Pac-Man ROM
- [ ] Test with Donkey Kong ROM
- [ ] Test with vector game (Asteroids)
- [ ] Test with 3D game (if supported)
- [ ] Long-running stability test (24+ hours)
- [ ] Memory leak detection

**Estimated effort:** 8-16 hours

---

## Phase 3: Features and Polish (Medium Priority)

### 🟡 3.1 Save State Support
**Status:** Not started

**Tasks:**
- [ ] Implement save state functionality
- [ ] Store states in ChrysaLisp filesystem
- [ ] Add UI for save/load in launcher
- [ ] Support quick save/load hotkeys
- [ ] Handle save state versioning

**Estimated effort:** 16-24 hours

---

### 🟡 3.2 Configuration System
**Status:** Not started

**Tasks:**
- [ ] Create configuration file format (.tre or custom)
- [ ] Per-game configuration support
- [ ] Video settings (resolution, aspect ratio, filters)
- [ ] Audio settings (volume, sample rate)
- [ ] Input mapping configuration
- [ ] Save/load configuration

**Estimated effort:** 12-20 hours

---

### 🟡 3.3 Enhanced Launcher UI
**Status:** Basic framework exists

**Tasks:**
- [ ] ROM metadata display (game name, year, manufacturer)
- [ ] Screenshot/marquee display
- [ ] Search/filter functionality
- [ ] Favorites system
- [ ] Recent games list
- [ ] Game information viewer

**Files to modify:**
- `apps/mame/app.lisp`

**Estimated effort:** 12-20 hours

---

### 🟡 3.4 Controller Configuration
**Status:** Not started

**Tasks:**
- [ ] Gamepad/joystick detection
- [ ] Button mapping UI
- [ ] Per-game controller profiles
- [ ] Support for multiple controllers
- [ ] Test with common USB controllers

**Estimated effort:** 16-24 hours

---

## Phase 4: Optimization (Medium/Low Priority)

### 🟡 4.1 Video Optimization
**Status:** Basic implementation exists

**Tasks:**
- [ ] Implement zero-copy blitting
- [ ] Add SIMD optimizations for pixel format conversion
- [ ] Direct framebuffer access (bypass SDL)
- [ ] GPU acceleration (if available)
- [ ] Measure and optimize frame time

**Estimated effort:** 16-32 hours

---

### 🟡 4.2 Performance Profiling
**Status:** Not started

**Tasks:**
- [ ] Add timing instrumentation
- [ ] Profile critical paths (video, audio, input)
- [ ] Identify bottlenecks
- [ ] Optimize hot code paths
- [ ] Document performance characteristics per platform

**Estimated effort:** 8-16 hours

---

### 🟢 4.3 Build Optimization
**Status:** Not started

**Tasks:**
- [ ] Enable LTO (Link Time Optimization)
- [ ] Profile-guided optimization (PGO)
- [ ] Strip unnecessary MAME drivers from build
- [ ] Reduce binary size

**Estimated effort:** 4-8 hours

---

## Phase 5: Advanced Features (Low Priority)

### 🟢 5.1 Network Play
**Status:** Not started

**Tasks:**
- [ ] Leverage ChrysaLisp distributed system
- [ ] Implement netplay synchronization
- [ ] Handle network latency
- [ ] Test multiplayer games
- [ ] Create matchmaking UI

**Estimated effort:** 40-80 hours

---

### 🟢 5.2 Debug UI
**Status:** Not started

**Tasks:**
- [ ] CPU state viewer
- [ ] Memory viewer/editor
- [ ] Breakpoint support
- [ ] Single-step execution
- [ ] Register dump

**Estimated effort:** 24-40 hours

---

### 🟢 5.3 Cheat System
**Status:** Not started

**Tasks:**
- [ ] Cheat code database
- [ ] Memory search functionality
- [ ] Cheat activation UI
- [ ] Save custom cheats

**Estimated effort:** 16-24 hours

---

### 🟢 5.4 Artwork and Bezels
**Status:** Not started

**Tasks:**
- [ ] Support for cabinet artwork
- [ ] Bezel/overlay rendering
- [ ] Artwork file format support
- [ ] Layout configuration

**Estimated effort:** 20-30 hours

---

### 🟢 5.5 Recording and Playback
**Status:** Not started

**Tasks:**
- [ ] Input recording
- [ ] Playback functionality
- [ ] Video recording (to file)
- [ ] Screenshot capture
- [ ] Sharing recordings

**Estimated effort:** 16-24 hours

---

## Known Issues

### Critical Issues
- [ ] Audio streaming not implemented (blocks sound)
- [ ] Threading model not chosen (blocks MAME integration)
- [ ] Directory enumeration incomplete (blocks ROM scanning)

### Non-Critical Issues
- [ ] Window title cannot be set from adapter layer
- [ ] GUI event field offsets are approximate (need verification)
- [ ] Test program requires manual testing (no automated tests yet)

---

## Documentation Needs

### High Priority
- [ ] Create ROM compatibility list
- [ ] Write user guide
- [ ] Document keyboard shortcuts
- [ ] Create troubleshooting guide

### Medium Priority
- [ ] Add code comments to adapter layer
- [ ] Create API reference
- [ ] Document threading model decision
- [ ] Performance benchmarks

### Low Priority
- [ ] Create video tutorials
- [ ] Write blog post about porting process
- [ ] Contribute findings back to MAME project

---

## Total Effort Estimate

**Phase 1 (Core):** 70-160 hours
**Phase 2 (Testing):** 32-56 hours
**Phase 3 (Features):** 56-88 hours
**Phase 4 (Optimization):** 28-56 hours
**Phase 5 (Advanced):** 116-206 hours

**Total:** 302-566 hours (~8-14 weeks full-time)

**Minimum Viable Product (MVP):**
- Phase 1 complete + basic Phase 2 testing
- **Estimate:** 100-200 hours (~2.5-5 weeks)

---

## Next Immediate Steps

**To get a working demo:**

1. **Audio Streaming** (Critical, 8-16h)
   - Extend ChrysaLisp PII
   - Implement in adapter

2. **Directory Enumeration** (Critical, 2-4h)
   - Complete ROM scanning

3. **File Memory Mapping** (Critical, 2-4h)
   - Enable efficient ROM loading

4. **Choose Threading Model** (Critical, 0-40h)
   - Decision + implementation

5. **Minimal MAME Integration** (Critical, 40-80h)
   - Single driver (Pac-Man)
   - Basic OSD layer

6. **End-to-End Testing** (Critical, 8-16h)
   - Load ROM, play game, verify functionality

**Total MVP Effort:** 60-160 hours

---

## Contributing

To work on these TODOs:

1. Choose a task from the list above
2. Create a branch: `git checkout -b feature/task-name`
3. Implement the task
4. Test thoroughly
5. Update this TODO.md (mark as ✅)
6. Submit pull request

For questions or discussion, see docs/PORTING_NOTES.md

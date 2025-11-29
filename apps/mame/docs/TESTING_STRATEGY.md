# MAME Port Testing Strategy

**Date:** 2025-11-18
**Status:** Ready for Phase 4

---

## Testing Approach

Instead of using copyrighted ROMs, we'll test the MAME port using:
1. **MAME's built-in test drivers** (no ROMs required)
2. **Public domain/freely distributable arcade ROMs**
3. **Homebrew test ROMs**

---

## Phase 4A: Built-In Test Machines

### What Are Test Drivers?

MAME includes special "skeleton" drivers designed for testing emulator functionality without requiring copyrighted game ROMs. These test:
- CPU core emulation
- Timer systems
- Video rendering
- Audio output
- Input handling

### Running Test Drivers

**Command format:**
```bash
./mamechrysalisp <test_driver_name>
```

**Common MAME test drivers:**
- `cpuexec` - CPU execution test
- `timertest` - Timer system test
- `vidtest` - Video rendering test
- `audiotest` - Audio system test

**Discovery command:**
```bash
# List all available drivers including test ones
./mamechrysalisp -listfull | grep -i test
```

### Expected Behavior

**Success indicators:**
- MAME starts without crashing
- Display window opens (via our PII GUI adapter)
- Console output shows emulation running
- No fatal errors in logs

**What we're validating:**
- ✅ MAME binary links and runs
- ✅ PII adapter layer functions correctly
- ✅ ChrysaLisp OSD integration works
- ✅ Basic emulation loop executes

---

## Phase 4B: Public Domain Arcade ROMs

### Gridlee (Fully Legal Test ROM)

**Why Gridlee?**
- Official arcade game from 1983
- ROM released for free distribution with MAME
- Perfect for testing full game emulation
- Tests all subsystems: CPU, video, audio, input

**Obtaining Gridlee:**
```bash
# Gridlee is distributed with MAME itself
# Location: mame-src/roms/gridlee.zip (if included)
# OR download from MAME's official ROM site
```

**Running Gridlee:**
```bash
./mamechrysalisp gridlee
```

**What this tests:**
- Full game emulation cycle
- Z80 CPU emulation
- Video rendering (320x240 resolution)
- Sound generation
- Joystick/button input
- Frame timing (60 FPS target)

### Robby Roto (Prototype ROM)

**About:**
- Unreleased prototype from Bally/Midway
- Released for free distribution
- Tests similar systems to Gridlee

**Running:**
```bash
./mamechrysalisp robby
```

---

## Phase 4C: Homebrew Test ROMs

### MAME Test Suite ROMs

Several developers have created test ROMs specifically for MAME:
- **cpucore** - CPU instruction tests
- **memtest** - Memory system validation
- **videotest** - Rendering verification

These are typically available in MAME development repositories.

---

## Testing Checklist

### Stage 1: Basic Launch
- [ ] MAME binary executes without crashing
- [ ] Command-line help works: `./mamechrysalisp -help`
- [ ] Driver listing works: `./mamechrysalisp -listfull`
- [ ] Version info displays: `./mamechrysalisp -version`

### Stage 2: Test Driver (No ROM)
- [ ] Test driver launches
- [ ] Display window opens
- [ ] PII GUI functions called
- [ ] Clean shutdown without crash

### Stage 3: Gridlee (Full Game)
- [ ] ROM loads successfully
- [ ] Attract mode displays
- [ ] Graphics render correctly
- [ ] Sound plays through audio streaming
- [ ] Input responds (coin, start, joystick)
- [ ] Game runs at ~60 FPS
- [ ] No memory leaks over 5 minutes

### Stage 4: Performance
- [ ] Frame rate stable at 60 FPS
- [ ] Audio sync maintained
- [ ] Input latency acceptable (<50ms)
- [ ] Memory usage stable
- [ ] CPU usage reasonable

---

## Testing Commands Reference

### Basic Testing
```bash
# Show version
./mamechrysalisp -version

# List all available machines
./mamechrysalisp -listfull

# List only games (exclude devices)
./mamechrysalisp -listfull | grep -v "^Name:"

# Show detailed info about a machine
./mamechrysalisp -listxml gridlee

# Verify ROM integrity (if ROM present)
./mamechrysalisp -verifyroms gridlee
```

### Running with Options
```bash
# Run in window mode
./mamechrysalisp gridlee -window

# Show FPS counter
./mamechrysalisp gridlee -showfps

# Enable verbose logging
./mamechrysalisp gridlee -verbose

# Specify ROM path
./mamechrysalisp gridlee -rompath ./roms
```

### Debugging
```bash
# Show OSD layer initialization
./mamechrysalisp gridlee -verbose 2>&1 | grep -i osd

# Monitor audio calls
./mamechrysalisp gridlee -verbose 2>&1 | grep -i audio

# Check video operations
./mamechrysalisp gridlee -verbose 2>&1 | grep -i video
```

---

## Expected First-Run Issues

### Common Problems and Solutions

**1. "ROM not found"**
- Solution: Use test driver instead, or obtain Gridlee ROM
- Command: `./mamechrysalisp -listfull` to see what's available without ROMs

**2. Display not appearing**
- Cause: PII GUI adapter issue
- Debug: Check if `pii_gui_open` is being called
- Verify: Add logging in `apps/mame/src/adapters/mame_video.cpp`

**3. Crash on startup**
- Cause: Missing PII function or incorrect implementation
- Debug: Run with `-verbose` to see last function called
- Check: PII adapter layer initialization sequence

**4. Audio not playing**
- Cause: Audio streaming not supported or SDL2 issue
- Check: Verify `host_audio_funcs` table populated
- Fallback: Audio adapter should handle missing streaming gracefully

**5. Input not responding**
- Cause: Event queue not being polled or mapping incorrect
- Debug: Log input events in `mame_input.cpp`
- Verify: ChrysaLisp GUI events being captured

**6. Performance issues**
- Cause: Debug build overhead or inefficient rendering
- Check: Build was Release mode (-O3 optimization)
- Profile: Use `time` command to measure CPU usage

---

## Success Criteria

### Minimum Viable Product (MVP)
✅ **MAME launches without crashing**
✅ **Test driver runs and displays output**
✅ **PII adapter layer functions without errors**
✅ **Clean shutdown when quit**

### Full Success
✅ **Gridlee loads and runs**
✅ **Graphics display correctly at 60 FPS**
✅ **Audio plays without stuttering**
✅ **Input controls work (coin, start, joystick)**
✅ **Stable operation for 5+ minutes**
✅ **No memory leaks or crashes**

---

## Logging and Diagnostics

### Enable Detailed Logging

Add to `apps/mame/src/osd/chrysalisp/chrysalisposd.cpp`:

```cpp
// In initialization
osd_printf_verbose("ChrysaLisp OSD: Initializing...\n");
osd_printf_verbose("PII Version: %d\n", /* get PII version */);
osd_printf_verbose("Video support: %s\n", pii_gui_open ? "YES" : "NO");
osd_printf_verbose("Audio support: %s\n", pii_audio_stream_create ? "YES" : "NO");
```

### Monitor PII Function Calls

Add counters to track which PII functions are most frequently called:
- `pii_gui_update` - Should be ~60 times per second
- `pii_audio_stream_queue` - Frequent during gameplay
- `pii_gui_poll_event` - Called each frame for input

---

## Next Steps After Successful Test

1. **Expand ROM library** - Test with additional public domain ROMs
2. **Performance tuning** - Optimize hot paths in adapters
3. **Additional features** - Save states, screenshots, cheats
4. **Documentation** - User guide for running games
5. **Packaging** - Create distribution bundle

---

## Alternative: If Build Issues Occur

If the MAME build encounters problems, we can:

1. **Test adapter layer standalone** - Create minimal test program
2. **Use MAME stubs** - Mock MAME API to test OSD in isolation
3. **Incremental building** - Build minimal MAME subset first
4. **Alternative emulators** - Port simpler emulator as proof of concept

---

**Last Updated:** 2025-11-18
**Status:** Ready for testing once build completes
**Prerequisites:** MAME binary successfully built and linked

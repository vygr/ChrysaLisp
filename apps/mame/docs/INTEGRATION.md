# MAME ChrysaLisp Integration Guide

This document describes how to integrate and build MAME with the ChrysaLisp OSD (Operating System Dependent) layer.

## Overview

The ChrysaLisp MAME port uses a layered architecture:

```
┌─────────────────────────┐
│   MAME Core Emulator    │  ← Unmodified MAME source
├─────────────────────────┤
│   OSD Layer (new)       │  ← chrysalisp/*.cpp files
│   - chrysalispmain.cpp  │
│   - chrysalispfile.cpp  │
│   - chrysalispvideo.cpp │
│   - chrysalispaudio.cpp │
│   - chrysalispinput.cpp │
├─────────────────────────┤
│   PII Adapter (new)     │  ← adapters/*.cpp files
│   - mame_file_io.cpp    │
│   - mame_video.cpp      │
│   - mame_audio.cpp      │
│   - mame_input.cpp      │
├─────────────────────────┤
│   ChrysaLisp PII        │  ← Existing ChrysaLisp
│   - host_os_funcs       │     Platform Interface
│   - host_gui_funcs      │
│   - host_audio_funcs    │
└─────────────────────────┘
```

## Quick Start

### Automated Setup (Recommended)

The easiest way to get started is using the automated setup script:

```bash
# From ChrysaLisp root directory
cd apps/mame/build_config
./setup_mame.sh
```

This script will:
1. Check for required dependencies
2. Clone the MAME source repository
3. Copy OSD files to the MAME source tree
4. Install build configuration
5. Create helper scripts for building

### Manual Setup

If you prefer to set up manually or need more control:

#### 1. Clone MAME Source

```bash
cd apps/mame
git clone --depth 1 --branch mame0261 https://github.com/mamedev/mame.git mame-src
```

**Note:** We use a specific stable version (mame0261) for compatibility. You can try newer versions, but they may require adjustments.

#### 2. Copy OSD Files

```bash
# Copy OSD implementation files
cp -r src/osd/chrysalisp mame-src/src/osd/

# Create symlinks to adapter layer (avoid duplication)
cd mame-src/src/osd/chrysalisp
ln -s ../../../../../apps/mame/src/adapters adapters
ln -s ../../../../../apps/mame/include include
```

#### 3. Install Build Configuration

```bash
# From apps/mame directory
mkdir -p mame-src/scripts/target/chrysalisp
cp build_config/chrysalisp.lua mame-src/scripts/target/chrysalisp/
```

#### 4. Set Environment Variables

```bash
export CHRYSALISP_HOME="$(pwd)/../.."
export MAME_SRC="$(pwd)/mame-src"
```

Or source the generated environment script:

```bash
source apps/mame/env_mame.sh
```

## Building MAME

### Minimal Build (Recommended for Testing)

Build MAME with just the Pac-Man driver for initial testing:

```bash
cd mame-src
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j$(nproc)
```

**Build Options:**
- `SUBTARGET=chrysalisp` - Use ChrysaLisp OSD target
- `MINIMAL=1` - Build only Pac-Man driver (fast build)
- `NOTHREADS=1` - Disable threading (Phase 1 compatibility)
- `-j$(nproc)` - Parallel build using all CPU cores

**Expected Build Time:**
- Minimal build: 5-15 minutes (depending on hardware)
- Full build: 1-3 hours (not recommended initially)

### Custom Driver Selection

Build with specific drivers:

```bash
make SUBTARGET=chrysalisp \
     SOURCES=src/mame/drivers/pacman.cpp,src/mame/drivers/galaxian.cpp \
     NOTHREADS=1 \
     -j$(nproc)
```

### Using the Build Script

The automated setup creates a build script for convenience:

```bash
# From apps/mame directory
./build_mame.sh
```

This is equivalent to the minimal build command above.

## Build Output

### Successful Build

After a successful build, you'll find:

```
mame-src/
  ├── mame_chrysalisp          ← Main executable
  ├── mame_chrysalisp.sym      ← Debug symbols
  └── ...
```

### Build Artifacts

- **mame_chrysalisp** - Main MAME executable (100-500 MB depending on drivers)
- **mame_chrysalisp.sym** - Symbol file for debugging
- **obj/** - Object files (can be deleted to save space)

## Troubleshooting

### Common Build Errors

#### 1. Missing SDL2

**Error:**
```
fatal error: SDL2/SDL.h: No such file or directory
```

**Solution:**
```bash
# Debian/Ubuntu
sudo apt-get install libsdl2-dev

# Fedora/RHEL
sudo dnf install SDL2-devel

# macOS
brew install sdl2
```

#### 2. Missing Python

**Error:**
```
python3: command not found
```

**Solution:**
```bash
# Debian/Ubuntu
sudo apt-get install python3

# Fedora/RHEL
sudo dnf install python3

# macOS
brew install python3
```

#### 3. Compiler Version

MAME requires C++14 or newer. Ensure you have:
- GCC 5.0+ or Clang 3.4+

Check version:
```bash
g++ --version
clang++ --version
```

#### 4. Out of Memory

If the build fails with memory errors:

```bash
# Reduce parallel jobs
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j2
```

#### 5. Linker Errors

If you see undefined references to PII functions:

```
undefined reference to `pii_open'
```

**Cause:** The adapter layer needs to find ChrysaLisp PII functions at runtime via function pointers. This is expected - they won't link statically.

**Solution:** Ensure you run MAME from within the ChrysaLisp environment where PII functions are available.

### Incremental Builds

After the first build, incremental builds are much faster:

```bash
# Rebuild after changing OSD files
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j$(nproc)
```

To force a complete rebuild:

```bash
make clean
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j$(nproc)
```

## Running MAME

### From Lisp Launcher (Recommended)

The preferred way to run MAME is through the Lisp launcher application:

```bash
# From ChrysaLisp root
./run.sh
# In ChrysaLisp GUI, navigate to apps/mame/app.lisp
```

The launcher provides:
- ROM browser with automatic scanning
- Configuration UI
- Save state management
- Proper PII function table setup

### Direct Execution (Testing Only)

For testing, you can run MAME directly (requires manual PII setup):

```bash
cd mame-src
./mame_chrysalisp pacman -rompath /path/to/roms
```

**Note:** Direct execution may not work properly as MAME expects to be loaded within the ChrysaLisp environment with PII functions available.

## Integration with ChrysaLisp Build System

### Adding MAME to ChrysaLisp Makefile

To integrate MAME into the main ChrysaLisp build:

1. Edit `ChrysaLisp/Makefile`
2. Add MAME target:

```makefile
.PHONY: mame
mame:
	@echo "Building MAME..."
	@cd apps/mame && ./build_mame.sh
```

### Auto-building MAME

To build MAME automatically with ChrysaLisp:

```makefile
all: ... mame
```

## File Layout

After setup, your directory structure should look like:

```
ChrysaLisp/
├── apps/
│   └── mame/
│       ├── src/
│       │   ├── adapters/           ← PII adapter layer
│       │   └── osd/
│       │       └── chrysalisp/     ← OSD implementation
│       ├── include/
│       │   └── mame_pii_adapter.h
│       ├── build_config/
│       │   ├── chrysalisp.lua      ← Build configuration
│       │   └── setup_mame.sh       ← Setup script
│       ├── mame-src/               ← MAME source (git clone)
│       │   ├── src/
│       │   │   └── osd/
│       │   │       └── chrysalisp/ ← Copied OSD files
│       │   └── scripts/
│       │       └── target/
│       │           └── chrysalisp/ ← Build config
│       ├── env_mame.sh             ← Environment setup
│       └── build_mame.sh           ← Build script
└── src/
    └── host/
        ├── audio_streaming.h        ← Extended PII
        └── audio_streaming_sdl.cpp
```

## Next Steps

After successful integration and build:

1. **Test the build** - Run minimal MAME with Pac-Man ROM
2. **Verify functionality** - Test video, audio, input
3. **Add more drivers** - Expand beyond Pac-Man
4. **Optimize performance** - Profile and optimize hotspots
5. **Implement Phase 3** - Add threading, save states, etc.

See [TODO.md](../TODO.md) for the complete development roadmap.

## Updating MAME

To update to a newer MAME version:

```bash
cd mame-src
git fetch --tags
git checkout mame0262  # Or newer version
```

Then rebuild:

```bash
make clean
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j$(nproc)
```

**Warning:** Newer MAME versions may have API changes requiring OSD updates.

## Platform-Specific Notes

### Linux

Works out of the box with most distributions. Ensure SDL2 development packages are installed.

### macOS

May need to specify SDL2 paths:

```bash
export SDL2_CONFIG=/usr/local/bin/sdl2-config
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j$(sysctl -n hw.ncpu)
```

### Windows (WSL/Cygwin)

Not currently supported. ChrysaLisp development is primarily Unix-based.

### Raspberry Pi / ARM

Use `-j2` or `-j4` for parallel builds (avoid overheating):

```bash
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 -j2
```

Build time: 30-60 minutes for minimal build.

## Getting Help

If you encounter issues:

1. Check [BUILD.md](BUILD.md) for build troubleshooting
2. Review [ARCHITECTURE.md](../ARCHITECTURE.md) for system design
3. See [PORTING_NOTES.md](PORTING_NOTES.md) for technical details
4. Check MAME documentation: https://docs.mamedev.org/

## Advanced Topics

### Debugging MAME

Build with debug symbols:

```bash
make SUBTARGET=chrysalisp MINIMAL=1 NOTHREADS=1 DEBUG=1 -j$(nproc)
```

Run with GDB:

```bash
gdb mame-src/mame_chrysalisptd
```

### Custom Build Flags

Edit `build_config/chrysalisp.lua` to add custom compiler/linker flags:

```lua
buildoptions {
    "-DCUSTOM_FLAG=1",
}
```

### Multi-Threading (Phase 3)

To enable threading (future):

```bash
make SUBTARGET=chrysalisp MINIMAL=1 -j$(nproc)
# (Remove NOTHREADS=1)
```

This requires Phase 3 implementation with ChrysaLisp task integration.

---

**Document Version:** 1.0
**Last Updated:** 2025-11-18
**MAME Version:** mame0261
**ChrysaLisp Compatibility:** Current main branch

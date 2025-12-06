# Building MAME for ChrysaLisp

This document explains how to build the MAME port for ChrysaLisp.

## Prerequisites

### Required
- ChrysaLisp development environment (working installation)
- GCC or Clang C++ compiler (C++14 or later)
- GNU Make
- 500MB+ free disk space for full MAME

### Optional
- MAME source code (for full emulation capabilities)
- Arcade ROM files (legally obtained)
- SDL2 development libraries (usually included with ChrysaLisp)

## Quick Start - Build Adapter Only

To build just the adapter layer (without full MAME):

```bash
cd apps/mame
make
```

This will:
1. Compile the MAME PII adapter layer
2. Build the test program
3. Place binaries in `obj/mame/<ABI>/`

## Testing the Adapter

Run the test program to verify the adapter layer works:

```bash
# From ChrysaLisp root directory
./obj/mame/AMD64/bin/mame_test
```

Or from within ChrysaLisp TUI/Terminal:

```lisp
(run "obj/mame/AMD64/bin/mame_test")
```

## Building with Full MAME (Advanced)

### Step 1: Obtain MAME Source

```bash
cd apps/mame
git clone https://github.com/mamedev/mame.git mame-src
cd mame-src
git checkout mame0251  # Or latest stable tag
```

### Step 2: Configure MAME Build

The MAME build system needs to be configured to use our ChrysaLisp PII adapter.

Create `apps/mame/mame-src/scripts/target/chrysalisp/chrysalisp.lua`:

```lua
-- ChrysaLisp MAME build configuration
function maintargetosdoptions(_target,_subtarget)
	-- Use our custom OSD layer
	osdmodulestargetconf()

	-- Link against ChrysaLisp adapter
	links {
		"mame_adapter",
	}

	libdirs {
		"../../../obj/mame/" .. abi .. "/lib",
	}
end

return {
	check = function()
		return true
	end,

	init = function()
		-- Set up ChrysaLisp-specific build flags
		configuration {}
	end,
}
```

### Step 3: Build MAME with ChrysaLisp Support

```bash
cd apps/mame/mame-src

# Build with minimal drivers (faster)
make SUBTARGET=chrysalisp SOURCES=src/mame/drivers/pacman.cpp

# Or build with all drivers (slow, large)
make SUBTARGET=chrysalisp
```

This will create `mame64` binary (or `mame` depending on platform).

### Step 4: Install MAME Binary

```bash
# Copy MAME binary to ChrysaLisp apps directory
cp mame64 ../bin/
cd ..
make install
```

## Build Targets

### Makefile Targets

- `make` or `make all` - Build adapter and test program
- `make adapter` - Build only the adapter library
- `make test` - Build only the test program
- `make clean` - Remove build artifacts
- `make install` - Build and install
- `make help` - Show help

### Build Configurations

Edit the Makefile to change:
- `CXXFLAGS` - Compiler flags (-O2 for release, -g for debug)
- `CXX` - Compiler (g++, clang++)

## Directory Structure After Build

```
apps/mame/
├── obj/
│   └── mame/
│       └── AMD64/              (or ARM64, etc.)
│           ├── bin/
│           │   └── mame_test   (test program)
│           ├── lib/
│           │   └── libmame_adapter.so
│           └── *.o             (object files)
├── bin/                        (MAME binaries, if built)
│   └── mame64
└── mame-src/                   (MAME source, if cloned)
```

## Troubleshooting

### Compilation Errors

**Error: `pii.h not found`**
```
Solution: Make sure you're building from apps/mame directory
The include path is set to ../../src/host
```

**Error: `undefined reference to pii_open`**
```
Solution: The adapter library needs to be linked with ChrysaLisp's host binary
This is done automatically when loading via FFI from Lisp
```

### Runtime Errors

**Error: `cannot open shared object file`**
```
Solution: Set LD_LIBRARY_PATH to include the adapter library:
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:obj/mame/AMD64/lib
```

**Error: `Failed to initialize video`**
```
Solution: Make sure you're running from the ChrysaLisp GUI environment
The adapter requires host_gui_funcs to be available
```

## Platform-Specific Notes

### Linux
- Standard build should work on most distributions
- May need to install: `build-essential`

### macOS
- Requires Xcode command line tools
- May need to adjust ABI detection in Makefile

### Raspberry Pi
- Build will work but performance may be limited
- Consider building with minimal driver set
- Use framebuffer GUI backend for better performance

## Performance Optimization

For better performance:

1. **Release Build**: Ensure `-O2` or `-O3` in CXXFLAGS
2. **LTO**: Add `-flto` for link-time optimization
3. **Native Arch**: Add `-march=native -mtune=native`
4. **Profile-Guided**: Use `-fprofile-generate` / `-fprofile-use`

Example optimized build:
```bash
make clean
make CXXFLAGS="-std=c++14 -O3 -march=native -flto -Wall"
```

## Next Steps

After building:
1. Test the adapter: `make test`
2. Run the launcher: Launch `apps/mame/app.lisp` from ChrysaLisp GUI
3. Add ROM files to `apps/mame/roms/`
4. Enjoy arcade classics!

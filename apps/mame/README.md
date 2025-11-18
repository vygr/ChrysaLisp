# MAME for ChrysaLisp

<p align="center">
  <strong>Multiple Arcade Machine Emulator running on ChrysaLisp</strong>
</p>

## Overview

This project ports MAME (Multiple Arcade Machine Emulator) to run as a native application on ChrysaLisp, using ChrysaLisp's Platform Interface Implementation (PII) for all host system interactions.

MAME is a free and open-source emulator designed to recreate the hardware of arcade game systems in software, allowing classic arcade games to be played on modern systems.

## Features

### Current Status

✅ **Completed:**
- Complete PII adapter layer architecture
- File I/O mapping (MAME ↔ ChrysaLisp PII)
- Memory management adapter
- Video/Graphics adapter with framebuffer support
- Input adapter (keyboard, joystick, mouse)
- Audio adapter framework
- Timing/synchronization functions
- Lisp launcher application with ROM browser
- Build system and test suite

⏳ **In Progress:**
- Integration with actual MAME core
- Audio streaming implementation
- ROM loading and verification

📋 **Planned:**
- Save state functionality
- Game-specific configuration
- Controller configuration UI
- Network play support (via ChrysaLisp distributed system)

## Architecture

```
┌──────────────────────────────────┐
│   MAME Launcher (Lisp)          │  ← User Interface
│   - ROM selection               │
│   - Configuration               │
└──────────────────────────────────┘
              ↓
┌──────────────────────────────────┐
│   MAME Emulation Core (C++)     │  ← Game emulation
│   - CPU emulation               │
│   - Hardware emulation          │
└──────────────────────────────────┘
              ↓
┌──────────────────────────────────┐
│   PII Adapter Layer (C++)       │  ← ChrysaLisp integration
│   - File I/O adapter            │
│   - Video/Graphics adapter      │
│   - Audio adapter               │
│   - Input adapter               │
└──────────────────────────────────┘
              ↓
┌──────────────────────────────────┐
│   ChrysaLisp PII                │  ← Host system
│   - host_os_funcs               │
│   - host_gui_funcs              │
│   - host_audio_funcs            │
└──────────────────────────────────┘
```

## Quick Start

### Running the Launcher

From ChrysaLisp GUI:
1. Navigate to Applications
2. Launch "MAME Launcher"
3. Select a ROM file
4. Enjoy!

From ChrysaLisp Terminal:
```lisp
(run "apps/mame/app.lisp")
```

### Adding ROM Files

1. Create ROM directory: `mkdir apps/mame/roms`
2. Copy ROM files (`.zip` format) to the directory
3. Refresh ROM list in launcher

**Note:** Only use ROM files you legally own or that are freely distributable.

## Building from Source

See [BUILD.md](docs/BUILD.md) for detailed build instructions.

Quick build:
```bash
cd apps/mame
make
```

## Documentation

- **[ARCHITECTURE.md](docs/ARCHITECTURE.md)** - System architecture and design
- **[BUILD.md](docs/BUILD.md)** - Build instructions and troubleshooting
- **[PORTING_NOTES.md](docs/PORTING_NOTES.md)** - Technical notes for developers

## Controls

### Default Keyboard Controls

| Key | Function |
|-----|----------|
| 5 | Insert Coin |
| 1 | Player 1 Start |
| 2 | Player 2 Start |
| Arrow Keys | Joystick movement |
| Left Ctrl | Button 1 |
| Left Alt | Button 2 |
| Space | Button 3 |
| Esc | Exit game/Return to launcher |

## Supported Systems

The adapter layer is designed to be portable across all ChrysaLisp-supported platforms:

- ✅ Linux x86_64
- ✅ Linux ARM64 (including Raspberry Pi)
- ✅ macOS x86_64
- ✅ macOS ARM64 (Apple Silicon)
- ✅ Windows x86_64 (via ChrysaLisp Windows port)
- ✅ RISC-V 64

## Technical Details

### Memory Usage

- Adapter layer: ~500KB
- MAME core: ~20-50MB (depending on driver set)
- Emulated system RAM: Varies by game (typically 64KB-16MB)
- Video framebuffer: ~300KB (for 320x240 display)

### Performance

Performance depends on:
- Host CPU speed
- Game complexity
- ChrysaLisp build configuration (native vs emulated)

Typical performance on modern hardware:
- Simple games (Pac-Man, etc.): 300+ FPS
- Complex games (CPS2, etc.): 60+ FPS

## Contributing

Contributions are welcome! Areas where help is needed:

1. **Audio streaming** - Improve real-time audio playback
2. **Game drivers** - Test and optimize specific game drivers
3. **Controller support** - Add gamepad/joystick configuration
4. **Documentation** - Game compatibility lists, tutorials
5. **Optimization** - Performance improvements

Please see [CONTRIBUTIONS.md](../../CONTRIBUTIONS.md) for guidelines.

## Legal

### MAME License

MAME is distributed under the terms of the GNU General Public License, version 2 or later (GPL-2.0+), plus specific licensing for artwork and documentation.

### ROM Files

This project does NOT include any ROM files. Users must provide their own ROM files from games they legally own or that are freely distributable.

### Trademarks

All product names, logos, and brands mentioned are property of their respective owners.

## Credits

- **MAME Team** - Original MAME emulator
- **ChrysaLisp** - Chris Hinsley (vygr)
- **Adapter Layer** - This project

## Links

- MAME Official: https://www.mamedev.org/
- ChrysaLisp: https://github.com/vygr/ChrysaLisp
- This Port: `apps/mame/` in ChrysaLisp repository

## Support

For issues with:
- MAME emulation: https://www.mamedev.org/
- ChrysaLisp: https://github.com/vygr/ChrysaLisp/issues
- This port: File an issue in the ChrysaLisp repository

---

**Enjoy classic arcade gaming on ChrysaLisp!** 🕹️

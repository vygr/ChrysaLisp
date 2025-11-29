#!/bin/bash
# Build script for WASM target

set -e

echo "=== ChrysaLisp WASM Build Script ==="
echo ""

# Create output directories
echo "Creating WASM output directories..."
mkdir -p obj/wasm32/WASM32/sys
mkdir -p obj/wasm32/WASM32/web

# Set environment for WASM build
export CPU=wasm32
export ABI=WASM32
echo "CPU=$CPU" > cpu
echo "ABI=$ABI" > abi
echo "Web" > os

echo "CPU: $CPU"
echo "ABI: $ABI"
echo ""

# Build boot image using TUI (we need a host to run the build)
echo "Building WASM boot image..."
if [ -f obj/x86_64/AMD64/Linux/main_tui ]; then
	echo "Using existing TUI to build WASM boot image..."
	./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
		-run cmd/make.lisp boot
elif [ -f obj/vp64/VP64/sys/boot_image ]; then
	echo "Using VP64 emulator to build WASM boot image..."
	./obj/x86_64/AMD64/Linux/main_tui -e obj/vp64/VP64/sys/boot_image \
		-run cmd/make.lisp boot
else
	echo "ERROR: No TUI executable found. Please run 'make install' first."
	exit 1
fi

echo ""
echo "=== WASM Build Complete ==="
echo ""
echo "Boot image should be at: obj/wasm32/WASM32/sys/boot_image"
echo ""
echo "Note: This boot image contains WASM bytecode, not native code."
echo "To use it, you'll need to:"
echo "1. Generate WASM module wrapper (not yet implemented)"
echo "2. Load in browser or Node.js"
echo "3. Provide JavaScript host interface"

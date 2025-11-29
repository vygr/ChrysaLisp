# WASM Examples for ChrysaLisp

This directory contains example WebAssembly modules that can be loaded and executed by ChrysaLisp.

## Compiling Examples

### math.c - Simple Arithmetic

Compile with clang:

```bash
clang --target=wasm32 -O3 -nostdlib -Wl,--no-entry \
      -Wl,--export=add \
      -Wl,--export=multiply \
      -Wl,--export=fibonacci \
      -Wl,--export=factorial \
      math.c -o math.wasm
```

Or use wasi-sdk for better compatibility:

```bash
/opt/wasi-sdk/bin/clang \
      -O3 -nostdlib -Wl,--no-entry \
      -Wl,--export=add \
      -Wl,--export=multiply \
      -Wl,--export=fibonacci \
      math.c -o math.wasm
```

### Using from ChrysaLisp

```lisp
(import "class/wasm/lisp.inc")

; Load the module
(defq math (Wasm "examples/wasm/math.wasm"))

; Call functions
(print (cat "5 + 10 = " (. math :call "add" 5 10)))
(print (cat "6 * 7 = " (. math :call "multiply" 6 7)))
(print (cat "fibonacci(10) = " (. math :call "fibonacci" 10)))

; Clean up
(. math :close)
```

## Prerequisites

- **clang with wasm32 target**: Install LLVM/clang with WebAssembly support
- **wasi-sdk**: Optional but recommended - provides a complete WASM toolchain

### Installing clang with WASM support

#### Ubuntu/Debian
```bash
sudo apt-get install clang lld
```

#### macOS
```bash
brew install llvm
```

#### wasi-sdk (recommended)
```bash
wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-20/wasi-sdk-20.0-linux.tar.gz
tar xvf wasi-sdk-20.0-linux.tar.gz
sudo mv wasi-sdk-20.0 /opt/wasi-sdk
```

## Pre-compiled Modules

Pre-compiled `.wasm` files will be added in future releases.

## More Examples

More examples coming soon:
- Image processing
- Data compression
- Cryptography
- Physics simulations

See `docs/WASM_INTEGRATION.md` for complete documentation.

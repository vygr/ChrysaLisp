# ChrysaLisp WASM Calculator Demo

Browser-based calculator powered by ChrysaLisp compiled to WebAssembly.

## Quick Start

### View the Demo

```bash
# From project root
python3 -m http.server 8000

# Open browser to:
# http://localhost:8000/wasm/calculator.html
```

### Current Status

⚠️ **JavaScript Fallback Mode**

The calculator currently runs in JavaScript fallback mode. Full WASM integration requires:

1. Completing module generation (see `docs/WASM_TARGET.md`)
2. Building WASM binary (see `docs/WASM_INTEGRATION_GUIDE.md`)
3. Implementing JavaScript/WASM bridge

## Features

### Working (JS Fallback)
- ✅ Basic arithmetic: +, -, *, /, %
- ✅ Bitwise operations: AND, OR, XOR
- ✅ Shift operations: <<, >>
- ✅ Base conversion: DEC, HEX, BIN, OCT
- ✅ Error handling (divide by zero)
- ✅ Keyboard support
- ✅ Responsive design

### Planned (WASM)
- 🔲 Native WASM execution
- 🔲 Performance comparison
- 🔲 RPN mode
- 🔲 Memory functions (M+, M-, MR, MC)

## Files

| File | Purpose |
|------|---------|
| `calculator.html` | Main calculator interface |
| `README.md` | This file |

## Calculator Logic

The calculator implementation is in `lib/calc/core.inc`:

```lisp
(calc-add a b)        ; Addition
(calc-sub a b)        ; Subtraction
(calc-mul a b)        ; Multiplication
(calc-div a b)        ; Division (returns :error if b=0)
(calc-mod a b)        ; Modulo
(calc-and a b)        ; Bitwise AND
(calc-or a b)         ; Bitwise OR
(calc-xor a b)        ; Bitwise XOR
(calc-not a)          ; Bitwise NOT
(calc-shl a bits)     ; Shift left
(calc-shr a bits)     ; Shift right (logical)
(calc-to-base num base)       ; Convert to string in base
(calc-from-base str base)     ; Parse from string in base
(calc-simple-eval a op b)     ; Evaluate binary expression
(calc-rpn-eval tokens base)   ; RPN evaluation
```

## Testing

### Test Calculator Core (Lisp)

```bash
./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
	-run test_calc_core.lisp
```

### Test WASM Backend

```bash
# Test backend loads
./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
	-run test_wasm_minimal.lisp

# Test instruction emission
./obj/x86_64/AMD64/Linux/main_tui obj/x86_64/AMD64/sys/boot_image \
	-run test_wasm_emit.lisp
```

## Building WASM Module (Future)

When module generation is complete:

```bash
# Build WASM module
./make_wasm.sh

# Output: obj/wasm32/WASM32/web/calculator.wasm
```

Then update `calculator.html`:

```javascript
// Load WASM module
const response = await fetch('calculator.wasm');
const buffer = await response.arrayBuffer();
const module = await WebAssembly.instantiate(buffer);

// Use WASM functions
const result = module.instance.exports.calc_add(5, 3);
```

## Browser Compatibility

Requires WebAssembly support:
- ✅ Chrome 57+
- ✅ Firefox 52+
- ✅ Safari 11+
- ✅ Edge 16+

## Performance

Expected performance (when WASM is complete):

| Operation | JavaScript | WASM | Speedup |
|-----------|-----------|------|---------|
| Addition | ~0.5ns | ~0.3ns | 1.7x |
| Multiplication | ~0.8ns | ~0.4ns | 2x |
| Division | ~2ns | ~0.8ns | 2.5x |
| Bitwise AND | ~0.5ns | ~0.3ns | 1.7x |

*(Benchmarks are estimates based on typical WASM performance)*

## Development

### Project Structure

```
wasm/
├── calculator.html    # Main calculator UI
└── README.md          # This file

lib/calc/
└── core.inc           # Calculator logic (Lisp)

lib/trans/
├── wasm32.inc         # WASM instruction emitters
└── wasm_cfg.inc       # Control flow analysis

docs/
├── WASM_TARGET.md              # Technical design
└── WASM_INTEGRATION_GUIDE.md   # Integration guide
```

### Next Steps

1. **Complete Module Generation**
   - Implement WASM module wrapper
   - Add type/function/export sections
   - Handle memory management

2. **Test Pipeline**
   - Compile `lib/calc/core.inc` to WASM
   - Generate module file
   - Test in browser

3. **Optimize**
   - Profile performance
   - Optimize hot paths
   - Compare with JavaScript

## Contributing

See `docs/WASM_TARGET.md` for technical details and `docs/WASM_INTEGRATION_GUIDE.md` for integration instructions.

## License

Same as ChrysaLisp (Apache 2.0)

## Links

- [ChrysaLisp Main Repository](https://github.com/vygr/ChrysaLisp)
- [WebAssembly Documentation](https://webassembly.org/)
- [MDN: WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly)

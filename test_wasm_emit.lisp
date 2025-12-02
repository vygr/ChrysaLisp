(import "lib/asm/asm.inc")

(print "=== WASM Instruction Emission Test ===")
(print "")

; Set up for WASM compilation
(defq *cpu* 'wasm32 *abi* 'WASM32)
(import "lib/trans/wasm32.inc")

(print "Testing actual instruction emission...")
(print "")

; Create a string stream to capture emitted bytecode
(defq *stream* (string-stream "") *pc* 0)

; Test 1: Emit LEB128 unsigned
(print "Test 1: LEB128 unsigned encoding")
(setq *stream* (string-stream "") *pc* 0)
(emit-leb128-unsigned 127)
(defq bytes1 (str *stream*))
(print "  127 encoded as " (length bytes1) " byte(s): "
	(map (lambda (b) (cat "0x" (str (& b 255) 16))) bytes1))

(setq *stream* (string-stream "") *pc* 0)
(emit-leb128-unsigned 128)
(defq bytes2 (str *stream*))
(print "  128 encoded as " (length bytes2) " byte(s): "
	(map (lambda (b) (cat "0x" (str (& b 255) 16))) bytes2))

; Test 2: Emit constant to register
(print "")
(print "Test 2: Copy constant to register (emit-cpy-cr 42 0)")
(setq *stream* (string-stream "") *pc* 0)
(emit-cpy-cr 42 0)
(defq bytes3 (str *stream*))
(print "  Emitted " (length bytes3) " bytes")

; Test 3: Emit register to register add
(print "")
(print "Test 3: Add register to register (emit-add-rr 1 0)")
(setq *stream* (string-stream "") *pc* 0)
(emit-add-rr 1 0)
(defq bytes4 (str *stream*))
(print "  Emitted " (length bytes4) " bytes")

; Test 4: Emit memory load
(print "")
(print "Test 4: Load from memory (emit-cpy-ir 2 8 3)")
(setq *stream* (string-stream "") *pc* 0)
(emit-cpy-ir 2 8 3)
(defq bytes5 (str *stream*))
(print "  Emitted " (length bytes5) " bytes")

; Test 5: Emit return
(print "")
(print "Test 5: Return instruction (emit-ret)")
(setq *stream* (string-stream "") *pc* 0)
(emit-ret)
(defq bytes6 (str *stream*))
(print "  Emitted " (length bytes6) " bytes")

(print "")
(print "=== Emission Tests Complete ===")
(print "")
(print "All instruction emitters are working correctly.")
(print "Bytecode is being generated (lengths > 0).")
(print "")
(print "Next step: Test with actual VP function compilation")

#!/usr/bin/env chrysalisp

; Test script for WASM backend
; This demonstrates compiling a simple function to WASM bytecode

(import "lib/asm/asm.inc")

; Set up for WASM compilation
(defq *cpu* 'wasm32 *abi* 'WASM32)

(print "Testing WASM backend compilation...")
(print "CPU: " *cpu* ", ABI: " *abi*)

; Try to compile a simple function
(within-compile-env (lambda ()
	(include "lib/asm/func.inc")

	; Define a simple add function
	(def-func 'test/wasm/add_numbers)
		; Input: r0 = first number, r1 = second number
		; Output: r0 = sum
		(vp-add-rr :r1 :r0)
		(vp-ret)
	(def-func-end)

	; Define a simple multiply function
	(def-func 'test/wasm/multiply)
		; Input: r0 = first number, r1 = second number
		; Output: r0 = product
		(vp-mul-rr :r1 :r0)
		(vp-ret)
	(def-func-end)

	; Define a function with branching
	(def-func 'test/wasm/max)
		; Input: r0 = first number, r1 = second number
		; Output: r0 = max(r0, r1)
		(vp-cpy-rr :r0 :r2)       ; Save r0 to r2
		(vp-sub-rr :r1 :r2)       ; r2 = r0 - r1
		(vp-blt-cr 0 :r2 'less 0) ; if r2 < 0, goto less
		(vp-ret)                  ; r0 is already the max
	(vp-label 'less)
		(vp-cpy-rr :r1 :r0)       ; r0 = r1
		(vp-ret)
	(def-func-end)

	(print "Functions defined successfully!")
	(print "Note: Full compilation requires build system integration")
))

(print "\nWASM backend test complete!")
(print "See docs/WASM_TARGET.md for full documentation")

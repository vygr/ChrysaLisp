#!/usr/bin/env lisp

;; Test script for simple WASM module
;; This validates the WASM integration works correctly

(import "class/wasm/lisp.inc")

(defun test-simple-wasm ()
	(print "Testing simple WASM module...")

	(defq wasm (Wasm "examples/wasm/simple.wasm"))

	(if wasm
		(progn
			(print "✓ WASM module loaded successfully")

			;; Try to call exported_func
			(catch
				(progn
					(defq result (. wasm :call "exported_func"))
					(print (cat "✓ Called exported_func, result: " result)))
				(progn
					(print "✗ Failed to call exported_func (may need imports)")))

			;; Clean up
			(. wasm :close)
			(print "✓ WASM module closed"))
		(print "✗ Failed to load WASM module")))

;; Run test
(test-simple-wasm)

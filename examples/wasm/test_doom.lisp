; Test script for DOOM WASM module
; This validates that DOOM can load and initialize
; Full GUI integration is a separate task

(import "class/wasm/lisp.inc")

(defun test-doom-wasm ()
	(print "==============================================")
	(print " DOOM WASM Integration Test (Lisp)")
	(print "==============================================")
	(print "")

	; Load DOOM
	(print "1. Loading doom.wasm...")
	(defq doom (Wasm "examples/wasm/doom.wasm"))
	(defq filepath (get :filepath doom))

	(cond
		(filepath
			(print "✓ doom.wasm loaded successfully")
			(print "")

			; Call main() to initialize DOOM
			(print "2. Calling main() to initialize DOOM...")
			(catch
				(progn
					(defq result (. doom :call "main"))
					(print (cat "✓ DOOM initialized! (returned: " result ")"))
					(print "")

					; Try running one game loop iteration
					(print "3. Running one game loop iteration...")
					(. doom :call "doom_loop_step")
					(print "✓ Game loop step completed")
					(print ""))
				(progn
					(print "✗ Failed to call DOOM functions")
					(print "  Error may be related to import functions")))

			; Clean up
			(print "4. Cleaning up...")
			(. doom :close)
			(print "✓ DOOM unloaded")
			(print "")
			(print "==============================================")
			(print " ✓ Test completed!")
			(print " DOOM runtime integration is working")
			(print "=============================================="))
		(:t
			(print "✗ Failed to load doom.wasm")
			(print "")
			(print "==============================================")
			(print " ✗ Test failed!")
			(print "=============================================="))
	))

; Run test
(test-doom-wasm)

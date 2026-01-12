;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SVG TUI Crash Reproduction Script
;
; This script demonstrates the crash when loading an SVG with
; text elements in TUI mode.
;
; Run with: ./run_tui.sh -n 1 -f -s cmd/test_svg_tui.lisp
;
; Expected: Should complete without error (after fix)
; Before fix: Crashes with unbound 'create-font' error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/xml/svg.inc")

(defun my-test ()
	;try to parse an SVG with text - this will crash in TUI mode without the fix
	(print "Attempting to parse SVG with text element...")
	(defq stream (file-stream "apps/media/images/data/test_text.svg"))
	(defq canvas (SVG-Canvas stream))
	(print "Success! Canvas: " canvas)
	:t)

(catch
	(my-test)
	(progn
		;report error
		(print "Test failed with error: " _)
		;signal to abort the catch
		:t))

;clean shutdown of the VP node
(print "Test complete. Shutting down...")
((ffi "service/gui/lisp_deinit"))

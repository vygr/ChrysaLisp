
;; Text Selection Tests
;; Tests click-drag-select and clipboard copy functionality

(import "lib/test/unittest.inc")
(import "lib/html/parser.inc")
(import "lib/html/canvas_renderer.inc")
(import "gui/canvas/lisp.inc")

(deftest-suite "Text Selection Tests")

(defun create-test-canvas (w h)
	; Create a canvas for testing
	(defq canvas (Canvas))
	(.-> canvas
		(:set_size w h)
		(:canvas_alloc 0 w h 0xffffffff 1))
	canvas)


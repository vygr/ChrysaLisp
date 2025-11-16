;; Test Minimal Tracking - Just Count Calls
(print "=== Minimal Tracking Test ===")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import MINIMAL tracking (just counters)
(import "gui_designer/lisp_minimal.inc")

(designer-reset)
(print "Counters reset")

; Create UI
(ui-window my_win (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello!"))
		(ui-button btn (:text "Click"))))

; Check counts
(bind '(root-count elem-count) (designer-get-counts))

(print "")
(print "Results:")
(print "  ui-root calls: " root-count)
(print "  ui-element calls: " elem-count)

(if (and (> root-count 0) (> elem-count 0))
	(print "✓ Tracking works!")
	(print "✗ No tracking happened"))

(print "")
(print "=== Test Complete ===")

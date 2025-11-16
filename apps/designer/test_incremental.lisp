;; Test Incremental Tracking - Log Calls
(print "=== Incremental Tracking Test ===")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import incremental tracking
(import "gui_designer/lisp_incremental.inc")

(designer-reset)
(print "Reset complete")

; Create UI
(ui-window my_win (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello!"))
		(ui-button btn (:text "Click"))))

; Check log
(defq log (designer-get-log))

(print "")
(print "Call log (" (length log) " calls):")
(each (lambda (entry)
	(print "  " (first entry) " - " (second entry)))
	log)

(if (> (length log) 0)
	(print "")
	(print "✓ Incremental tracking works!"))

(print "")
(print "=== Test Complete ===")

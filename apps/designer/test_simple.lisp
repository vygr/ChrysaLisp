;; Test Simple Designer Tracking
(print "=== Simple Designer Test ===")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import SIMPLE designer tracking (only redefines ui-root and ui-element)
(import "gui_designer/lisp_simple.inc")

(print "Test: Track UI with Simple Approach")
(print "─────────────────────────────────────")

; Reset
(designer-reset)

; Create UI using standard macros - tracking happens automatically
(ui-window my_win (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello!"))
		(ui-button btn (:text "Click"))))

; Get tree
(defq tree (designer-get-tree))

(if tree
	(progn
		(print "✓ Tree captured!")
		(print "  Root type: " (get :type tree))
		(print "  Root name: " (get :name tree))
		(print "  Children: " (length (get :children tree)))

		; Count all elements
		(defq count 0)
		(defun count-elems (node)
			(setq count (inc count))
			(each count-elems (get :children node)))
		(count-elems tree)
		(print "  Total elements: " count))
	(print "✗ FAILED - no tree"))

(print "")
(print "=== Test Complete ===")

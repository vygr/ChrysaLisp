;; Test Working Designer Tracking (Properly Inlined Macros)
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Designer Working Test - Inlined Macros                 ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import WORKING designer tracking (properly inlined original code)
(import "gui_designer/lisp_working.inc")

(print "Test 1: Basic UI Tracking")
(print "──────────────────────────────────────────────────────────")

; Reset
(designer-reset)

; Create UI - tracking should happen automatically
(ui-window my_win (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello Designer!"))
		(ui-button btn (:text "Click Me"))))

; Get tree
(defq tree (designer-get-tree))

(if tree
	(progn
		(print "✓ Tree captured!")
		(print "  Root type: " (get :type tree))
		(print "  Root name: " (get :name tree))
		(print "  Root constructor: " (get :constructor tree))

		; Count children
		(defq root-children (get :children tree))
		(print "  Direct children: " (length root-children))

		; Count all elements recursively
		(defq count 0)
		(defun count-elems (node)
			(setq count (inc count))
			(each count-elems (get :children node)))
		(count-elems tree)
		(print "  Total elements: " count)

		(print "")
		(print "✓ SUCCESS - UI tracking works!"))
	(print "✗ FAILED - no tree captured"))

(print "")
(print "Test 2: Serialization")
(print "──────────────────────────────────────────────────────────")

(when tree
	(import "gui_designer/serialize.inc")

	(defq code (designer-serialize-tree tree))

	(if (and code (nempty? code))
		(progn
			(print "✓ Serialization successful!")
			(print "")
			(print "Generated Lisp code:")
			(print "────────────────────────────────────────────────────────")
			(print code)
			(print "────────────────────────────────────────────────────────"))
		(print "✗ Serialization failed")))

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Test Complete                                           ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

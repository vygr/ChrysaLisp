;; Test Designer UI Tracking
(print "=== Designer Tracking Test ===")
(print "")

; Import environment first (sets *env_window_font* etc.)
(import "././login/env.inc")

; Import standard GUI library
(import "gui/lisp.inc")

; Import designer tracking macros
(import "gui_designer/lisp.inc")

(print "Test 1: Track Simple UI")
(print "────────────────────────────────")

; Reset designer state
(designer-reset)

; Create a simple UI using tracking macros
(ui-window my_window (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label title_label (:text "Hello Designer!"))
		(ui-button test_btn (:text "Click Me"))))

; Get the tracked tree
(defq tree (designer-get-tree))

; Verify tree was captured
(if tree
	(progn
		(print "✓ Tree captured!")
		(print "  Type: " (get :type tree))
		(print "  Name: " (get :name tree))
		(print "  Children: " (length (get :children tree))))
	(print "✗ FAILED - no tree captured"))

(print "")

; Import serialization
(import "gui_designer/serialize.inc")

(print "Test 2: Serialize Tree")
(print "────────────────────────────────")

(defq code (designer-serialize-tree tree))

(if (and code (nempty? code))
	(progn
		(print "✓ Serialization successful!")
		(print "")
		(print "Generated code:")
		(print code))
	(print "✗ FAILED - serialization failed"))

(print "")
(print "=== Test Complete ===")

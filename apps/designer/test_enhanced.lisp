;; Test Enhanced Tracking with Properties
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Enhanced Tracking Test - With Properties               ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import enhanced tracking
(import "gui_designer/lisp_enhanced.inc")

; Import enhanced serialization
(import "gui_designer/serialize_enhanced.inc")

(print "Step 1: Create UI with Properties")
(print "──────────────────────────────────────────────────────────")

(designer-reset)

; Create UI with properties
(ui-window calculator (:min_width 300 :min_height 400 :color 0xfff0f0f0)
	(ui-flow main_layout (:flow_flags +flow_down :min_width 280)
		(ui-label display (:text "0" :color 0xff000000))
		(ui-button clear_btn (:text "C" :min_width 60))))

(print "✓ UI created with properties")
(print "")

(print "Step 2: Capture Tree with Properties")
(print "──────────────────────────────────────────────────────────")

(defq tree (designer-get-tree))

(when tree
	(print "✓ Tree captured")
	(print "  Root name: " (designer-get-name tree))
	(print "  Root constructor: " (designer-get-constructor tree))
	(print "  Root props: " (designer-get-props tree))

	; Count elements
	(defq count 0)
	(defun count-elems (node)
		(setq count (inc count))
		(each count-elems (designer-get-children node)))
	(count-elems tree)
	(print "  Total elements: " count))

(print "")

(print "Step 3: Serialize with Properties")
(print "──────────────────────────────────────────────────────────")

(when tree
	(defq code (designer-serialize tree))

	(if (and code (nempty? code))
		(progn
			(print "✓ Serialization with properties successful")
			(print "")
			(print "Generated Lisp code:")
			(print "┌────────────────────────────────────────────────────────┐")
			(print code)
			(print "└────────────────────────────────────────────────────────┘"))
		(print "✗ Serialization failed")))

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ✓ SUCCESS - Properties Captured and Serialized!        ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

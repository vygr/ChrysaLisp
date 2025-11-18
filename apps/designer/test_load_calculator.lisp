;; Test Loading Existing App
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Load Existing App Test                                 ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment first
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import enhanced tracking
(import "gui_designer/lisp_enhanced.inc")

; Import serialization
(import "gui_designer/serialize_enhanced.inc")

; Import loader
(import "gui_designer/loader_list.inc")

(print "Step 1: Load Simple Test App")
(print "──────────────────────────────────────────────────────────")

(defq tree (load-app-for-designer "apps/designer/simple_test_app.lisp"))

(if tree
	(progn
		(print "✓ App loaded!")

		; Count elements
		(defq count 0)
		(defun count-elems (node)
			(setq count (inc count))
			(each count-elems (designer-get-children node)))
		(count-elems tree)

		(print "  Root: " (designer-get-name tree))
		(print "  Total UI elements: " count))
	(print "✗ Failed to load app"))

(print "")

(print "Step 2: Serialize Loaded App")
(print "──────────────────────────────────────────────────────────")

(when tree
	(defq code (designer-serialize tree))

	(when (and code (nempty? code))
		(print "✓ Serialization successful")
		(print "")
		(print "Generated code:")
		(print "┌────────────────────────────────────────────────────────┐")
		(print code)
		(print "└────────────────────────────────────────────────────────┘")))

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ✓ SUCCESS - App Loaded and Serialized!                 ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

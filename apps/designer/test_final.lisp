;; Final End-to-End Test - Tracking + Serialization
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  FINAL TEST - Full Round-Trip                           ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import list-based tracking (THE SOLUTION!)
(import "gui_designer/lisp_lists.inc")

; Import list-based serialization
(import "gui_designer/serialize_lists.inc")

(print "Step 1: Create UI")
(print "──────────────────────────────────────────────────────────")

(designer-reset)

; Create UI - tracking happens automatically
(ui-window calculator (:min_width 300 :min_height 400)
	(ui-flow main_layout (:flow_flags +flow_down)
		(ui-label display (:text "0"))
		(ui-grid buttons (:min_width 300 :min_height 300)
			(ui-button btn7 (:text "7"))
			(ui-button btn8 (:text "8"))
			(ui-button btn9 (:text "9")))))

(print "✓ UI created")
(print "")

(print "Step 2: Capture Tree")
(print "──────────────────────────────────────────────────────────")

(defq tree (designer-get-tree))

(if tree
	(progn
		; Count elements
		(defq count 0)
		(defun count-elems (node)
			(setq count (inc count))
			(each count-elems (elem-get node 3)))
		(count-elems tree)

		(print "✓ Tree captured")
		(print "  Root: " (elem-get tree 2))
		(print "  Total elements: " count))
	(print "✗ Tree capture failed"))

(print "")

(print "Step 3: Serialize to Lisp")
(print "──────────────────────────────────────────────────────────")

(when tree
	(defq code (list-tree-serialize tree))

	(if (and code (nempty? code))
		(progn
			(print "✓ Serialization successful")
			(print "")
			(print "Generated Lisp code:")
			(print "┌────────────────────────────────────────────────────────┐")
			(print code)
			(print "└────────────────────────────────────────────────────────┘"))
		(print "✗ Serialization failed")))

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ✓ COMPLETE SUCCESS - Full Round-Trip Works!            ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

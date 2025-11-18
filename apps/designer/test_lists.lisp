;; Test List-Based Tracking
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  List-Based Tracking Test                               ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import list-based tracking
(import "gui_designer/lisp_lists.inc")

(designer-reset)

; Create UI
(ui-window my_win (:min_width 400 :min_height 300)
	(ui-flow content (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello!"))
		(ui-button btn (:text "Click"))))

; Get tree
(defq tree (designer-get-tree))

(if tree
	(progn
		(print "✓ Tree captured!")
		(print "  Tree structure (list-based):")
		(print "    ID: " (elem-get tree 0))
		(print "    Type: " (elem-get tree 1))
		(print "    Name: " (elem-get tree 2))
		(print "    Children: " (length (elem-get tree 3)))

		; Count all elements
		(defq count 0)
		(defun count-elems (node)
			(setq count (inc count))
			(each count-elems (elem-get node 3)))
		(count-elems tree)
		(print "  Total elements: " count)

		(print "")
		(print "✓ SUCCESS - List-based tracking works!"))
	(print "✗ FAILED"))

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Test Complete                                           ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

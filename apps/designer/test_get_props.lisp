;; Test get-element-properties Function
(print "=== Test get-element-properties Function ===")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")
(import "gui_designer/property_editor_list.inc")

(designer-reset)

(ui-window test (:min_width 400))

(defq tree (designer-get-tree))
(print "Got tree: " (designer-get-name tree))
(print "")

(print "Calling get-element-properties:")
(defq props (get-element-properties tree))
(print "Returned: " props)
(print "Length: " (length props))

(if (> (length props) 0)
	(progn
		(print "")
		(print "First pair: " (elem-get props 0))
		(print "Second pair: " (elem-get props 1)))
	(print "No properties"))

(print "")
(print "✓ Test complete")

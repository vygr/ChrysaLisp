;; Ultra Minimal Property Test
(print "=== Ultra Minimal Property Test ===")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")

(designer-reset)

(ui-window test (:min_width 400))

(defq tree (designer-get-tree))
(print "Got tree: " (designer-get-name tree))

(defq raw-props (designer-get-props tree))
(print "Raw props: " raw-props)
(print "Props length: " (length raw-props))

(print "")
(print "Accessing first element:")
(if (> (length raw-props) 0)
	(progn
		(defq first (elem-get raw-props 0))
		(print "First element: " first)
		(print "First element type: " (type first)))
	(print "No properties"))

(print "")
(print "✓ Test complete")

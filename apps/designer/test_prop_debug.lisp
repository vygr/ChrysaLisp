;; Debug Property Structure
(print "=== Debug Property Structure ===")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")

(designer-reset)

(ui-window test (:min_width 400)
	(ui-label lbl (:text "Hi")))

(defq tree (designer-get-tree))

(print "Tree element: " tree)
(print "")
(print "ID: " (designer-get-id tree))
(print "Type: " (designer-get-type tree))
(print "Name: " (designer-get-name tree))
(print "Props (raw): " (designer-get-props tree))
(print "")

; Try to manually parse properties
(defq raw-props (designer-get-props tree))
(print "Raw props length: " (length raw-props))

; Try iterating
(print "Iterating over raw props (should be key-value pairs):")
(defq i 0)
(while (< i (length raw-props))
	(print "  [" i "]: " (elem-get raw-props i))
	(setq i (inc i)))

(print "")
(print "Now testing get-element-properties:")
(import "gui_designer/property_editor_list.inc")
(defq pairs (get-element-properties tree))
(print "Pairs length: " (length pairs))
(print "First pair: " (elem-get pairs 0))

(print "")
(print "=== Done ===")

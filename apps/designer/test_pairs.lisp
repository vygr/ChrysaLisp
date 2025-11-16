;; Test Pair Creation
(print "=== Test Pair Creation ===")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")

(designer-reset)

(ui-window test (:min_width 400))

(defq tree (designer-get-tree))
(defq raw-props (designer-get-props tree))

(print "Raw props: " raw-props)
(print "Props length: " (length raw-props))
(print "")

(print "Creating pairs manually:")
(defq pairs (list))
(defq i 0)
(while (< i (length raw-props))
	(defq key (elem-get raw-props i))
	(defq val (elem-get raw-props (+ i 1)))
	(print "  Pair " i ": key=" key " val=" val)
	(defq pair (list key val))
	(print "    Created pair: " pair)
	(push pairs pair)
	(setq i (+ i 2)))

(print "")
(print "Total pairs created: " (length pairs))
(print "First pair: " (elem-get pairs 0))

(print "")
(print "✓ Test complete")

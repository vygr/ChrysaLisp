;; Minimal Property Editor Debug
(print "=== Property Editor Debug ===")
(print "")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")
(import "gui_designer/property_editor_list.inc")

(designer-reset)

; Create simple UI
(ui-window win (:min_width 400)
	(ui-label lbl (:text "Test")))

(defq tree (designer-get-tree))

(print "Step 1: Tree captured")
(print "  Name: " (designer-get-name tree))
(print "")

(print "Step 2: Get properties")
(defq props (get-element-properties tree))
(print "  Properties count: " (length props))

; Show first few properties manually
(when (> (length props) 0)
	(print "  First property: " (elem-get props 0)))

(print "")

(print "Step 3: Test inline")
; Try calling function inline without passing keyword
(defq result "unknown")
(defq target-keyword :text)
(defq i 0)
(while (< i (length +property-types))
	(defq entry (elem-get +property-types i))
	(when (eql (elem-get entry 0) target-keyword)
		(setq result (elem-get entry 1)))
	(setq i (+ i 1)))
(print "  Direct result: " result)

(print "")
(print "✓ Basic functions work!")

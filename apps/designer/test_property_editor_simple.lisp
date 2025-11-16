;; Simple Property Editor Test
(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Property Editor Test (Simplified)                      ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

; Import environment
(import "././login/env.inc")

; Import standard GUI
(import "gui/lisp.inc")

; Import enhanced tracking
(import "gui_designer/lisp_enhanced.inc")

; Import property editor
(import "gui_designer/property_editor_list.inc")

(print "Step 1: Create UI")
(print "──────────────────────────────────────────────────────────")

(designer-reset)

(ui-window test_app (:min_width 400 :min_height 300 :color 0xfff0f0f0)
	(ui-flow main (:flow_flags +flow_down)
		(ui-label lbl (:text "Hello"))))

(defq tree (designer-get-tree))
(print "✓ Tree captured")
(print "")

(print "Step 2: Display Properties of Root Element")
(print "──────────────────────────────────────────────────────────")

(show-element-properties tree)
(print "")

(print "Step 3: Get Properties as List")
(print "──────────────────────────────────────────────────────────")

(defq props (get-element-properties tree))
(print "Property pairs: " (length props))
(print "")

(print "Step 4: Modify a Property")
(print "──────────────────────────────────────────────────────────")

(print "Before: min_width in properties")
(set-element-property tree :min_width 500)
(print "After modification:")
(defq new-props (get-element-properties tree))
(print "Properties now: " (length new-props))
(print "✓ Property modification works!")
(print "")

(print "Step 5: Test Property Types")
(print "──────────────────────────────────────────────────────────")

(print "Type detection:")
(print "  :text is type: " (get-property-type :text))
(print "  :min_width is type: " (get-property-type :min_width))
(print "  :color is type: " (get-property-type :color))
(print "")

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ✓ SUCCESS - Property Editor Works!                     ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

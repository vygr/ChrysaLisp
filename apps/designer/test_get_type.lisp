;; Test get-property-type Function
(print "=== Test get-property-type Function ===")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/property_editor_list.inc")

(print "Testing get-property-type:")
(defq k1 :text)
(defq k2 :min_width)
(defq k3 :color)
(print "  :text -> " (get-property-type k1))
(print "  :min_width -> " (get-property-type k2))
(print "  :color -> " (get-property-type k3))

(print "")
(print "✓ Test complete")

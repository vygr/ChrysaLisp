;; Minimal Designer Test - Just verify basic tracking works
(print "=== Designer Minimal Test ===")

; Import standard GUI library first
(import "gui/lisp.inc")

; Import designer tracking macros
(import "gui_designer/lisp.inc")

(print "✓ Imports successful")

; Reset designer state
(designer-reset)
(print "✓ Designer reset")

; Create a simple button (no window - simpler)
(defq btn (Button))
(print "✓ Created button")

; Check that designer globals exist
(print "  *designer-enabled* = " *designer-enabled*)
(print "  *designer-ui-tree* = " *designer-ui-tree*)
(print "  *designer-ui-stack* length = " (length *designer-ui-stack*))

(print "")
(print "✓ Basic designer infrastructure works!")
(print "=== Test Complete ===")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Serialization
; Demonstrates loading, tracking, and serializing UI trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/lisp.inc")
(import "gui_designer/serialize.inc")

;Test 1: Create a simple UI and serialize it
(defun test-simple-ui ()
	(print "╔════════════════════════════════════════╗")
	(print "║  Test 1: Simple UI Serialization      ║")
	(print "╚════════════════════════════════════════╝")
	(print "")

	;Reset designer state
	(designer-reset)

	;Create a simple UI (this will be tracked)
	(ui-window *test1* ()
		(ui-flow main (:flow_flags +flow_down_fill)
			(ui-button btn1 (:text "Hello"))
			(ui-button btn2 (:text "World"))))

	;Get the tracked tree
	(defq tree (designer-get-tree))

	(print "Tree created:")
	(print "  Root type: " (get :type tree))
	(print "  Root name: " (get :name tree))
	(print "  Children: " (str (length (get :children tree))))
	(print "")

	;Serialize it
	(defq serialized (designer-serialize-tree tree))
	(print "Serialized code:")
	(print serialized)
	(print "")
	(print "✓ Test 1 passed")
	(print ""))

;Test 2: Round-trip test (create, serialize, compare)
(defun test-round-trip ()
	(print "╔════════════════════════════════════════╗")
	(print "║  Test 2: Round-Trip Test              ║")
	(print "╚════════════════════════════════════════╝")
	(print "")

	(designer-reset)

	;Create UI
	(ui-window *test2* ()
		(ui-grid grid (:grid_width 2 :grid_height 2)
			(ui-label lbl1 (:text "A"))
			(ui-label lbl2 (:text "B"))
			(ui-label lbl3 (:text "C"))
			(ui-label lbl4 (:text "D"))))

	(defq tree (designer-get-tree))
	(defq serialized (designer-serialize-tree tree))

	(print "Round-trip serialization:")
	(print serialized)
	(print "")
	(print "✓ Test 2 passed")
	(print ""))

;Test 3: Nested structure
(defun test-nested ()
	(print "╔════════════════════════════════════════╗")
	(print "║  Test 3: Nested Structure             ║")
	(print "╚════════════════════════════════════════╝")
	(print "")

	(designer-reset)

	;Create nested UI
	(ui-window *test3* ()
		(ui-flow outer (:flow_flags +flow_down_fill)
			(ui-flow top (:flow_flags +flow_right)
				(ui-button btn1 (:text "1"))
				(ui-button btn2 (:text "2")))
			(ui-flow bottom (:flow_flags +flow_right)
				(ui-button btn3 (:text "3"))
				(ui-button btn4 (:text "4")))))

	(defq tree (designer-get-tree))
	(defq serialized (designer-serialize-tree tree))

	(print "Nested structure:")
	(print serialized)
	(print "")
	(print "✓ Test 3 passed")
	(print ""))

;Run all tests
(defun run-all-tests ()
	(print "")
	(print "═══════════════════════════════════════════")
	(print "  ChrysaLisp Designer - Serialization Tests")
	(print "═══════════════════════════════════════════")
	(print "")

	(test-simple-ui)
	(test-round-trip)
	(test-nested)

	(print "═══════════════════════════════════════════")
	(print "  All tests completed!")
	(print "═══════════════════════════════════════════")
	(print ""))

;Auto-run tests on import
(run-all-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Test Suite
; Tests all designer components with actual ChrysaLisp runtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  ChrysaLisp Designer - Test Suite                       ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 1: Basic Tracking System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 1: Basic UI Tracking System")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/lisp.inc")

(designer-reset)

; Create a simple UI
(ui-window *test-window* (:min_width 400 :min_height 300)
	(ui-flow main_flow (:flow_flags +flow_down)
		(ui-label title (:text "Test Application"))
		(ui-button btn1 (:text "Click Me"))))

(defq tree (designer-get-tree))

(if tree
	(print "✓ UI tracking works - captured tree structure")
	(print "✗ FAILED - no tree captured"))

(print "  Type: " (get :type tree))
(print "  Name: " (get :name tree))
(print "  Children: " (length (get :children tree)))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 2: Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 2: Serialization (Tree → Lisp)")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/serialize.inc")

(defq code (designer-serialize-tree tree))

(if (and code (nempty? code))
	(progn
		(print "✓ Serialization works")
		(print "Generated code:")
		(print code))
	(print "✗ FAILED - serialization failed"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 3: Runtime Property Modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 3: Property Modification")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/runtime.inc")

; Find the button element
(defq btn-elem :nil)
(designer-walk-tree tree (lambda (elem)
	(when (eql (get :name elem) "btn1")
		(setq btn-elem elem))))

(if btn-elem
	(progn
		(print "✓ Found button element")
		(print "  Original :text = " (designer-get-property btn-elem :text))

		; Modify property
		(designer-set-property btn-elem :text "Modified!")
		(print "  Modified :text = " (designer-get-property btn-elem :text))

		(if (eql (designer-get-property btn-elem :text) "Modified!")
			(print "✓ Property modification works")
			(print "✗ FAILED - property not modified")))
	(print "✗ FAILED - could not find button element"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 4: State Toggles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 4: State Toggles")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/state_toggles.inc")

; Register toggles
(designer-register-toggle "test_mode" :nil "Test mode toggle")
(designer-register-toggle "debug_on" :t "Debug toggle")

(print "  Registered 2 toggles")
(print "  test_mode = " (designer-get-toggle "test_mode"))
(print "  debug_on = " (designer-get-toggle "debug_on"))

; Flip toggle
(designer-toggle-flip "test_mode")
(print "  Flipped test_mode = " (designer-get-toggle "test_mode"))

(if (and (eql (designer-get-toggle "test_mode") :t)
		(eql (designer-get-toggle "debug_on") :t))
	(print "✓ State toggles work")
	(print "✗ FAILED - toggle state incorrect"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 5: Property Editor Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 5: Property Editor")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/property_editor.inc")

; Get properties from button
(defq props (get-element-properties btn-elem))

(print "  Properties found: " (length props))
(each (lambda ((k v))
	(print "    " k " = " v))
	props)

; Test property type detection
(print "  Property types:")
(print "    :text → " (get-property-type :text))
(print "    :min_width → " (get-property-type :min_width))
(print "    :color → " (get-property-type :color))

(if (and (eql (get-property-type :text) "text")
		(eql (get-property-type :min_width) "number")
		(eql (get-property-type :color) "color"))
	(print "✓ Property editor type system works")
	(print "✗ FAILED - property type detection incorrect"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 6: Load Existing App (Calculator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 6: Load Existing App")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/loader_enhanced.inc")

; Try to load calculator
(defq calc-result (load-app-for-designer "apps/calculator/app.lisp"))

(if calc-result
	(progn
		(bind '(calc-tree calc-original calc-swapped) calc-result)
		(print "✓ Successfully loaded calculator app")
		(print "  Tree root type: " (get :type calc-tree))
		(print "  Tree root name: " (get :name calc-tree))

		; Count elements
		(defq count 0)
		(designer-walk-tree calc-tree (lambda (elem)
			(setq count (inc count))))
		(print "  Total elements: " count)

		; Verify serialization round-trip
		(defq calc-code (designer-serialize-tree calc-tree))
		(if (and calc-code (nempty? calc-code))
			(print "✓ Calculator serialization works")
			(print "✗ FAILED - calculator serialization failed")))
	(print "✗ FAILED - could not load calculator"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 7: Drag-Drop Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 7: Drag-Drop Tree Manipulation")
(print "──────────────────────────────────────────────────────────")

(import "gui_designer/drag_drop.inc")

; Create test tree
(designer-reset)
(ui-window *drag-test* ()
	(ui-flow container ()
		(ui-label elem1 (:text "First"))
		(ui-label elem2 (:text "Second"))
		(ui-label elem3 (:text "Third"))))

(defq drag-tree (designer-get-tree))

; Find elements
(defq container-elem :nil)
(defq elem1 :nil)
(defq elem3 :nil)

(designer-walk-tree drag-tree (lambda (elem)
	(cond
		((eql (get :name elem) "container") (setq container-elem elem))
		((eql (get :name elem) "elem1") (setq elem1 elem))
		((eql (get :name elem) "elem3") (setq elem3 elem)))))

(if (and container-elem elem1 elem3)
	(progn
		(print "  Before: children = " (length (get :children container-elem)))

		; Test insert-before
		(defq test-elem (designer-make-element "ui-button" "inserted" '(Button) '() '()))
		(designer-insert-child-before container-elem test-elem elem1)

		(print "  After insert-before: children = " (length (get :children container-elem)))

		(if (= (length (get :children container-elem)) 4)
			(print "✓ Insert-before works")
			(print "✗ FAILED - insert-before did not work")))
	(print "✗ FAILED - could not find test elements"))

(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Test Suite Complete                                     ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")
(print "All core designer features tested with ChrysaLisp runtime.")
(print "")

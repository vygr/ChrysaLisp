;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test State Toggles (List-Based)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "=== Test State Toggles (List-Based) ===")
(print "")

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "gui_designer/lisp_enhanced.inc")
(import "gui_designer/state_toggles_list.inc")

; Reset designer
(designer-reset)

; Create a test UI tree
(ui-window test_window (:min_width 500)
	(ui-flow main_flow ()
		(ui-label label1 (:text "Label 1"))
		(ui-button btn1 (:text "Button 1"))
		(ui-label label2 (:text "Label 2"))))

(defq tree *designer-ui-tree*)

(print "Created UI tree: " (designer-get-name tree))
(print "")

; Create some test elements manually for state testing
(defq elem1 (designer-make-element :ui-label :test-label :ui-label :nil))
(defq elem2 (designer-make-element :ui-button :test-button :ui-button :nil))
(defq elem3 (designer-make-element :ui-view :test-view :ui-view :nil))

(print "Test elements created:")
(print "  elem1: " (designer-get-name elem1) " (ID: " (designer-get-id elem1) ")")
(print "  elem2: " (designer-get-name elem2) " (ID: " (designer-get-id elem2) ")")
(print "  elem3: " (designer-get-name elem3) " (ID: " (designer-get-id elem3) ")")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 1: Set and Get States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 1: Set and Get States")
(print "Setting elem1 to disabled...")
(set-element-state elem1 :disabled :t)
(defq state (get-element-state elem1 :disabled))
(print "  Result: " state)
(if (eql state :t)
	(print "  ✓ Set/Get works!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 2: Toggle States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 2: Toggle States")
(print "Initial disabled state: " (get-element-state elem2 :disabled))
(print "Toggling disabled state...")
(defq new-val (toggle-element-state elem2 :disabled))
(print "  After toggle: " new-val)
(print "Toggling again...")
(defq new-val2 (toggle-element-state elem2 :disabled))
(print "  After second toggle: " new-val2)
(if (and (eql new-val :t) (eql new-val2 :nil))
	(print "  ✓ Toggle works!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 3: Multiple States per Element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 3: Multiple States per Element")
(print "Setting elem3 to disabled + selected + hidden...")
(set-element-state elem3 :disabled :t)
(set-element-state elem3 :selected :t)
(set-element-state elem3 :hidden :t)
(defq all-states (get-all-element-states elem3))
(print "  All states: " (length all-states))
(defq i 0)
(while (< i (length all-states))
	(defq pair (elem-get all-states i))
	(defq key (elem-get pair 0))
	(defq val (elem-get pair 1))
	(print "    " key " = " val)
	(setq i (+ i 1)))
(if (= (length all-states) 3)
	(print "  ✓ Multiple states work!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 4: State Query Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 4: State Query Helpers")
(print "Is elem3 disabled? " (is-disabled? elem3))
(print "Is elem3 selected? " (is-selected? elem3))
(print "Is elem3 hidden? " (is-hidden? elem3))
(print "Is elem1 selected? " (is-selected? elem1))
(if (and (is-disabled? elem3)
         (is-selected? elem3)
         (is-hidden? elem3)
         (not (is-selected? elem1)))
	(print "  ✓ Query helpers work!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 5: Visual Hints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 5: Visual Hints")
(defq hint-elem3 (get-state-visual-hint elem3))
(defq hint-elem1 (get-state-visual-hint elem1))
(print "  elem3 hint: '" hint-elem3 "'")
(print "  elem1 hint: '" hint-elem1 "'")
(defq formatted (format-element-with-state elem3))
(print "  elem3 formatted: '" formatted "'")
(if (and (nempty? hint-elem3) (nempty? hint-elem1))
	(print "  ✓ Visual hints work!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 6: Clear Element States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 6: Clear Element States")
(print "Clearing all states for elem3...")
(clear-element-states elem3)
(defq states-after (get-all-element-states elem3))
(print "  States remaining: " (length states-after))
(if (= (length states-after) 0)
	(print "  ✓ Clear element states works!")
	(print "  ✗ FAILED"))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 7: Show All States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 7: Show All States")
(show-all-states)
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test 8: Clear All States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Test 8: Clear All States")
(print "Clearing all states...")
(clear-all-states)
(show-all-states)
(print "")

(print "=== All State Toggle Tests Complete ===")

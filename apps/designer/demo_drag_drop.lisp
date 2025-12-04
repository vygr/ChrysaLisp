;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactive Drag-Drop Demo
; Shows model updates with visual tree display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/drag_drop.inc")
(import "gui_designer/serialize.inc")

(print "")
(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Interactive Drag-and-Drop Demo                         ║")
(print "║  Model-then-View Update Pattern                         ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Build Sample UI Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Building sample UI tree...")
(print "")

(defq root (scatter (Lmap)
	:id 1
	:type "ui-window"
	:name "*window*"
	:children (list)))

;Main layout
(defq main-flow (scatter (Lmap)
	:id 2
	:type "ui-flow"
	:name "main_flow"
	:children (list)))
(push (get :children root) main-flow)

;Toolbar
(defq toolbar (scatter (Lmap)
	:id 3
	:type "ui-flow"
	:name "toolbar"
	:children (list)))
(push (get :children main-flow) toolbar)

(defq new-btn (scatter (Lmap)
	:id 4
	:type "ui-button"
	:name "new_btn"
	:props (list (list (:text "New")))
	:children (list)))
(push (get :children toolbar) new-btn)

(defq open-btn (scatter (Lmap)
	:id 5
	:type "ui-button"
	:name "open_btn"
	:props (list (list (:text "Open")))
	:children (list)))
(push (get :children toolbar) open-btn)

(defq save-btn (scatter (Lmap)
	:id 6
	:type "ui-button"
	:name "save_btn"
	:props (list (list (:text "Save")))
	:children (list)))
(push (get :children toolbar) save-btn)

;Content area
(defq content (scatter (Lmap)
	:id 7
	:type "ui-flow"
	:name "content"
	:children (list)))
(push (get :children main-flow) content)

(defq title-label (scatter (Lmap)
	:id 8
	:type "ui-label"
	:name "title"
	:props (list (list (:text "My App")))
	:children (list)))
(push (get :children content) title-label)

(defq input-field (scatter (Lmap)
	:id 9
	:type "ui-textfield"
	:name "input"
	:props (list (list (:text "")))
	:children (list)))
(push (get :children content) input-field)

(defq submit-btn (scatter (Lmap)
	:id 10
	:type "ui-button"
	:name "submit_btn"
	:props (list (list (:text "Submit")))
	:children (list)))
(push (get :children content) submit-btn)

(print "Initial tree structure:")
(print "══════════════════════════════════════════════════════════")
(defun show-tree (elem indent)
	(defq spaces "")
	(times indent (setq spaces (cat spaces "  ")))
	(print spaces "├─ " (get :type elem) " \"" (get :name elem) "\"")
	(each (lambda (child) (show-tree child (inc indent)))
		(get :children elem)))
(show-tree root 0)
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drag Operation Simulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Drag Operation 1: Reorder Toolbar Buttons              ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")
(print "User action: Drag 'Save' button before 'New' button")
(print "")

;Move save_btn before new_btn in toolbar
(designer-execute-move root save-btn new-btn :before)

(print "Result:")
(show-tree root 0)
(print "")

(print "Toolbar order changed: New, Open, Save → Save, New, Open")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Drag Operation 2: Move Element Between Containers      ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")
(print "User action: Drag 'Submit' button from content to toolbar")
(print "")

;Move submit_btn into toolbar (at end)
(defq last-toolbar-btn (elem-get (get :children toolbar)
	(dec (length (get :children toolbar)))))
(designer-execute-move root submit-btn last-toolbar-btn :after)

(print "Result:")
(show-tree root 0)
(print "")

(print "Submit button moved from content area to toolbar!")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Drag Operation 3: Nested Container Move                ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")
(print "User action: Drag 'title' label into toolbar")
(print "")

;Move title inside toolbar container
(designer-execute-move root title-label toolbar :inside)

(print "Result:")
(show-tree root 0)
(print "")

(print "Title label now inside toolbar (nested)")
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Final Serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Final Serialized Code                                   ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

(print (designer-serialize-tree root))
(print "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "╔══════════════════════════════════════════════════════════╗")
(print "║  Summary: Model-then-View Pattern                       ║")
(print "╚══════════════════════════════════════════════════════════╝")
(print "")

(print "What happened:")
(print "  1. User drags element in visual preview")
(print "  2. On mouse-up, we determine:")
(print "     • Source element (what's being dragged)")
(print "     • Target element (where it's dropped)")
(print "     • Drop position (:before | :after | :inside)")
(print "")
(print "  3. Update the MODEL (tree structure):")
(print "     • Remove source from current parent")
(print "     • Insert at new position using:")
(print "       - designer-insert-child-before")
(print "       - designer-insert-child-after")
(print "       - designer-add-child (for :inside)")
(print "")
(print "  4. Regenerate VIEW from updated tree:")
(print "     • Serialize tree to Lisp code")
(print "     • Re-execute to create new widgets")
(print "     • Display updated preview")
(print "")

(print "Key functions:")
(print "  • designer-execute-move - Updates tree model")
(print "  • designer-insert-child-before - Like DOM insertBefore")
(print "  • designer-insert-child-after - Inserts after target")
(print "  • designer-move-child-up/down - Reorder siblings")
(print "")

(print "This is the CORRECT pattern:")
(print "  ✓ Model is source of truth (tree)")
(print "  ✓ View is derived from model")
(print "  ✓ Drag updates model, then view regenerates")
(print "  ✓ Serialization always works (model is consistent)")
(print "")

(print "Next step: Wire this to actual mouse events in designer UI!")
(print "")

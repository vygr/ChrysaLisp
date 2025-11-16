;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Visual Drag-and-Drop for Designer
; Click-to-select, drag-to-move, drop updates model then view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/runtime.inc")
(import "gui_designer/serialize.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drag State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *drag-state* (scatter (Lmap)
	:dragging :nil           ;Currently dragging?
	:selected-element :nil   ;Selected element in tree
	:selected-view :nil      ;Corresponding view widget
	:drag-start-x 0          ;Mouse down position
	:drag-start-y 0
	:drag-current-x 0        ;Current mouse position
	:drag-current-y 0
	:drop-target :nil        ;Element we're hovering over
	:drop-position :nil))    ;:before | :after | :inside

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-select-element (element view)
	; (designer-select-element element view) -> :nil
	; Selects an element for editing/dragging
	(. *drag-state* :insert :selected-element element)
	(. *drag-state* :insert :selected-view view)
	(print "Selected: " (get :type element) " \"" (get :name element) "\"")

	;Visual feedback - could add selection border
	(when view
		;TODO: Add visual selection highlight
		:nil)
	:nil)

(defun designer-deselect ()
	; (designer-deselect) -> :nil
	(. *drag-state* :insert :selected-element :nil)
	(. *drag-state* :insert :selected-view :nil)
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Hit Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-find-element-at (tree x y view)
	; (designer-find-element-at tree x y view) -> element | :nil
	; Finds which element is at screen position x,y
	; This would use view's :hit_tree method
	(when view
		(bind '(hit-view rx ry) (. view :hit_tree x y))
		(when hit-view
			;Find corresponding element in tree
			(designer-find-element-for-view tree hit-view))))

(defun designer-find-element-for-view (tree view)
	; (designer-find-element-for-view tree view) -> element | :nil
	; Finds tree element that corresponds to this view widget
	; This requires tracking view->element mapping during creation
	(defq result :nil)
	(designer-walk-tree tree (lambda (elem)
		;Would check if elem's view matches
		;For now, simplified
		(when (eql (get :name elem) (get :name view))
			(setq result elem))))
	result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drag Detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-mouse-down (tree preview-view x y)
	; (designer-mouse-down tree preview-view x y) -> :nil
	; Handle mouse down - start potential drag or selection

	;Find what was clicked
	(defq element (designer-find-element-at tree x y preview-view))

	(if element
		(progn
			;Select it
			(designer-select-element element :nil)

			;Record drag start position
			(. *drag-state* :insert :drag-start-x x)
			(. *drag-state* :insert :drag-start-y y)
			(. *drag-state* :insert :drag-current-x x)
			(. *drag-state* :insert :drag-current-y y)
			(. *drag-state* :insert :dragging :nil))  ;Not dragging yet

		;Clicked empty space - deselect
		(designer-deselect))
	:nil)

(defun designer-mouse-move (tree preview-view x y)
	; (designer-mouse-move tree preview-view x y) -> :nil
	; Handle mouse move - detect drag start, update drop target

	(when (get :selected-element *drag-state*)
		;Update current position
		(. *drag-state* :insert :drag-current-x x)
		(. *drag-state* :insert :drag-current-y y)

		;Check if we've moved enough to start dragging (threshold: 5 pixels)
		(defq dx (- x (get :drag-start-x *drag-state*))
			dy (- y (get :drag-start-y *drag-state*))
			distance (+ (* dx dx) (* dy dy)))

		(when (and (not (get :dragging *drag-state*)) (> distance 25))
			;Start dragging!
			(. *drag-state* :insert :dragging :t)
			(print "Drag started: " (get :name (get :selected-element *drag-state*))))

		;If dragging, update drop target
		(when (get :dragging *drag-state*)
			(defq target (designer-find-element-at tree x y preview-view))
			(when (and target
					(not (eql target (get :selected-element *drag-state*))))
				;Determine drop position relative to target
				(defq drop-pos (designer-determine-drop-position target x y))
				(. *drag-state* :insert :drop-target target)
				(. *drag-state* :insert :drop-position drop-pos))))
	:nil)

(defun designer-mouse-up (tree)
	; (designer-mouse-up tree) -> modified-tree | :nil
	; Handle mouse up - execute the drag operation

	(defq result :nil)

	(when (get :dragging *drag-state*)
		(defq source (get :selected-element *drag-state*)
			target (get :drop-target *drag-state*)
			position (get :drop-position *drag-state*))

		(when (and source target)
			;Execute the move!
			(print "Drop: " (get :name source) " " position " " (get :name target))
			(setq result (designer-execute-move tree source target position))))

	;Clear drag state
	(. *drag-state* :insert :dragging :nil)
	(. *drag-state* :insert :drop-target :nil)
	(. *drag-state* :insert :drop-position :nil)

	result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Drop Position Determination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-determine-drop-position (target x y)
	; (designer-determine-drop-position target x y) -> :before | :after | :inside
	; Determines where to drop relative to target element

	;Simple heuristic based on target type
	(defq target-type (get :type target))

	(cond
		;Container types - can drop inside
		((or (eql target-type "ui-flow")
			(eql target-type "ui-grid")
			(eql target-type "ui-window")
			(eql target-type "ui-backdrop")
			(eql target-type "ui-scroll"))
			:inside)

		;Non-containers - drop before/after
		;Could use y position to determine which
		(:t :after)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Model Updates (The Key Part!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-execute-move (tree source target position)
	; (designer-execute-move tree source target position) -> modified-tree
	; Updates the tree model, then caller can rebuild view

	(print "Executing move:")
	(print "  Source: " (get :name source))
	(print "  Target: " (get :name target))
	(print "  Position: " position)

	;Find source's parent
	(defq source-parent (designer-find-parent tree source))

	(unless source-parent
		(print "  Error: Source has no parent")
		(return :nil))

	;Remove source from its current parent
	(designer-remove-child source-parent source)
	(print "  Removed from: " (get :name source-parent))

	(case position
		(:inside
			;Add source as child of target
			(designer-add-child target source)
			(print "  Added inside: " (get :name target)))

		(:before
			;Add source before target in target's parent
			(defq target-parent (designer-find-parent tree target))
			(when target-parent
				(designer-insert-child-before target-parent source target)
				(print "  Inserted before: " (get :name target))))

		(:after
			;Add source after target in target's parent
			(defq target-parent (designer-find-parent tree target))
			(when target-parent
				(designer-insert-child-after target-parent source target)
				(print "  Inserted after: " (get :name target)))))

	(print "  Move complete!")
	tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tree Manipulation (insertBefore equivalent!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-insert-child-before (parent new-child before-child)
	; (designer-insert-child-before parent new-child before-child) -> :nil
	; Inserts new-child before before-child in parent's children list

	(defq children (get :children parent))
	(defq new-children (list))
	(defq target-id (get :id before-child))

	;Rebuild children list with insertion
	(each (lambda (child)
		(when (= (get :id child) target-id)
			;Insert before this one
			(push new-children new-child))
		(push new-children child))
		children)

	;Update parent
	(. parent :insert :children new-children)
	:nil)

(defun designer-insert-child-after (parent new-child after-child)
	; (designer-insert-child-after parent new-child after-child) -> :nil
	; Inserts new-child after after-child in parent's children list

	(defq children (get :children parent))
	(defq new-children (list))
	(defq target-id (get :id after-child))

	;Rebuild children list with insertion
	(each (lambda (child)
		(push new-children child)
		(when (= (get :id child) target-id)
			;Insert after this one
			(push new-children new-child)))
		children)

	;Update parent
	(. parent :insert :children new-children)
	:nil)

(defun designer-move-child-up (parent child)
	; (designer-move-child-up parent child) -> :nil
	; Moves child earlier in parent's children list

	(defq children (get :children parent))
	(defq child-id (get :id child))
	(defq index (findfirst (lambda (c) (= (get :id c) child-id)) children))

	(when (and index (> index 0))
		;Swap with previous
		(defq new-children (list))
		(each! 0 -1 1 (lambda (c)
			(cond
				((= (!) (dec index))
					;Insert target here
					(push new-children child))
				((= (!) index)
					;Insert previous here
					(push new-children (elem-get children (dec index))))
				(:t
					;Keep as-is
					(push new-children c))))
			children)
		(. parent :insert :children new-children))
	:nil)

(defun designer-move-child-down (parent child)
	; (designer-move-child-down parent child) -> :nil
	; Moves child later in parent's children list
	(defq children (get :children parent))
	(defq child-id (get :id child))
	(defq index (findfirst (lambda (c) (= (get :id c) child-id)) children))

	(when (and index (< index (dec (length children))))
		;Swap with next
		(defq new-children (list))
		(each! 0 -1 1 (lambda (c)
			(cond
				((= (!) index)
					;Insert next here
					(push new-children (elem-get children (inc index))))
				((= (!) (inc index))
					;Insert target here
					(push new-children child))
				(:t
					;Keep as-is
					(push new-children c))))
			children)
		(. parent :insert :children new-children))
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper: Find first index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findfirst (predicate list)
	; (findfirst predicate list) -> index | :nil
	(defq index 0 found :nil)
	(while (and (< index (length list)) (not found))
		(when (predicate (elem-get list index))
			(setq found index))
		(++ index))
	found)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Demo: Drag and Drop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-drag-drop ()
	; (demo-drag-drop) -> :nil
	(print "")
	(print "╔══════════════════════════════════════════════════════════╗")
	(print "║  Drag-and-Drop Model Update Demo                        ║")
	(print "╚══════════════════════════════════════════════════════════╝")
	(print "")

	;Create a sample tree
	(defq root (scatter (Lmap)
		:id 1
		:type "ui-window"
		:name "*window*"
		:children (list)))

	(defq flow (scatter (Lmap)
		:id 2
		:type "ui-flow"
		:name "main_flow"
		:children (list)))
	(push (get :children root) flow)

	(defq btn1 (scatter (Lmap)
		:id 3
		:type "ui-button"
		:name "btn1"
		:props (list (list (:text "Button 1")))
		:children (list)))
	(push (get :children flow) btn1)

	(defq btn2 (scatter (Lmap)
		:id 4
		:type "ui-button"
		:name "btn2"
		:props (list (list (:text "Button 2")))
		:children (list)))
	(push (get :children flow) btn2)

	(defq btn3 (scatter (Lmap)
		:id 5
		:type "ui-button"
		:name "btn3"
		:props (list (list (:text "Button 3")))
		:children (list)))
	(push (get :children flow) btn3)

	(print "Initial tree:")
	(print (designer-serialize-tree root))
	(print "")

	;Simulate drag: move btn3 before btn1
	(print "═══════════════════════════════════════════════════════════")
	(print "Dragging btn3 to before btn1...")
	(print "═══════════════════════════════════════════════════════════")
	(designer-execute-move root btn3 btn1 :before)
	(print "")

	(print "Tree after move:")
	(print (designer-serialize-tree root))
	(print "")

	;Simulate another drag: move btn2 after btn3
	(print "═══════════════════════════════════════════════════════════")
	(print "Dragging btn2 to after btn3...")
	(print "═══════════════════════════════════════════════════════════")
	(designer-execute-move root btn2 btn3 :after)
	(print "")

	(print "Tree after second move:")
	(print (designer-serialize-tree root))
	(print "")

	(print "Final order: btn3, btn2, btn1 (originally: btn1, btn2, btn3)")
	(print "")
	(print "Model updated successfully! View would regenerate from tree.")
	(print ""))

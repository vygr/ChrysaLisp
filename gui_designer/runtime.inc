;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Designer Runtime
; Provides interactive editing, selection, and manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/lisp.inc")

;selection state
(defq *designer-selection* :nil)
(defq *designer-view-to-element* (scatter (Lmap)))
(defq *designer-element-to-view* (scatter (Lmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; View-Element Mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-register-view (element view)
	; (designer-register-view element view) -> :nil
	(when (and element view)
		(defq element-id (get :id element))
		(. *designer-view-to-element* :insert view element)
		(. *designer-element-to-view* :insert element-id view))
	:nil)

(defun designer-get-element-for-view (view)
	; (designer-get-element-for-view view) -> element | :nil
	(. *designer-view-to-element* :find view))

(defun designer-get-view-for-element (element)
	; (designer-get-view-for-element element) -> view | :nil
	(. *designer-element-to-view* :find (get :id element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-select (element)
	; (designer-select element) -> :nil
	(setq *designer-selection* element)
	:nil)

(defun designer-get-selection ()
	; (designer-get-selection) -> element | :nil
	*designer-selection*)

(defun designer-clear-selection ()
	; (designer-clear-selection) -> :nil
	(setq *designer-selection* :nil)
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Element Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-find-element (tree element-id)
	; (designer-find-element tree element-id) -> element | :nil
	(cond
		((not tree) :nil)
		((= (get :id tree) element-id) tree)
		(:t
			(defq result :nil)
			(some (lambda (child)
				(setq result (designer-find-element child element-id))
				result) (get :children tree))
			result)))

(defun designer-find-parent (tree element)
	; (designer-find-parent tree element) -> parent | :nil
	(cond
		((not tree) :nil)
		(:t
			(defq target-id (get :id element) result :nil)
			(some (lambda (child)
				(if (= (get :id child) target-id)
					(setq result tree)
					(setq result (designer-find-parent child element)))
				result) (get :children tree))
			result)))

(defun designer-add-child (parent child)
	; (designer-add-child parent child) -> :nil
	(when (and parent child)
		(push (get :children parent) child))
	:nil)

(defun designer-remove-child (parent child)
	; (designer-remove-child parent child) -> :nil
	(when (and parent child)
		(defq children (get :children parent)
			target-id (get :id child))
		(setq children (filter (lambda (c)
			(/= (get :id c) target-id)) children))
		(. parent :insert :children children))
	:nil)

(defun designer-set-property (element key value)
	; (designer-set-property element key value) -> :nil
	(when element
		(defq props (first (get :props element)))
		(unless props
			(setq props (list))
			(. element :insert :props (list props)))
		;find existing property
		(defq found :nil idx 0)
		(while (and (not found) (< idx (length props)))
			(defq prop (elem-get props idx))
			(when (and (lst? prop) (>= (length prop) 1)
					(eql (elem-get prop 0) key))
				(elem-set props idx (list key value))
				(setq found :t))
			(++ idx))
		;add new property if not found
		(unless found
			(push props (list key value))))
	:nil)

(defun designer-get-property (element key)
	; (designer-get-property element key) -> value | :nil
	(when element
		(defq props (first (get :props element)))
		(when props
			(some (lambda (prop)
				(when (and (lst? prop) (>= (length prop) 2)
						(eql (elem-get prop 0) key))
					(elem-get prop 1))) props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tree Walking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-walk-tree (tree fn)
	; (designer-walk-tree tree fn) -> :nil
	; Walks the tree and calls fn on each element
	(when tree
		(fn tree)
		(each (lambda (child)
			(designer-walk-tree child fn))
			(get :children tree)))
	:nil)

(defun designer-count-elements (tree)
	; (designer-count-elements tree) -> count
	(defq count 0)
	(designer-walk-tree tree (lambda (_) (++ count)))
	count)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Property Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +common-properties (list
	":text" ":color" ":ink_color" ":font"
	":min_width" ":min_height" ":border"
	":flow_flags" ":grid_width" ":grid_height"))

(defun designer-get-available-properties (element-type)
	; (designer-get-available-properties element-type) -> (prop ...)
	;return common properties for all types
	+common-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-clipboard* :nil)

(defun designer-copy-element (element)
	; (designer-copy-element element) -> element
	;deep copy the element
	(defq copy (scatter (Lmap)
		:id (get :id element)
		:type (get :type element)
		:name (get :name element)
		:constructor (get :constructor element)
		:props (get :props element)
		:children (list)))
	;recursively copy children
	(each (lambda (child)
		(push (get :children copy) (designer-copy-element child)))
		(get :children element))
	copy)

(defun designer-cut ()
	; (designer-cut) -> :nil
	(when *designer-selection*
		(setq *designer-clipboard* (designer-copy-element *designer-selection*))
		;TODO: remove from parent
		)
	:nil)

(defun designer-copy ()
	; (designer-copy) -> :nil
	(when *designer-selection*
		(setq *designer-clipboard* (designer-copy-element *designer-selection*)))
	:nil)

(defun designer-paste (parent)
	; (designer-paste parent) -> :nil
	(when (and *designer-clipboard* parent)
		(defq copy (designer-copy-element *designer-clipboard*))
		(designer-add-child parent copy))
	:nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Undo/Redo (simplified)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-undo-stack* (list))
(defq *designer-redo-stack* (list))

(defun designer-snapshot (tree)
	; (designer-snapshot tree) -> :nil
	;TODO: implement tree snapshot for undo
	:nil)

(defun designer-undo ()
	; (designer-undo) -> tree | :nil
	;TODO: implement undo
	:nil)

(defun designer-redo ()
	; (designer-redo) -> tree | :nil
	;TODO: implement redo
	:nil)

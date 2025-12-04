;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - Simple Tracking (Correct Approach)
; Only redefines ui-root and ui-element - all other macros automatically track
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Global State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-enabled* :t)
(defq *designer-ui-tree* :nil)
(defq *designer-ui-stack* (list))
(defq *designer-ui-counter* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designer-reset ()
	; Reset all designer state
	(setq *designer-ui-tree* :nil)
	(setq *designer-ui-stack* (list))
	(setq *designer-ui-counter* 0))

(defun designer-make-element (type name constructor props body)
	; Create a designer element node
	(defq id (setq *designer-ui-counter* (inc *designer-ui-counter*)))
	(scatter (Lmap)
		:id id
		:type type
		:name name
		:constructor constructor
		:props (list props)
		:body body
		:children (list)))

(defun designer-push-element (elem)
	; Push element onto stack and tree
	(when *designer-enabled*
		(if (empty? *designer-ui-stack*)
			; First element - becomes root
			(setq *designer-ui-tree* elem)
			; Add as child to current parent
			(defq parent (elem-get *designer-ui-stack* -2))
			(when parent
				(defq children (get :children parent))
				(push children elem)
				(. parent :insert :children children)))
		; Push to stack
		(push *designer-ui-stack* elem)))

(defun designer-pop-element ()
	; Pop element from stack
	(when *designer-enabled*
		(pop *designer-ui-stack*)))

(defun designer-get-tree ()
	; Get the captured UI tree
	*designer-ui-tree*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Import original GUI macros first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The standard gui/lisp.inc should already be imported before this file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ONLY ui-root - all window/backdrop macros use this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	; Wrap ui-root to track structure
	; All high-level macros (ui-window, ui-backdrop) call this
	(bind '(p e) (ui-merge-props p))
	(defq props-code (if (= 0 (length p))
		`(progn)
		`(def ,n ~p)))
	(defq undef-code (if (= 0 (length e))
		`(progn)
		`(undef ,n ~e)))
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-root" ',n ',c ',p '(~x)))
		(defq _ui (list (defq ,n ,c)))
		~props-code
		~undef-code
		~x
		(setq _ui :nil)
		(designer-pop-element)
		,n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ONLY ui-element - all other UI widgets use this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	; Wrap ui-element to track structure
	; All high-level macros (ui-flow, ui-button, ui-label, etc.) call this
	(bind '(p e) (ui-merge-props p))
	(defq props-code (if (= 0 (length p))
		`(progn)
		`(def ,n ~p)))
	(defq undef-code (if (= 0 (length e))
		`(progn)
		`(undef ,n ~e)))
	(static-qqp (progn
		(designer-push-element (designer-make-element "ui-element" ',n ',c ',p '(~x)))
		(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
		~props-code
		~undef-code
		~x
		(pop _ui)
		(designer-pop-element))))

(print "Designer tracking macros loaded (simple version)")

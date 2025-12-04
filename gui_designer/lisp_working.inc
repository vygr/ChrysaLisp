;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - Working Tracking Implementation
; Properly inlines original macro code with tracking
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
; Redefine ui-root with tracking (EXACT original code inlined)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	; (ui-root name constructor [props] [body]) -> view
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			; Case 1: No props, no undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-root" ',n ',c :nil :nil))
				(defq _ui (list (defq ,n ,c)))
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			; Case 2: No props, has undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-root" ',n ',c :nil :nil))
				(defq _ui (list (defq ,n ,c)))
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))
		(if (= 0 (length e))
			; Case 3: Has props, no undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-root" ',n ',c ',p :nil))
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			; Case 4: Has props, has undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-root" ',n ',c ',p :nil))
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-element with tracking (EXACT original code inlined)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	; (ui-element name constructor [props] [body]) -> view
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			; Case 1: No props, no undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-element" ',n ',c :nil :nil))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x
				(pop _ui)
				(designer-pop-element)))
			; Case 2: No props, has undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-element" ',n ',c :nil :nil))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))
		(if (= 0 (length e))
			; Case 3: Has props, no undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-element" ',n ',c ',p :nil))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				~x
				(pop _ui)
				(designer-pop-element)))
			; Case 4: Has props, has undef
			(static-qq (progn
				(designer-push-element (designer-make-element "ui-element" ',n ',c ',p :nil))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))))

(print "Designer tracking macros loaded (working version)")

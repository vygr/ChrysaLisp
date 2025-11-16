;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - List-Based Tracking
; Use simple lists instead of Lmap objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-enabled* :t)
(defq *designer-ui-tree* :nil)
(defq *designer-ui-stack* (list))
(defq *designer-ui-counter* 0)

(defun designer-reset ()
	(setq *designer-ui-tree* :nil)
	(setq *designer-ui-stack* (list))
	(setq *designer-ui-counter* 0))

(defun designer-make-element (type name)
	; Create element as simple list: (id type name children)
	(defq id (setq *designer-ui-counter* (inc *designer-ui-counter*)))
	(list id type name (list)))

(defun designer-get-children (elem)
	; Get children list from element
	(elem-get elem 3))

(defun designer-add-child (parent child)
	; Add child to parent's children list
	(when parent
		(defq children (designer-get-children parent))
		(push children child)
		(elem-set parent 3 children)))

(defun designer-push-element (type name)
	(when *designer-enabled*
		(defq elem (designer-make-element type name))
		(if (empty? *designer-ui-stack*)
			; First element - becomes root
			(setq *designer-ui-tree* elem)
			; Add as child to current parent
			(designer-add-child (elem-get *designer-ui-stack* -2) elem))
		; Push to stack
		(push *designer-ui-stack* elem)))

(defun designer-pop-element ()
	(when *designer-enabled*
		(pop *designer-ui-stack*)))

(defun designer-get-tree ()
	*designer-ui-tree*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			(static-qq (progn
				(designer-push-element "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			(static-qq (progn
				(designer-push-element "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x
				(pop _ui)
				(designer-pop-element)))
			(static-qq (progn
				(designer-push-element "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				~x
				(pop _ui)
				(designer-pop-element)))
			(static-qq (progn
				(designer-push-element "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))))

(print "Designer list-based tracking loaded")

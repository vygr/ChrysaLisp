;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - Enhanced List-Based Tracking
; Captures properties and constructor information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-enabled* :t)
(defq *designer-ui-tree* :nil)
(defq *designer-ui-stack* (list))
(defq *designer-ui-counter* 0)

(defun designer-reset ()
	(setq *designer-ui-tree* :nil)
	(setq *designer-ui-stack* (list))
	(setq *designer-ui-counter* 0))

(defun designer-make-element (type name constructor props)
	; Create element as list: (id type name constructor props children)
	(defq id (setq *designer-ui-counter* (inc *designer-ui-counter*)))
	(list id type name constructor props (list)))

(defun designer-get-id (elem) (elem-get elem 0))
(defun designer-get-type (elem) (elem-get elem 1))
(defun designer-get-name (elem) (elem-get elem 2))
(defun designer-get-constructor (elem) (elem-get elem 3))
(defun designer-get-props (elem) (elem-get elem 4))
(defun designer-get-children (elem) (elem-get elem 5))

(defun designer-set-children (elem children)
	(elem-set elem 5 children))

(defun designer-add-child (parent child)
	; Add child to parent's children list
	(when parent
		(defq children (designer-get-children parent))
		(push children child)
		(designer-set-children parent children)))

(defun designer-push-element (type name constructor props)
	(when *designer-enabled*
		(defq elem (designer-make-element type name constructor props))
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
; Redefine ui-root with property capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-root" ',n ',c :nil)
				(defq _ui (list (defq ,n ,c)))
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			(static-qq (progn
				(designer-push-element "ui-root" ',n ',c :nil)
				(defq _ui (list (defq ,n ,c)))
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-root" ',n ',c ',p)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n))
			(static-qq (progn
				(designer-push-element "ui-root" ',n ',c ',p)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-element)
				,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-element with property capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-element" ',n ',c :nil)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x
				(pop _ui)
				(designer-pop-element)))
			(static-qq (progn
				(designer-push-element "ui-element" ',n ',c :nil)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-element "ui-element" ',n ',c ',p)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				~x
				(pop _ui)
				(designer-pop-element)))
			(static-qq (progn
				(designer-push-element "ui-element" ',n ',c ',p)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-element))))))

(print "Designer enhanced list-based tracking loaded")

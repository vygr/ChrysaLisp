;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - Incremental Tracking
; Add features one at a time to find what breaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-enabled* :t)
(defq *designer-ui-stack* (list))
(defq *designer-call-log* (list))

(defun designer-reset ()
	(setq *designer-ui-stack* (list))
	(setq *designer-call-log* (list)))

(defun designer-push-call (type name)
	; Just log the call - no complex object creation
	(when *designer-enabled*
		(push *designer-call-log* (list type name))
		(push *designer-ui-stack* (list type name))))

(defun designer-pop-call ()
	(when *designer-enabled*
		(pop *designer-ui-stack*)))

(defun designer-get-log ()
	*designer-call-log*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-root with simple logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-call "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				~x
				(setq _ui :nil)
				(designer-pop-call)
				,n))
			(static-qq (progn
				(designer-push-call "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-call)
				,n)))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-call "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				~x
				(setq _ui :nil)
				(designer-pop-call)
				,n))
			(static-qq (progn
				(designer-push-call "ui-root" ',n)
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				(designer-pop-call)
				,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-element with simple logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-call "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x
				(pop _ui)
				(designer-pop-call)))
			(static-qq (progn
				(designer-push-call "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-call))))
		(if (= 0 (length e))
			(static-qq (progn
				(designer-push-call "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				~x
				(pop _ui)
				(designer-pop-call)))
			(static-qq (progn
				(designer-push-call "ui-element" ',n)
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(pop _ui)
				(designer-pop-call))))))

(print "Designer incremental tracking loaded")

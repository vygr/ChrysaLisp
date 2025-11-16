;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Designer - Minimal Tracking
; Ultra-simple approach - just count calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *designer-root-count* 0)
(defq *designer-element-count* 0)

(defun designer-reset ()
	(setq *designer-root-count* 0)
	(setq *designer-element-count* 0))

(defun designer-get-counts ()
	(list *designer-root-count* *designer-element-count*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-root - MINIMAL tracking (just increment counter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-root (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(setq *designer-root-count* (inc *designer-root-count*))
				(defq _ui (list (defq ,n ,c)))
				~x
				(setq _ui :nil)
				,n))
			(static-qq (progn
				(setq *designer-root-count* (inc *designer-root-count*))
				(defq _ui (list (defq ,n ,c)))
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				,n)))
		(if (= 0 (length e))
			(static-qq (progn
				(setq *designer-root-count* (inc *designer-root-count*))
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				~x
				(setq _ui :nil)
				,n))
			(static-qq (progn
				(setq *designer-root-count* (inc *designer-root-count*))
				(defq _ui (list (defq ,n ,c)))
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(setq _ui :nil)
				,n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Redefine ui-element - MINIMAL tracking (just increment counter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(redefmacro ui-element (n c &optional p &rest x)
	(bind '(p e) (ui-merge-props p))
	(if (= 0 (length p))
		(if (= 0 (length e))
			(static-qq (progn
				(setq *designer-element-count* (inc *designer-element-count*))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				~x
				(pop _ui)))
			(static-qq (progn
				(setq *designer-element-count* (inc *designer-element-count*))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(undef ,n ~e)
				~x
				(pop _ui))))
		(if (= 0 (length e))
			(static-qq (progn
				(setq *designer-element-count* (inc *designer-element-count*))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				~x
				(pop _ui)))
			(static-qq (progn
				(setq *designer-element-count* (inc *designer-element-count*))
				(. (elem-get (push _ui (defq ,n ,c)) -3) :add_child ,n)
				(def ,n ~p)
				(undef ,n ~e)
				~x
				(pop _ui))))))

(print "Designer minimal tracking loaded")

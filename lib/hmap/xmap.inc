;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defmacro-bind @ (m this &rest _)
	(if (sym? this)
		`((get ,m ,this) ,this ~_)
		`((get ,m (defq ,(defq _o (gensym)) ,this)) ,_o ~_)))

(defmacro-bind method (n p &rest _)
	`(def this ,n (lambda (this ~p) ~_)))

(defmacro-bind method-bind (n p &rest _)
	`(def this ,n (lambda (this ~p) ~(prebind (macroexpand _)))))

(defmacro-bind xmap-slot ()
	`(defq x (get :buckets this)
		b (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem b x))))

(defun xmap (&optional num_buckets cmp_fnc hash_fnc)
	;(xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	;(@ :insert xmap key val)
	;(@ :find xmap key) -> nil|val
	;(@ :erase xmap key)
	;(@ :each xmap lambda)
	(def (defq this (env -1)) :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list) (list)))
	(method-bind :insert (key val)
		(xmap-slot)
		(cond
			(e (elem-set e (elem (inc b) x) val))
			(t (push (elem b x) key) (push (elem (inc b) x) val))))
	(method-bind :find (key)
		(xmap-slot)
		(if e (elem e (elem (inc b) x))))
	(method-bind :erase (key)
		(xmap-slot)
		(when e
			(defq bv (elem (inc b) x) b (elem b x))
			(elem-set e b (elem -2 b))
			(elem-set e bv (elem -2 bv))
			(pop b) (pop bv)))
	(method-bind :each (_hf)
		(defq _i -2 _b (get :buckets this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _hf (elem _i _b) (elem (inc _i) _b))) this)
	this)

(undef (env) 'xmap-slot)

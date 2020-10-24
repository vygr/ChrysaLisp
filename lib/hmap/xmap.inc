;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defmacro-bind @ (m o &rest _)
	(if (sym? o)
		`((get ,m ,o) ,o ~_)
		`((get ,m (defq ,(defq _o (gensym)) ,o)) ,_o ~_)))

(defmacro-bind xmap-slot ()
	`(defq x (get :buckets obj)
		b (* (% ((get :hash_fnc obj) key) (get :num_buckets obj)) 2)
		e (some (# (if ((get :cmp_fnc obj) %0 key) _)) (elem b x))))

(defun-bind xmap (&optional num_buckets cmp_fnc hash_fnc)
	;(xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	;(@ :insert xmap key val)
	;(@ :find xmap key) -> nil|val
	;(@ :erase xmap key)
	;(@ :each xmap lambda)
	(def (defq obj (env -1)) :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets obj) (list) (list)))
	(def obj :insert (lambda (obj key val)
		(xmap-slot)
		(cond
			(e (elem-set e (elem (inc b) x) val))
			(t (push (elem b x) key) (push (elem (inc b) x) val)))))
	(def obj :find (lambda (obj key)
		(xmap-slot)
		(if e (elem e (elem (inc b) x)))))
	(def obj :erase (lambda (obj key)
		(xmap-slot)
		(when e
			(defq bv (elem (inc b) x) b (elem b x))
			(elem-set e b (elem -2 b))
			(elem-set e bv (elem -2 bv))
			(pop b) (pop bv))))
	(def obj :each (lambda (obj _hf)
		(defq _i -2 _b (get :buckets obj))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _hf (elem _i _b) (elem (inc _i) _b))) obj))
	obj)

(undef (env) 'xmap-slot)

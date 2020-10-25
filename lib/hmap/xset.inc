;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash set object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defmacro-bind xset-slot ()
	`(defq x (get :buckets this)
		b (% ((get :hash_fnc this) key) (get :num_buckets this))
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem b x))))

(class xset (&optional num_buckets cmp_fnc hash_fnc)
	;(xset [num_buckets cmp_fnc hash_fnc]) -> xset
	;(@ :find xset key) -> nil|key
	;(@ :insert xset key)
	;(@ :erase xset key)
	;(@ :each xset lambda)
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash) :xset t)
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list)))
	(method-bind :find (key)
		(xset-slot)
		(if e (elem e (elem b x))))
	(method-bind :insert (key)
		(xset-slot)
		(cond
			(e (elem-set e (elem b x) key))
			(t (push (elem b x) key))))
	(method-bind :erase (key)
		(xset-slot)
		(when e
			(elem-set e (defq b (elem b x)) (elem -2 b))
			(pop b)))
	(method-bind :each (_hf)
		(defq _i -1 _b (get :buckets this))
		(while (< (setq _i (inc _i)) (length _b))
			(each _hf (elem _i _b))) this))

(defun-bind xset-k (&rest _)
	;(xset-kv [key ...]) -> xset
	(defq this (xset))
	(each! 0 -1 (# (@ :insert this %0)) (unzip _ (list (list))))
	this)

(defun-bind xset? (this)
	;(xset? xset) -> nil|t
	(and (env? this) (get :xset this)))

(undef (env) 'xset-slot)

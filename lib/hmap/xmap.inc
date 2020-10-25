;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")

(defmacro-bind xmap-slot ()
	`(defq x (get :buckets this)
		b (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem b x))))

(class xmap (&optional num_buckets cmp_fnc hash_fnc)
	;(xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash) :xmap t)
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list) (list)))
	(method-bind :find (this key)
		;(@ xmap :find key) -> nil|val
		(xmap-slot)
		(if e (elem e (elem (inc b) x))))
	(method-bind :insert (this key val)
		;(@ xmap :insert key val)
		(xmap-slot)
		(cond
			(e (elem-set e (elem (inc b) x) val))
			(t (push (elem b x) key) (push (elem (inc b) x) val))))
	(method-bind :erase (this key)
		;(@ xmap :erase key)
		(xmap-slot)
		(when e
			(defq bv (elem (inc b) x) b (elem b x))
			(elem-set e b (elem -2 b))
			(elem-set e bv (elem -2 bv))
			(pop b) (pop bv)))
	(method-bind :each (_this _hf)
		;(@ xmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _hf (elem _i _b) (elem (inc _i) _b))) _this)
	(method-bind :clear (this)
		;(@ xmap :clear) -> xmap
		(each (# (clear %0)) (get :buckets this))
		this)
	(method-bind :copy (this &optional num_buckets)
		;(@ xmap :copy [num_buckets]) -> xmap
		(defq that (xmap (opt num_buckets (get :num_buckets this))
			(get :cmp_fnc this) (get :hash_fnc this)))
		(@ this :each (# (@ that :insert %0 %1)))
		that)
	(method-bind :deep_copy (this &optional num_buckets)
		;(@ xmap :deep_copy [num_buckets]) -> xmap
		(defq that (xmap (opt num_buckets (get :num_buckets this))
			(get :cmp_fnc this) (get :hash_fnc this)))
		(@ this :each (# (@ that :insert (copy %0) (copy %1))))
		that)
	)

(defun-bind xmap-kv (&rest _)
	;(xmap-kv [key val ...]) -> xmap
	(defq this (xmap))
	(each! 0 -1 (# (@ this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)

(defun-bind xmap? (this)
	;(xmap? xmap) -> nil|t
	(and (env? this) (get :xmap this)))

(undef (env) 'xmap-slot)

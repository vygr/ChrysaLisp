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
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash) :xset t)
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list)))
	(method-bind :find (this key)
		;(. xset :find key) -> nil|key
		(xset-slot)
		(if e (elem e (elem b x))))
	(method-bind :insert (this key)
		;(. xset :insert key)
		(xset-slot)
		(cond
			(e (elem-set e (elem b x) key))
			(t (push (elem b x) key))))
	(method-bind :erase (this key)
		;(. xset :erase key)
		(xset-slot)
		(when e
			(elem-set e (defq b (elem b x)) (elem -2 b))
			(pop b)))
	(method-bind :each (_this _hf)
		;(. xset :each lambda)
		(each (lambda (_) (each _hf _)) (get :buckets _this)))
	(method-bind :clear (this)
		;(. xset :clear) -> xset
		(each (# (clear %0)) (get :buckets this))
		this)
	(method-bind :copy (this &optional num_buckets)
		;(. xset :copy [num_buckets]) -> xset
		(defq that (xset (opt num_buckets (get :num_buckets this))
			(get :cmp_fnc this) (get :hash_fnc this)))
		(. this :each (# (. that :insert %0)))
		that)
	(method-bind :deep_copy (this &optional num_buckets)
		;(. xset :deep_copy [num_buckets]) -> xset
		(defq that (xset (opt num_buckets (get :num_buckets this))
			(get :cmp_fnc this) (get :hash_fnc this)))
		(. this :each (# (. that :insert (copy %0))))
		that)
	(method-bind :union (this that)
		;(. xset :union xset) -> xset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)
	(method-bind :difference (this that)
		;(. xset :difference xset) -> xset
		(cond
			((eql this that)
				(. this :clear))
			(t	(. that :each (# (. this :erase %0)))
				this)))
	(method-bind :intersect (this that)
		;(. xset :intersect xset) -> xset
		(unless (eql this that)
			(each (# (elem-set _ b (filter (# (. that :find %0)) %0)))
				(defq b (get :buckets this))))
		this)
	(method-bind :not_intersect (this that)
		;(. xset :not_intersect xset) -> xset
		(cond
			((eql this that)
				(. this :clear))
			(t	(. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))
	)

(defun-bind xset-k (&rest _)
	;(xset-k [key ...]) -> xset
	(defq this (xset))
	(each (# (. this :insert %0)) _)
	this)

(defun-bind xset? (this)
	;(xset? xset) -> nil|t
	(and (env? this) (get :xset this)))

(undef (env) 'xset-slot)

;tests
(when nil
	(defq a (xset-k :a :b :c :z) b (xset-k :a :x :y :z))
	(defq a (. a :copy 2) b (. b :copy 7))
	(print "originals")
	(. a :each prin)
	(print)
	(. b :each prin)
	(print)
	(print "copy")
	(defq x (. a :copy))
	(. x :each prin)
	(print)
	(print "union")
	(defq x (. a :copy))
	(. x :union b)
	(. x :each prin)
	(print)
	(print "difference 1")
	(defq x (. a :copy))
	(. x :difference b)
	(. x :each prin)
	(print)
	(print "difference 2")
	(defq x (. b :copy))
	(. x :difference a)
	(. x :each prin)
	(print)
	(print "intersect")
	(defq x (. a :copy))
	(. x :intersect b)
	(. x :each prin)
	(print)
	(print "not_intersect")
	(defq x (. a :copy))
	(. x :not_intersect b)
	(. x :each prin)
	(print)
	(print "originals")
	(. a :each prin)
	(print)
	(. b :each prin)
	(print))

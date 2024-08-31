;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		e (some (# (if ((get :cmp_fnc this) %0 key) (!))) (elem-get x b))))

(defclass Xmap (&optional num_buckets cmp_fnc hash_fnc) (Map)
	; (Xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists (* num_buckets 2))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))

	(defmethod :find (key)
		; (. xmap :find key) -> :nil | val
		(if (slot) (elem-get (elem-get x (inc b)) e)))

	(defmethod :insert (key val)
		; (. xmap :insert key val) -> xmap
		(cond
			((slot) (elem-set (elem-get x (inc b)) e val))
			(:t (push (elem-get x b) key) (push (elem-get x (inc b)) val)))
		this)

	(defmethod :erase (key)
		; (. xmap :erase key) -> xmap
		(when (slot)
			(defq bv (elem-get x (inc b)) b (elem-get x b))
			(elem-set b e (last b))
			(elem-set bv e (last bv))
			(pop b) (pop bv))
		this)

	(defmethod_ :each (_fnc)
		; (. xmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem-get _b _i) (elem-get _b (inc _i)))) _this)

	(defmethod :copy ()
		; (. xmap :copy) -> xmap
		(defq that ((get 'Xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket key_val)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy ()
		; (. xmap :deep_copy) -> xmap
		(defq that ((get 'Xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket (copy key_val))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :empty ()
		; (. xmap :empty) -> xmap
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move ()
		; (. xmap :move) -> xmap
		(defq that ((get 'Xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (num_buckets)
		; (. xmap :resize num_buckets) -> xmap
		(raise :hash_fnc :buckets (new_buckets (lists (* num_buckets 2))))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets (i -2))
		(while (< (setq i (+ i 2)) (length buckets))
			(defq old_keys (elem-get buckets i) old_vals (elem-get buckets (inc i)))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash_fnc key) num_buckets) 2))
				(push (elem-get new_buckets ni) key)
				(push (elem-get new_buckets (inc ni)) val)))
		this)

	(defmethod :empty? ()
		; (. xmap :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Xmap))
(env-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem-get b x))))

(defclass Xmap (&optional num_buckets cmp_fnc hash_fnc) :nil
	; (Xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists (* num_buckets 2))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))

	(defmethod :find (key)
		; (. xmap :find key) -> :nil | val
		(slot)
		(if e (elem-get e (elem-get (inc b) x))))

	(defmethod :insert (key val)
		; (. xmap :insert key val) -> xmap
		(slot)
		(cond
			(e (elem-set e (elem-get (inc b) x) val))
			(:t (push (elem-get b x) key) (push (elem-get (inc b) x) val)))
		this)

	(defmethod :erase (key)
		; (. xmap :erase key) -> xmap
		(slot)
		(when e
			(defq bv (elem-get (inc b) x) b (elem-get b x))
			(elem-set e b (last b))
			(elem-set e bv (last bv))
			(pop b) (pop bv))
		this)

	(defmethod_ :each (_fnc)
		; (. xmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem-get _i _b) (elem-get (inc _i) _b))) _this)

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
			(defq old_keys (elem-get i buckets) old_vals (elem-get (inc i) buckets))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash_fnc key) num_buckets) 2))
				(push (elem-get ni new_buckets) key)
				(push (elem-get (inc ni) new_buckets) val)))
		this)

	(defmethod :empty? ()
		; (. xmap :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Xmap))
(env-pop)

(defun Xmap-kv (&rest _)
	; (Xmap-kv [key val ...]) -> xmap
	(reduce (lambda (m (k v)) (. m :insert k v)) (partition _ 2) (Xmap)))

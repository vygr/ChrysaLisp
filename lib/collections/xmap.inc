;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem b x))))

(defclass xmap (&optional num_buckets cmp_fnc hash_fnc) nil
	; (xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list) (list)))

	(defmethod :find (this key)
		; (. xmap :find key) -> nil | val
		(slot)
		(if e (elem e (elem (inc b) x))))

	(defmethod :insert (this key val)
		; (. xmap :insert key val) -> xmap
		(slot)
		(cond
			(e (elem-set e (elem (inc b) x) val))
			(t (push (elem b x) key) (push (elem (inc b) x) val)))
		this)

	(defmethod :erase (this key)
		; (. xmap :erase key) -> xmap
		(slot)
		(when e
			(defq bv (elem (inc b) x) b (elem b x))
			(elem-set e b (elem -2 b))
			(elem-set e bv (elem -2 bv))
			(pop b) (pop bv))
		this)

	(defmethod :each (_this _fnc)
		; (. xmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem _i _b) (elem (inc _i) _b))) _this)

	(defmethod :copy (this)
		; (. xmap :copy) -> xmap
		(defq that ((get 'xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket key_val)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy (this)
		; (. xmap :deep_copy) -> xmap
		(defq that ((get 'xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket (copy key_val))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :empty (this)
		; (. xmap :empty) -> xmap
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move (this)
		; (. xmap :move) -> xmap
		(defq that ((get 'xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (this num_buckets)
		; (. xmap :resize num_buckets) -> xmap
		(raise :hash_fnc :buckets (new_buckets (list)))
		(lower :num_buckets (:buckets new_buckets))
		(while (>= (setq num_buckets (dec num_buckets)) 0)
			(push new_buckets (list) (list)))
		(raise :num_buckets (i -2))
		(while (< (setq i (+ i 2)) (length buckets))
			(defq old_keys (elem i buckets) old_vals (elem (inc i) buckets))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash_fnc key) num_buckets) 2))
				(push (elem ni new_buckets) key)
				(push (elem (inc ni) new_buckets) val)))
		this)
	)

;module
(export-classes
	xmap)
(env-pop)

(defun xmap-kv (&rest _)
	; (xmap-kv [key val ...]) -> xmap
	(defq this (xmap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)

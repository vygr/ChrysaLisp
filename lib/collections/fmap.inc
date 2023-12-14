;;;;;;;;;;;;;;;;;
; fast map object
;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (* (% (hash key) (get :num_buckets this)) 2)
		e (find 0 key (elem-get b x))))

(defclass Fmap (&optional num_buckets) :nil
	; (Fmap [num_buckets]) -> fmap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists (* num_buckets 2)))

	(defmethod :find (key)
		; (. fmap :find key) -> :nil | val
		(slot)
		(if e (elem-get e (elem-get (inc b) x))))

	(defmethod :insert (key val)
		; (. fmap :insert key val) -> fmap
		(slot)
		(cond
			(e (elem-set e (elem-get (inc b) x) val))
			(:t (push (elem-get b x) key) (push (elem-get (inc b) x) val)))
		this)

	(defmethod :erase (key)
		; (. fmap :erase key) -> fmap
		(slot)
		(when e
			(defq bv (elem-get (inc b) x) b (elem-get b x))
			(elem-set e b (last b))
			(elem-set e bv (last bv))
			(pop b) (pop bv))
		this)

	(defmethod_ :each (_fnc)
		; (. fmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem-get _i _b) (elem-get (inc _i) _b))) _this)

	(defmethod :copy ()
		; (. fmap :copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket key_val)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy ()
		; (. fmap :deep_copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket (copy key_val))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :empty ()
		; (. fmap :empty) -> fmap
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move ()
		; (. fmap :move) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (num_buckets)
		; (. fmap :resize num_buckets) -> fmap
		(raise :buckets (new_buckets (lists (* num_buckets 2))))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets (i -2))
		(while (< (setq i (+ i 2)) (length buckets))
			(defq old_keys (elem-get i buckets) old_vals (elem-get (inc i) buckets))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash key) num_buckets) 2))
				(push (elem-get ni new_buckets) key)
				(push (elem-get (inc ni) new_buckets) val)))
		this)

	(defmethod :empty? ()
		; (. fmap :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Fmap))
(env-pop)

(defun Fmap-kv (&rest _)
	; (Fmap-kv [key val ...]) -> fmap
	(reduce (lambda (m (k v)) (. m :insert k v)) (partition 2 _) (Fmap)))

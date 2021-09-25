;;;;;;;;;;;;;;;;;
; fast map object
;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (* (% (hash key) (get :num_buckets this)) 2)
		e (find key (elem b x))))

(defclass fmap (&optional num_buckets) nil
	; (fmap [num_buckets]) -> fmap
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1)))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list) (list)))

	(defmethod :find (this key)
		; (. fmap :find key) -> nil | val
		(slot)
		(if e (elem e (elem (inc b) x))))

	(defmethod :insert (this key val)
		; (. fmap :insert key val) -> fmap
		(slot)
		(cond
			(e (elem-set e (elem (inc b) x) val))
			(t (push (elem b x) key) (push (elem (inc b) x) val)))
		this)

	(defmethod :erase (this key)
		; (. fmap :erase key) -> fmap
		(slot)
		(when e
			(defq bv (elem (inc b) x) b (elem b x))
			(elem-set e b (elem -2 b))
			(elem-set e bv (elem -2 bv))
			(pop b) (pop bv))
		this)

	(defmethod :each (_this _fnc)
		; (. fmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem _i _b) (elem (inc _i) _b))) _this)

	(defmethod :copy (this)
		; (. fmap :copy) -> fmap
		(defq that ((get 'fmap) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket key_val)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy (this)
		; (. fmap :deep_copy) -> fmap
		(defq that ((get 'fmap) (get :num_buckets this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key_val)
				(push that_bucket (copy key_val))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :empty (this)
		; (. fmap :empty) -> fmap
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move (this)
		; (. fmap :move) -> fmap
		(defq that ((get 'fmap) (get :num_buckets this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (this num_buckets)
		; (. fmap :resize num_buckets) -> fmap
		(raise :buckets (new_buckets (list)))
		(lower :num_buckets (:buckets new_buckets))
		(while (>= (setq num_buckets (dec num_buckets)) 0)
			(push new_buckets (list) (list)))
		(raise :num_buckets (i -2))
		(while (< (setq i (+ i 2)) (length buckets))
			(defq old_keys (elem i buckets) old_vals (elem (inc i) buckets))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash key) num_buckets) 2))
				(push (elem ni new_buckets) key)
				(push (elem (inc ni) new_buckets) val)))
		this)
	)

;module
(export-classes
	fmap)
(env-pop)

(defun fmap-kv (&rest _)
	; (fmap-kv [key val ...]) -> fmap
	(defq this (fmap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)

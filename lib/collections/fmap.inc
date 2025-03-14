;;;;;;;;;;;;;;;;;
; fast map object
;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defmacro slot ()
	`(defq _x (get :buckets this)
		_b (* (% (hash key) (get :num_buckets this)) 2)
		_i (find key (elem-get _x _b))))

(defclass Fmap (&optional num_buckets) (Map)
	; (Fmap [num_buckets]) -> fmap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists (* num_buckets 2)))

	(defmethod :find (key)
		; (. fmap :find key) -> :nil | val
		(if (slot) (elem-get (elem-get _x (inc _b)) _i)))

	(defmethod :insert (key val)
		; (. fmap :insert key val) -> fmap
		(cond
			((slot) (elem-set (elem-get _x (inc _b)) _i val))
			((push (elem-get _x _b) key) (push (elem-get _x (inc _b)) val)))
		this)

	(defmethod :update (key f)
		; (. fmap :update key lambda) -> fmap
		(cond
			((slot) (elem-set (setq _x (elem-get _x (inc _b))) _i (f (elem-get _x _i))))
			((push (elem-get _x _b) key) (push (elem-get _x (inc _b)) (f :nil))))
		this)

	(defmethod :erase (key)
		; (. fmap :erase key) -> fmap
		(when (slot)
			(defq bv (elem-get _x (inc _b)) _b (elem-get _x _b))
			(elem-set _b _i (last _b))
			(elem-set bv _i (last bv))
			(pop _b) (pop bv))
		this)

	(defmethod_ :each (_fnc)
		; (. fmap :each lambda)
		(defq _i -2 _b (get :buckets _this))
		(while (< (setq _i (+ _i 2)) (length _b))
			(each _fnc (elem-get _b _i) (elem-get _b (inc _i)))) _this)

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
			(defq old_keys (elem-get buckets i) old_vals (elem-get buckets (inc i)))
			(while (defq key (pop old_keys) val (pop old_vals))
				(defq ni (* (% (hash key) num_buckets) 2))
				(push (elem-get new_buckets ni) key)
				(push (elem-get new_buckets (inc ni)) val)))
		this)

	(defmethod :empty? ()
		; (. fmap :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Fmap))
(env-pop)

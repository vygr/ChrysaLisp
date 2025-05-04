;;;;;;;;;;;;;;;;;
; fast map object
;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defmacro slot ()
	`(defq _vl (get :buckets this)
		_si (* (% (hash key) (get :num_buckets this)) 2)
		_kl (elem-get _vl _si)
		_vl (elem-get _vl (inc _si))
		_si (find key _kl)))

(defclass Fmap (&optional num_buckets) (Map)
	; (Fmap [num_buckets]) -> fmap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (lists (* num_buckets 2)))

	(defmethod :find (key)
		; (. fmap :find key) -> :nil | val
		(if (slot) (elem-get _vl _si)))

	(defmethod :insert (key val)
		; (. fmap :insert key val) -> fmap
		(cond
			((slot) (elem-set _vl _si val))
			((push _kl key) (push _vl val)))
		this)

	(defmethod :update (key _fnc)
		; (. fmap :update key lambda) -> val
		(unless (slot)
			(setq _si (length _kl))
			(push _kl key)
			(push _vl :nil))
		(elem-get (elem-set _vl _si (_fnc (elem-get _vl _si))) _si))

	(defmethod :defer (key _fnc)
		; (. fmap :defer key lambda) -> val
		(cond
			((slot) (elem-get _vl _si))
			((push _vl (_fnc)) (push _kl key) (last _vl))))

	(defmethod :erase (key)
		; (. fmap :erase key) -> fmap
		(when (slot)
			(elem-set _kl _si (last _kl))
			(elem-set _vl _si (last _vl))
			(pop _kl) (pop _vl))
		this)

	(defmethod_ :each (_fnc)
		; (. fmap :each lambda)
		(defq _si -2 _b (get :buckets _this))
		(while (< (setq _si (+ _si 2)) (length _b))
			(each _fnc (elem-get _b _si) (elem-get _b (inc _si)))) _this)

	(defmethod :copy ()
		; (. fmap :copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (# (elem-set that_buckets (!) (cat %0))) (get :buckets this))
		that)

	(defmethod :deep_copy ()
		; (. fmap :deep_copy) -> fmap
		(defq that ((get 'Fmap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (# (elem-set that_buckets (!) (copy %0))) (get :buckets this))
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

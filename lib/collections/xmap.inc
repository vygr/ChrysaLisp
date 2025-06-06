;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash map object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defmacro slot ()
	`(defq _vl (get :buckets this)
		_si (* (% ((get :hash_fnc this) key) (get :num_buckets this)) 2)
		_kl (elem-get _vl _si)
		_vl (elem-get _vl (inc _si))
		_si (some (# (if ((get :cmp_fnc this) %0 key) (!))) _kl)))

(defclass Xmap (&optional num_buckets cmp_fnc hash_fnc) (Map)
	; (Xmap [num_buckets cmp_fnc hash_fnc]) -> xmap
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (lists (* num_buckets 2))
		:cmp_fnc (ifn cmp_fnc eql) :hash_fnc (ifn hash_fnc hash))

	(defmethod :find (key)
		; (. xmap :find key) -> :nil | val
		(if (slot) (elem-get _vl _si)))

	(defmethod :insert (key val)
		; (. xmap :insert key val) -> xmap
		(cond
			((slot) (elem-set _vl _si val))
			((push _kl key) (push _vl val)))
		this)

	(defmethod :update (key _fnc)
		; (. xmap :insert key lambda) -> val
		(cond
			((slot)
				(until (setq _kl (elem-get _vl _si))
					((const (ffi "sys/task/lisp_sleep")) 0))
				(elem-set _vl _si :nil)
				(elem-set _vl _si (setq _kl (_fnc _kl))))
			((setq _si (length _kl))
				(push _kl key) (push _vl :nil)
				(elem-set _vl _si (setq _kl (_fnc :nil))))) _kl)

	(defmethod :memoize (key _fnc)
		; (. fmap :memoize key lambda) -> val
		(cond
			((slot)
				(until (setq _kl (elem-get _vl _si))
					((const (ffi "sys/task/lisp_sleep")) 0)))
			((setq _si (length _kl))
				(push _kl key) (push _vl :nil)
				(elem-set _vl _si (setq _kl (_fnc))))) _kl)

	(defmethod :erase (key)
		; (. xmap :erase key) -> xmap
		(when (slot)
			(elem-set _kl _si (last _kl))
			(elem-set _vl _si (last _vl))
			(pop _kl) (pop _vl))
		this)

	(defmethod_ :each (_fnc)
		; (. xmap :each lambda)
		(defq _si -2 _b (get :buckets _this))
		(while (< (setq _si (+ _si 2)) (length _b))
			(each _fnc (elem-get _b _si) (elem-get _b (inc _si)))) _this)

	(defmethod :copy ()
		; (. xmap :copy) -> xmap
		(defq that ((get 'Xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			that_buckets (get :buckets that))
		(each (# (elem-set that_buckets (!) (cat %0))) (get :buckets this))
		that)

	(defmethod :deep_copy ()
		; (. xmap :deep_copy) -> xmap
		(defq that ((get 'Xmap) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			that_buckets (get :buckets that))
		(each (# (elem-set that_buckets (!) (copy %0))) (get :buckets this))
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

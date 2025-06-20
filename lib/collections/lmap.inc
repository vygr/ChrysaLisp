;;;;;;;;;;;;;;;;;;;
; linear map object
;;;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defclass Lmap () (Map)
	; (Lmap) -> lmap
	(def this :keys (list) :vals (list))

	(defmethod :find (key)
		; (. lmap :find key) -> :nil | val
		(if (defq _si (find key (get :keys this))) (elem-get (get :vals this) _si)))

	(defmethod :insert (key val)
		; (. lmap :insert key val) -> lmap
		(raise :keys :vals)
		(cond
			((defq _si (find key keys)) (elem-set vals _si val))
			((push keys key) (push vals val)))
		this)

	(defmethod :update (key _fnc)
		; (. lmap :update key lambda) -> val
		(raise :keys :vals)
		(cond
			((defq _si (find key keys))
				(until (defq _v (elem-get vals _si))
					((const (ffi "sys/task/lisp_sleep")) 0))
				(elem-set vals _si :nil)
				(elem-set vals _si (setq _v (_fnc _v))))
			((setq _si (length keys))
				(push keys key) (push vals :nil)
				(elem-set vals _si (defq _v (_fnc :nil))))) _v)

	(defmethod :memoize (key _fnc)
		; (. lmap :memoize key lambda) -> val
		(raise :keys :vals)
		(cond
			((defq _si (find key keys))
				(until (defq _v (elem-get vals _si))
					((const (ffi "sys/task/lisp_sleep")) 0)))
			((setq _si (length keys))
				(push keys key) (push vals :nil)
				(elem-set vals _si (defq _v (_fnc))))) _v)

	(defmethod :erase (key)
		; (. lmap :erase key) -> lmap
		(raise :keys :vals)
		(when (defq _si (find key keys))
			(elem-set keys _si (last keys))
			(elem-set vals _si (last vals))
			(pop keys) (pop vals))
		this)

	(defmethod :each (fnc)
		; (. lmap :each lambda) -> lmap
		(defq e (penv))
		(each (lambda (k v) (callback fnc e k v))
			(get :keys this) (get :vals this))
		this)

	(defmethod :copy ()
		; (. lmap :copy) -> lmap
		(defq that ((get 'Lmap)))
		(set that :keys (cat (get :keys this)) :vals (cat (get :vals this)))
		that)

	(defmethod :deep_copy ()
		; (. lmap :deep_copy) -> lmap
		(defq that ((get 'Lmap)))
		(set that :keys (copy (get :keys this)) :vals (copy (get :vals this)))
		that)

	(defmethod :empty ()
		; (. lmap :empty) -> lmap
		(clear (get :keys this) (get :vals this))
		this)

	(defmethod :move ()
		; (. lmap :move) -> lmap
		(raise :keys :vals (that ((get 'Lmap))
			that_keys (get :keys that) that_vals (get :vals that)))
		(lower (:keys that_keys :vals that_vals))
		(set that :keys keys :vals vals)
		that)

	(defmethod :resize (num_buckets)
		; (. lmap :resize num_buckets) -> lmap
		this)

	(defmethod :empty? ()
		; (. lmap :empty?) -> :t | :nil
		(= (length (get :keys this)) 0))
	)

;module
(export-classes '(Lmap))
(env-pop)

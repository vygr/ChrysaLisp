;;;;;;;;;;;;;;;;;;;
; linear map object
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defclass Lmap () :nil
	; (Lmap) -> lmap
	(def this :keys (list) :vals (list))

	(defmethod :find (key)
		; (. lmap :find key) -> :nil | val
		(if (defq e (find key (get :keys this))) (elem-get e (get :vals this))))

	(defmethod :insert (key val)
		; (. lmap :insert key val) -> lmap
		(raise :keys :vals)
		(cond
			((defq e (find key keys)) (elem-set e vals val))
			(:t (push keys key) (push vals val)))
		this)

	(defmethod :erase (key)
		; (. lmap :erase key) -> lmap
		(raise :keys :vals)
		(when (defq e (find key keys))
			(elem-set e keys (last keys))
			(elem-set e vals (last vals))
			(pop keys) (pop vals))
		this)

	(defmethod_ :each (_fnc)
		; (. lmap :each lambda)
		(each _fnc (get :keys _this) (get :vals _this)) _this)

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

(defun Lmap-kv (&rest _)
	; (Lmap-kv [key val ...]) -> lmap
	(reduce (lambda (m (k v)) (. m :insert k v)) (partition _ 2) (Lmap)))

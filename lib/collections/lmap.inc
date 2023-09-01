;;;;;;;;;;;;;;;;;;;
; linear map object
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defclass Lmap () :nil
	; (Lmap) -> lmap
	(def this :keys (list) :vals (list))

	(defmethod :find (this key)
		; (. lmap :find key) -> :nil | val
		(if (defq e (find key (get :keys this))) (elem-get e (get :vals this))))

	(defmethod :insert (this key val)
		; (. lmap :insert key val) -> lmap
		(raise :keys :vals)
		(cond
			((defq e (find key keys)) (elem-set e vals val))
			(:t (push keys key) (push vals val)))
		this)

	(defmethod :erase (this key)
		; (. lmap :erase key) -> lmap
		(raise :keys :vals)
		(when (defq e (find key keys))
			(elem-set e keys (elem-get -2 keys))
			(elem-set e vals (elem-get -2 vals))
			(pop keys) (pop vals))
		this)

	(defmethod :each (_this _fnc)
		; (. lmap :each lambda)
		(each _fnc (get :keys _this) (get :vals _this)) _this)

	(defmethod :copy (this)
		; (. lmap :copy) -> lmap
		(defq that ((get 'Lmap)))
		(set that :keys (cat (get :keys this)) :vals (cat (get :vals this)))
		that)

	(defmethod :deep_copy (this)
		; (. lmap :deep_copy) -> lmap
		(defq that ((get 'Lmap)))
		(set that :keys (copy (get :keys this)) :vals (copy (get :vals this)))
		that)

	(defmethod :empty (this)
		; (. lmap :empty) -> lmap
		(clear (get :keys this) (get :vals this))
		this)

	(defmethod :move (this)
		; (. lmap :move) -> lmap
		(raise :keys :vals (that ((get 'Lmap))
			that_keys (get :keys that) that_vals (get :vals that)))
		(lower (:keys that_keys :vals that_vals))
		(set that :keys keys :vals vals)
		that)

	(defmethod :resize (this num_buckets)
		; (. lmap :resize num_buckets) -> lmap
		this)

	(defmethod :empty? (this)
		; (. lmap :empty?) -> :t | :nil
		(= (length (get :keys this)) 0))
	)

;module
(export-classes
	Lmap)
(env-pop)

(defun Lmap-kv (&rest _)
	; (Lmap-kv [key val ...]) -> lmap
	(defq this (Lmap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (lists2)))
	this)

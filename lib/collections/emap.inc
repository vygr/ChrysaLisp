;;;;;;;;;;;;;;;;;;;;;;;;
; generic env map object
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass emap (&optional num_buckets) :nil
	; (emap [num_buckets]) -> emap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (env num_buckets))

	(defmethod :find (this key)
		; (. emap :find key) -> :nil | val
		(get key (get :buckets this)))

	(defmethod :insert (this key val)
		; (. emap :insert key val) -> emap
		(def (get :buckets this) key val)
		this)

	(defmethod :erase (this key)
		; (. emap :erase key) -> emap
		(undef (get :buckets this) key)
		this)

	(defmethod :each (_this _fnc)
		; (. emap :each lambda)
		(each (lambda ((_k _v)) (_fnc _k _v)) (tolist (get :buckets _this))))

	(defmethod :copy (this)
		; (. emap :copy) -> emap
		(defq that ((get 'emap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets key val)) (tolist (get :buckets this)))
		that)

	(defmethod :deep_copy (this)
		; (. emap :deep_copy) -> emap
		(defq that ((get 'emap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets (copy key) (copy val))) (tolist (get :buckets this)))
		that)

	(defmethod :empty (this)
		; (. emap :empty) -> emap
		(def this :buckets (env (get :num_buckets this)))
		this)

	(defmethod :move (this)
		; (. emap :move) -> emap
		(defq that (. this :copy))
		(. this :empty)
		that)

	(defmethod :resize (this num_buckets)
		; (. emap :resize num_buckets) -> emap
		(env-resize num_buckets (get :buckets this))
		this)
	)

(defun emap-kv (&rest _)
	; (emap-kv [key val ...]) -> emap
	(defq this (emap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)


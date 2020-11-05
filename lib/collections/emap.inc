;;;;;;;;;;;;;;;;;;;;;;;;
; generic env map object
;;;;;;;;;;;;;;;;;;;;;;;;

(class emap (&optional num_buckets)
	; (emap [num_buckets]) -> emap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (env (neg num_buckets)) :emap t)

	(method :find (this key)
		; (. emap :find key) -> nil|val
		(get key (get :buckets this)))

	(method :insert (this key val)
		; (. emap :insert key val) -> emap
		(def (get :buckets this) key val)
		this)

	(method :erase (this key)
		; (. emap :erase key) -> emap
		(undef (get :buckets this) key)
		this)

	(method :each (_this _fnc)
		; (. emap :each lambda)
		(each (lambda ((_k _v)) (_fnc _k _v)) (tolist (get :buckets _this))))

	(method :copy (this)
		; (. emap :copy) -> emap
		(defq that (emap (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets key val)) (tolist (get :buckets this)))
		that)

	(method :deep_copy (this)
		; (. emap :deep_copy) -> emap
		(defq that (emap (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets (copy key) (copy val))) (tolist (get :buckets this)))
		that)

	(method :empty (this)
		; (. emap :empty) -> emap
		(def this :buckets (env (neg (get :num_buckets this))))
		this)

	(method :move (this)
		; (. emap :move) -> emap
		(defq that (. this :copy))
		(. this :empty)
		that)

	(method :resize (this num_buckets)
		; (. emap :resize num_buckets) -> emap
		(defq old_buckets (tolist (get :buckets this)))
		(def this :num_buckets num_buckets :buckets (defq new_buckets (env (neg num_buckets))))
		(each (lambda ((key val)) (def new_buckets key val)) old_buckets)
		this)
	)

(defun emap-kv (&rest _)
	; (emap-kv [key val ...]) -> emap
	(defq this (emap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)

(defun emap? (this)
	; (emap? emap) -> nil|t
	(and (env? this) (get :emap this)))

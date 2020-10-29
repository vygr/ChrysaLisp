;;;;;;;;;;;;;;;;;;;;;;;;
; generic env map object
;;;;;;;;;;;;;;;;;;;;;;;;

(class emap (&optional num_buckets)
	;(emap [num_buckets]) -> emap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (env (neg num_buckets)) :emap t)

	(method-bind :find (this key)
		;(. emap :find key) -> nil|val
		(get key (get :buckets this)))

	(method-bind :insert (this key val)
		;(. emap :insert key val) -> emap
		(def (get :buckets this) key val)
		this)

	(method-bind :erase (this key)
		;(. emap :erase key) -> emap
		(undef (get :buckets this) key)
		this)

	(method-bind :each (_this _fnc)
		;(. emap :each lambda)
		(each (lambda ((_k _v)) (_fnc _k _v)) (tolist (get :buckets _this))))

	(method-bind :copy (this)
		;(. emap :copy) -> emap
		(defq that (emap (neg (get :num_buckets this))))
		(each (lambda ((_k _v)) (. that :insert _k _v)) (tolist (get :buckets this)))
		that)

	(method-bind :deep_copy (this)
		;(. emap :deep_copy) -> emap
		(defq that (emap (neg (get :num_buckets this))))
		(each (lambda ((_k _v)) (. that :insert (copy _k) (copy _v))) (tolist (get :buckets this)))
		that)

	(method-bind :clear (this)
		;(. emap :clear) -> emap
		(def this :buckets (env (neg (get :num_buckets this))))
		this)

	(method-bind :move (this)
		;(. emap :move) -> emap
		(defq that (. this :copy))
		(. this :clear)
		that)

	(method-bind :resize (this num_buckets)
		;(. emap :resize num_buckets) -> emap
		(env (get this :buckets) (def this :num_buckets num_buckets))
		this)
	)

(defun-bind emap-kv (&rest _)
	;(emap-kv [key val ...]) -> emap
	(defq this (emap))
	(each! 0 -1 (# (. this :insert %0 %1)) (unzip _ (list (list) (list))))
	this)

(defun-bind emap? (this)
	;(emap? emap) -> nil|t
	(and (env? this) (get :emap this)))

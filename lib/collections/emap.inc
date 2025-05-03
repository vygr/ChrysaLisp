;;;;;;;;;;;;;;;;;;;;;;;;
; generic env map object
;;;;;;;;;;;;;;;;;;;;;;;;

(import "./map.inc")

(defclass Emap (&optional num_buckets) (Map)
	; (Emap [num_buckets]) -> emap
	(def this :num_buckets (setq num_buckets (opt num_buckets 1))
		:buckets (env num_buckets))

	(defmethod :find (key)
		; (. emap :find key) -> :nil | val
		(get key (get :buckets this)))

	(defmethod :insert (key val)
		; (. emap :insert key val) -> emap
		(def (get :buckets this) key val)
		this)

	(defmethod :update (key _fnc)
		; (. emap :update key lambda) -> val
		(def (defq _b (get :buckets this)) key (_fnc (def? key _b))))

	(defmethod :erase (key)
		; (. emap :erase key) -> emap
		(undef (get :buckets this) key)
		this)

	(defmethod_ :each (_fnc)
		; (. emap :each lambda)
		(each (lambda ((_k _v)) (_fnc _k _v)) (tolist (get :buckets _this))))

	(defmethod :copy ()
		; (. emap :copy) -> emap
		(defq that ((get 'Emap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets key val)) (tolist (get :buckets this)))
		that)

	(defmethod :deep_copy ()
		; (. emap :deep_copy) -> emap
		(defq that ((get 'Emap) (get :num_buckets this)) that_buckets (get :buckets that))
		(each (lambda ((key val)) (def that_buckets (copy key) (copy val))) (tolist (get :buckets this)))
		that)

	(defmethod :empty ()
		; (. emap :empty) -> emap
		(def this :buckets (env (get :num_buckets this)))
		this)

	(defmethod :move ()
		; (. emap :move) -> emap
		(defq that (. this :copy))
		(. this :empty)
		that)

	(defmethod :resize (num_buckets)
		; (. emap :resize num_buckets) -> emap
		(env-resize num_buckets (get :buckets this))
		this)
	)

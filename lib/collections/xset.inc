;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash set object
;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defmacro slot ()
	`(defq x (get :buckets this)
		b (% ((get :hash_fnc this) key) (get :num_buckets this))
		e (some (# (if ((get :cmp_fnc this) %0 key) _)) (elem-get b x))))

(defclass xset (&optional num_buckets cmp_fnc hash_fnc) :nil
	; (xset [num_buckets cmp_fnc hash_fnc]) -> xset
	(def this :buckets (list) :num_buckets (setq num_buckets (opt num_buckets 1))
		:cmp_fnc (opt cmp_fnc eql) :hash_fnc (opt hash_fnc hash))
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push (get :buckets this) (list)))

	(defmethod :find (this key)
		; (. xset :find key) -> :nil | key
		(slot)
		(if e (elem-get e (elem-get b x))))

	(defmethod :insert (this key)
		; (. xset :insert key) -> xset
		(slot)
		(cond
			(e (elem-set e (elem-get b x) key))
			(:t (push (elem-get b x) key)))
		this)

	(defmethod :erase (this key)
		; (. xset :erase key) -> xset
		(slot)
		(when e
			(elem-set e (defq b (elem-get b x)) (elem-get -2 b))
			(pop b))
		this)

	(defmethod :each (_this _fnc)
		; (. xset :each lambda)
		(each (lambda (_) (each _fnc _)) (get :buckets _this)))

	(defmethod :copy (this)
		; (. xset :copy) -> xset
		(defq that ((get 'xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket key)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy (this)
		; (. xset :deep_copy) -> xset
		(defq that ((get 'xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket (copy key))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :union (this that)
		; (. xset :union xset) -> xset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (this that)
		; (. xset :difference xset) -> xset
		(cond
			((eql this that)
				(. this :empty))
			(:t  (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (this that)
		; (. xset :intersect xset) -> xset
		(unless (eql this that)
			(each (# (elem-set _ b (filter (# (. that :find %0)) %0)))
				(defq b (get :buckets this))))
		this)

	(defmethod :not_intersect (this that)
		; (. xset :not_intersect xset) -> xset
		(cond
			((eql this that)
				(. this :empty))
			(:t  (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))

	(defmethod :empty (this)
		; (. xset :empty) -> xset
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move (this)
		; (. xset :move) -> xset
		(defq that ((get 'xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (this num_buckets)
		; (. xset :resize num_buckets) -> xset
		(raise :hash_fnc :buckets (new_buckets (list)))
		(lower :num_buckets (:buckets new_buckets))
		(while (>= (setq num_buckets (dec num_buckets)) 0)
			(push new_buckets (list)))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(while (defq key (pop old_bucket))
				(push (elem-get (% (hash_fnc key) num_buckets) new_buckets) key))) buckets)
		this)

	(defmethod :empty? (this)
		; (. xset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes
	xset)
(env-pop)

(defun xset-k (&rest _)
	; (xset-k [key ...]) -> xset
	(defq this (xset))
	(each (# (. this :insert %0)) _)
	this)

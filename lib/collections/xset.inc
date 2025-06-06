;;;;;;;;;;;;;;;;;;;;;;;;;
; generic hash set object
;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defmacro slot ()
	`(defq _kl (get :buckets this)
		_kl (elem-get _kl (% ((get :hash_fnc this) key) (get :num_buckets this)))
		_si (some (# (if ((get :cmp_fnc this) %0 key) (!))) _kl)))

(defclass Xset (&optional num_buckets cmp_fnc hash_fnc) (Set)
	; (Xset [num_buckets cmp_fnc hash_fnc]) -> xset
	(def this :num_buckets (setq num_buckets (ifn num_buckets 1))
		:buckets (lists num_buckets)
		:cmp_fnc (ifn cmp_fnc eql) :hash_fnc (ifn hash_fnc hash))

	(defmethod :find (key)
		; (. xset :find key) -> :nil | key
		(if (slot) (elem-get _kl _si)))

	(defmethod :insert (key)
		; (. xset :insert key) -> xset
		(if (slot) :nil (push _kl key))
		this)

	(defmethod :inserted (key)
		; (. xset :inserted key) -> :nil | xset
		(cond
			((slot) this)
			((push _kl key) :nil)))

	(defmethod :intern (key)
		; (. xset :intern key) -> key
		(cond
			((slot) (elem-get _kl _si))
			((push _kl key) key)))

	(defmethod :erase (key)
		; (. xset :erase key) -> xset
		(when (slot)
			(elem-set _kl _si (last _kl))
			(pop _kl))
		this)

	(defmethod_ :each (_fnc)
		; (. xset :each lambda)
		(each (lambda (_) (each _fnc _)) (get :buckets _this)))

	(defmethod :copy ()
		; (. xset :copy) -> xset
		(defq that ((get 'Xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket key)) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :deep_copy ()
		; (. xset :deep_copy) -> xset
		(defq that ((get 'Xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this)))
		(each (lambda (this_bucket that_bucket)
			(each (lambda (key)
				(push that_bucket (copy key))) this_bucket)) (get :buckets this) (get :buckets that))
		that)

	(defmethod :union (that)
		; (. xset :union xset) -> xset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
		this)

	(defmethod :difference (that)
		; (. xset :difference xset) -> xset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (that)
		; (. xset :intersect xset) -> xset
		(unless (eql this that)
			(each (# (elem-set _b (!) (filter-array (# (. that :find %0)) %0)))
				(defq _b (get :buckets this))))
		this)

	(defmethod :not_intersect (that)
		; (. xset :not_intersect xset) -> xset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))

	(defmethod :empty ()
		; (. xset :empty) -> xset
		(each (# (clear %0)) (get :buckets this))
		this)

	(defmethod :move ()
		; (. xset :move) -> xset
		(defq that ((get 'Xset) (get :num_buckets this) (get :cmp_fnc this) (get :hash_fnc this))
			this_buckets (get :buckets this) that_buckets (get :buckets that))
		(set this :buckets that_buckets)
		(set that :buckets this_buckets)
		that)

	(defmethod :resize (num_buckets)
		; (. xset :resize num_buckets) -> xset
		(raise :hash_fnc :buckets (new_buckets (lists num_buckets)))
		(lower :num_buckets (:buckets new_buckets))
		(raise :num_buckets)
		(each (lambda (old_bucket)
			(while (defq key (pop old_bucket))
				(push (elem-get new_buckets (% (hash_fnc key) num_buckets)) key))) buckets)
		this)

	(defmethod :empty? ()
		; (. xset :empty?) -> :t | :nil
		(every (# (= (length %0) 0)) (get :buckets this)))
	)

;module
(export-classes '(Xset))
(env-pop)

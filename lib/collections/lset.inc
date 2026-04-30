;;;;;;;;;;;;;;;;;;;
; linear set object
;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defclass Lset () (Set)
	; (Lset) -> lset
	(def this :keys (list))

	(defmethod :find (key)
		; (. lset :find key) -> :nil | key
		(raise :keys)
		(if (defq _si (find key keys)) (elem-get keys _si)))

	(defmethod :insert (key)
		; (. lset :insert key) -> lset
		(merge (get :keys this) (list key))
		this)

	(defmethod :inserted (key)
		; (. lset :inserted key) -> :nil | lset
		(raise :keys)
		(cond
			((find key keys) :nil)
			((push keys key) this)))

	(defmethod :intern (key)
		; (. lset :intern key) -> key
		(raise :keys)
		(cond
			((defq _si (find key keys)) (elem-get keys _si))
			((push keys key) key)))

	(defmethod :erase (key)
		; (. lset :erase key) -> lset
		(raise :keys)
		(when (defq _si (find key keys))
			(elem-set keys _si (last keys))
			(pop keys))
		this)

	(defmethod :each (fnc)
		; (. lset :each lambda) -> lset
		(defq e (penv))
		(each (lambda (k) (callback fnc e k)) (get :keys this))
		this)

	(defmethod :copy ()
		; (. lset :copy) -> lset
		(defq that ((get 'Lset)))
		(set that :keys (cat (get :keys this)))
		that)

	(defmethod :deep_copy ()
		; (. lset :deep_copy) -> lset
		(defq that ((get 'Lset)))
		(set that :keys (copy (get :keys this)))
		that)

	(defmethod :union (that)
		; (. lset :union set) -> lset
		(raise :keys)
		(unless (eql this that)
			(if (defq that_keys (get :keys that))
				(merge keys that_keys)
				(. that :each (# (. this :insert %0)))))
		this)

	(defmethod :difference (that)
		; (. lset :difference set) -> lset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. that :each (# (. this :erase %0)))
				this)))

	(defmethod :intersect (that)
		; (. lset :intersect set) -> lset
		(unless (eql this that)
			(set this :keys (filter (# (. that :find %0)) (get :keys this))))
		this)

	(defmethod :not_intersect (that)
		; (. lset :not_intersect set) -> lset
		(cond
			((eql this that)
				(. this :empty))
			(:t (. (defq other (. that :copy)) :difference this)
				(. this :difference that)
				(. this :union other))))

	(defmethod :empty ()
		; (. lset :empty) -> lset
		(clear (get :keys this))
		this)

	(defmethod :move ()
		; (. lset :move) -> lset
		(raise :keys (that ((get 'Lset)) that_keys (get :keys that)))
		(lower (:keys that_keys))
		(set that :keys keys)
		that)

	(defmethod :resize (num_buckets)
		; (. lset :resize num_buckets) -> lset
		this)

	(defmethod :empty? ()
		; (. lset :empty?) -> :t | :nil
		(= (length (get :keys this)) 0))

	(defmethod :size ()
		; (. lset :size) -> size
		(length (get :keys this)))
	)

;module
(export-classes '(Lset))
(env-pop)

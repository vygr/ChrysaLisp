;;;;;;;;;;;;;;;;;;;
; linear set object
;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defclass Lset () (Set)
	; (Lset) -> lset
	(def this :bucket (pset))

	(defmethod :find (key)
		; (. lset :find key) -> :nil | key
		(pfind (get :bucket this) key))

	(defmethod :insert (key)
		; (. lset :insert key) -> lset
		(pinsert (get :bucket this) key)
		this)

	(defmethod :inserted (key)
		; (. lset :inserted key) -> :nil | lset
		(raise :bucket)
		(cond
			((pfind bucket key) :nil)
			((pinsert bucket key) this)))

	(defmethod :intern (key)
		; (. lset :intern key) -> key
		(raise :bucket)
		(or (pfind bucket key)
			(progn (pinsert bucket key) key)))

	(defmethod :erase (key)
		; (. lset :erase key) -> lset
		(perase (get :bucket this) key)
		this)

	(defmethod :each (fnc)
		; (. lset :each lambda) -> lset
		(defq e (penv))
		(each (lambda (k) (callback fnc e k)) (get :bucket this))
		this)

	(defmethod :tolist ()
		; (. lset :tolist) -> list
		(merge (list) (get :bucket this)))

	(defmethod :copy ()
		; (. lset :copy) -> lset
		(defq that ((get 'Lset)))
		(set that :bucket (copy (get :bucket this)))
		that)

	(defmethod :deep_copy ()
		; (. lset :deep_copy) -> lset
		(defq that ((get 'Lset)) that_bucket (get :bucket that))
		(each (lambda (key) (pinsert that_bucket (copy key))) (get :bucket this))
		that)

	(defmethod :union (that)
		; (. lset :union set) -> lset
		(unless (eql this that)
			(. that :each (# (. this :insert %0))))
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
			(defq new_bucket (pset))
			(each (lambda (k) (if (. that :find k) (pinsert new_bucket k))) (get :bucket this))
			(set this :bucket new_bucket))
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
		(clear (get :bucket this))
		this)

	(defmethod :move ()
		; (. lset :move) -> lset
		(raise :bucket (that ((get 'Lset)) that_bucket (get :bucket that)))
		(lower (:bucket that_bucket))
		(set that :bucket bucket)
		that)

	(defmethod :resize (num_buckets)
		; (. lset :resize num_buckets) -> lset
		this)

	(defmethod :empty? ()
		; (. lset :empty?) -> :t | :nil
		(= (length (get :bucket this)) 0))

	(defmethod :size ()
		; (. lset :size) -> size
		(length (get :bucket this)))
	)

;module
(export-classes '(Lset))
(env-pop)
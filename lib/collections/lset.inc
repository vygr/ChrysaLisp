;;;;;;;;;;;;;;;;;;;
; linear set object
;;;;;;;;;;;;;;;;;;;

(import "./set.inc")

;module
(env-push)

(defclass Lset () (Set)
	; (Lset) -> lset
	(def this :bucket (pmap))

	(defmethod :find (key)
		; (. lset :find key) -> :nil | key
		(if (defq i (pfindi (get :bucket this) key))
			(elem-get (get :bucket this) i)))

	(defmethod :insert (key)
		; (. lset :insert key) -> lset
		(pinsert (get :bucket this) key :t)
		this)

	(defmethod :inserted (key)
		; (. lset :inserted key) -> :nil | lset
		(raise :bucket)
		(cond
			((pfind bucket key) :nil)
			((pinsert bucket key :t) this)))

	(defmethod :intern (key)
		; (. lset :intern key) -> key
		(raise :bucket)
		(if (defq i (pfindi bucket key))
			(elem-get bucket i)
			(progn (pinsert bucket key :t) key)))

	(defmethod :erase (key)
		; (. lset :erase key) -> lset
		(perase (get :bucket this) key)
		this)

	(defmethod :each (fnc)
		; (. lset :each lambda) -> lset
		(defq e (penv))
		(each (lambda ((k v)) (callback fnc e k)) (partition (get :bucket this) 2))
		this)

	(defmethod :tolist ()
		; (. lset :tolist) -> list
		(defq out (list))
		(each (lambda ((k v)) (push out k)) (partition (get :bucket this) 2))
		out)

	(defmethod :copy ()
		; (. lset :copy) -> lset
		(defq that ((get 'Lset)))
		(set that :bucket (copy (get :bucket this)))
		that)

	(defmethod :deep_copy ()
		; (. lset :deep_copy) -> lset
		(defq that ((get 'Lset)) that_bucket (get :bucket that))
		(each (lambda ((key val)) (pinsert that_bucket (copy key) (copy val))) (partition (get :bucket this) 2))
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
			(defq new_bucket (pmap))
			(each (lambda ((k v)) (if (. that :find k) (pinsert new_bucket k v))) (partition (get :bucket this) 2))
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
		(/ (length (get :bucket this)) 2))
	)

;module
(export-classes '(Lset))
(env-pop)

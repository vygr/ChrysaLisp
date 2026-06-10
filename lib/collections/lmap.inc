;;;;;;;;;;;;;;;;;;;
; linear map object
;;;;;;;;;;;;;;;;;;;

(import "./map.inc")

;module
(env-push)

(defclass Lmap () (Map)
	; (Lmap) -> lmap
	(def this :bucket (plist))

	(defmethod :find (key)
		; (. lmap :find key) -> :nil | val
		(pfind (get :bucket this) key))

	(defmethod :insert (key val)
		; (. lmap :insert key val) -> lmap
		(pinsert (get :bucket this) key val)
		this)

	(defmethod :update (key _fnc)
		; (. lmap :update key lambda) -> val
		(defq _p (get :bucket this) _val (_fnc (pfind _p key)))
		(pinsert _p key _val)
		_val)

	(defmethod :memoize (key _fnc)
		; (. lmap :memoize key lambda) -> val
		(defq _p (get :bucket this))
		(if (nql (defq _idx (pfindi _p key)) :nil)
			(elem-get _p (inc _idx))
			(progn
				(defq _val (_fnc))
				(pinsert _p key _val)
				_val)))

	(defmethod :erase (key)
		; (. lmap :erase key) -> lmap
		(raise :bucket)
		(when (defq _si (pfindi bucket key))
			(elem-set bucket _si (elem-get bucket -3))
			(elem-set bucket (inc _si) (elem-get bucket -2))
			(pop bucket) (pop bucket))
		this)

	(defmethod :each (fnc)
		; (. lmap :each lambda) -> lmap
		(defq e (penv))
		(each (lambda ((k v)) (callback fnc e k v)) (. this :tolist))
		this)

	(defmethod :tolist ()
		; (. lmap :tolist) -> list
		(partition (get :bucket this) 2))

	(defmethod :copy ()
		; (. lmap :copy) -> lmap
		(defq that ((get 'Lmap)))
		(set that :bucket (copy (get :bucket this)))
		that)

	(defmethod :deep_copy ()
		; (. lmap :deep_copy) -> lmap
		(defq that ((get 'Lmap)) that_plist (get :bucket that))
		(each (lambda ((key val)) (pinsert that_plist (copy key) (copy val))) (. this :tolist))
		that)

	(defmethod :empty ()
		; (. lmap :empty) -> lmap
		(set this :bucket (plist))
		this)

	(defmethod :move ()
		; (. lmap :move) -> lmap
		(defq that (. this :copy))
		(. this :empty)
		that)

	(defmethod :resize (num_buckets)
		; (. lmap :resize num_buckets) -> lmap
		this)

	(defmethod :empty? ()
		; (. lmap :empty?) -> :t | :nil
		(= (length (get :bucket this)) 0))

	(defmethod :size ()
		; (. lmap :size) -> size
		(/ (length (get :bucket this)) 2))
	)

;module
(export-classes '(Lmap))
(env-pop)
